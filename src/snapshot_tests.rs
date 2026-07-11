//! Reviewable headless snapshots of the native frame model.
//!
//! The core crate owns application behaviour.  These tests intentionally use
//! its public key path only to reach renderer states, then snapshot the
//! plugin's `Frame` rather than duplicating core assertions.

use std::{
    fmt::Write,
    fs,
    path::PathBuf,
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicUsize},
    },
    thread,
    time::{Duration, Instant},
};

use scooter_core::{
    app::{Screen, SearchPhase},
    fields::Field,
    line_reader::LineEnding,
    replace::{PerformingReplacementState, ReplaceResult, ReplaceState},
    search::{SearchResult, SearchResultWithReplacement},
};
use tempfile::{TempDir, tempdir};
use tokio::sync::mpsc;
use unicode_width::UnicodeWidthChar;

use crate::{
    engine::ScooterEngine,
    view::{Frame, Run},
};

const STANDARD_SIZE: (usize, usize) = (120, 40);
const WIDE_SIZE: (usize, usize) = (160, 45);

#[derive(Clone, Default)]
struct Cell {
    character: char,
    tag: Option<String>,
    continuation: bool,
}

/// Paint a frame's ordered runs into a fixed canvas, preserving the semantic
/// tag of every painted cell in a compact legend below it.  The coordinate
/// markers keep blank cells and trailing whitespace reviewable in snapshots.
fn frame_snapshot(frame: Frame, width: usize, height: usize) -> String {
    let mut canvas = vec![vec![Cell::default(); width]; height];
    for run in frame.runs {
        paint_run(&mut canvas, run);
    }

    let mut snapshot = format!("canvas: {width}x{height}\n");
    for (row, cells) in canvas.iter().enumerate() {
        write!(&mut snapshot, "{row:02} |").expect("write to string");
        for cell in cells {
            snapshot.push(if cell.continuation || cell.character == '\0' {
                ' '
            } else {
                cell.character
            });
        }
        snapshot.push_str("|\n");
    }

    snapshot.push_str("tags:\n");
    for (row, cells) in canvas.iter().enumerate() {
        let spans = tag_spans(cells);
        if !spans.is_empty() {
            writeln!(&mut snapshot, "{row:02}: {}", spans.join(" | ")).expect("write to string");
        }
    }
    if let Some((x, y)) = frame.cursor {
        writeln!(&mut snapshot, "cursor: {x},{y}").expect("write to string");
    }
    snapshot
}

fn paint_run(canvas: &mut [Vec<Cell>], Run { x, y, text, tag }: Run) {
    let Some(row) = canvas.get_mut(y) else {
        return;
    };

    let mut column = x;
    for character in stable_snapshot_text(&text).chars() {
        let character = visible_character(character);
        let character_width = UnicodeWidthChar::width(character).unwrap_or(1).max(1);
        if column >= row.len() {
            break;
        }

        row[column] = Cell {
            character,
            tag: Some(tag.as_str().to_string()),
            continuation: false,
        };
        for offset in 1..character_width {
            let Some(cell) = row.get_mut(column + offset) else {
                break;
            };
            *cell = Cell {
                character: '\0',
                tag: Some(tag.as_str().to_string()),
                continuation: true,
            };
        }
        column = column.saturating_add(character_width);
    }
}

/// A popup can cover the middle of a right-aligned search-duration run.  At
/// that point insta's whole-string filters cannot recognise the duration, so
/// replace it before painting while preserving its six-cell display width.
fn stable_snapshot_text(text: &str) -> String {
    let Some((prefix, _duration)) = text.split_once("[Time taken: ") else {
        return text.to_string();
    };
    format!("{prefix}[Time taken: 0.000s]")
}

fn visible_character(character: char) -> char {
    match character {
        '\n' => '↵',
        '\r' => '␍',
        '\t' => '⇥',
        character if character.is_control() => '�',
        character => character,
    }
}

fn tag_spans(cells: &[Cell]) -> Vec<String> {
    let mut spans = Vec::new();
    let mut start = 0;
    while start < cells.len() {
        let Some(tag) = cells[start].tag.as_deref() else {
            start += 1;
            continue;
        };
        let end = cells[start..]
            .iter()
            .take_while(|cell| cell.tag.as_deref() == Some(tag))
            .count()
            + start;
        spans.push(format!("{start}-{end} {tag}"));
        start = end;
    }
    spans
}

fn assert_frame(engine: &mut ScooterEngine, name: &str, size: (usize, usize)) {
    let snapshot = frame_snapshot(engine.render(size.0, size.1), size.0, size.1);
    insta::with_settings!({filters => vec![
        (r"\[Time taken: [^\]]+\]", "[Time taken: TIME]"),
        (r"Time: [0-9]+\.[0-9]{3}s", "Time: TIME"),
        (r"Completed: [0-9]+/[0-9]+ \([0-9.]+%\)", "Completed: PROGRESS"),
        (r"/(?:private/)?var/folders/[A-Za-z0-9._/-]+", "<TEMP>"),
    ]}, {
        insta::assert_snapshot!(name, snapshot);
    });
}

fn engine_with_fixture(contents: &str) -> (TempDir, ScooterEngine) {
    engine_with_named_fixture("fixture.txt", contents)
}

fn engine_with_named_fixture(name: &str, contents: &str) -> (TempDir, ScooterEngine) {
    let fixture = tempdir().expect("fixture directory");
    fs::write(fixture.path().join(name), contents).expect("write fixture");
    let engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
    (fixture, engine)
}

fn press(engine: &mut ScooterEngine, code: &str, modifiers: usize) {
    let _ = engine.handle_key(code, modifiers);
}

fn type_text(engine: &mut ScooterEngine, text: &str) {
    for character in text.chars() {
        press(engine, &character.to_string(), 0);
    }
}

fn complete_search(engine: &mut ScooterEngine, text: &str) {
    type_text(engine, text);
    wait_until_complete(engine);
}

fn search_with_replacement(engine: &mut ScooterEngine, search: &str, replacement: &str) {
    complete_search(engine, search);
    press(engine, "tab", 0);
    type_text(engine, replacement);
    wait_until_preview_updated(engine);
    press(engine, "enter", 0);
}

fn wait_until_complete(engine: &mut ScooterEngine) {
    let deadline = Instant::now() + Duration::from_secs(10);
    while Instant::now() < deadline {
        let _ = engine.pump();
        if matches!(
            &engine.app.ui_state.current_screen,
            Screen::SearchFields(state)
                if matches!(
                    state.search_state.as_ref().map(|search| search.phase),
                    Some(SearchPhase::Complete { .. })
                )
        ) {
            return;
        }
        thread::sleep(Duration::from_millis(10));
    }
    panic!("search did not complete");
}

fn wait_until_invalid_search(engine: &mut ScooterEngine) {
    let deadline = Instant::now() + Duration::from_secs(10);
    while Instant::now() < deadline {
        let _ = engine.pump();
        if engine.app.search_fields.fields[0].error().is_some() {
            return;
        }
        thread::sleep(Duration::from_millis(10));
    }
    panic!("search did not become invalid");
}

fn wait_until_preview_updated(engine: &mut ScooterEngine) {
    let deadline = Instant::now() + Duration::from_secs(10);
    while Instant::now() < deadline {
        let _ = engine.pump();
        if engine.app.is_preview_updated() {
            return;
        }
        thread::sleep(Duration::from_millis(10));
    }
    panic!("replacement preview did not finish");
}

fn wait_until_toast_dismissed(engine: &mut ScooterEngine) {
    let deadline = Instant::now() + Duration::from_secs(3);
    while Instant::now() < deadline {
        let _ = engine.pump();
        if engine.app.toast_message().is_none() {
            return;
        }
        thread::sleep(Duration::from_millis(10));
    }
    panic!("toast did not dismiss");
}

fn set_text_field(engine: &mut ScooterEngine, index: usize, text: &str) {
    let Field::Text(field) = &mut engine.app.search_fields.fields[index].field else {
        panic!("field {index} should be text");
    };
    field.set_text(text);
}

fn error_result(path: &str, line: usize, error: &str) -> SearchResultWithReplacement {
    SearchResultWithReplacement {
        search_result: SearchResult::new_line(
            Some(PathBuf::from(path)),
            line,
            "original".to_string(),
            LineEnding::Lf,
            true,
        ),
        replacement: "replacement".to_string(),
        replace_result: Some(ReplaceResult::Error(error.to_string())),
        preview_error: None,
    }
}

#[test]
fn fields_fresh_and_populated() {
    let (_fixture, mut engine) = engine_with_fixture("nothing to find\n");
    assert_frame(&mut engine, "fields_fresh", STANDARD_SIZE);

    set_text_field(&mut engine, 0, "needle");
    set_text_field(&mut engine, 1, "replacement");
    set_text_field(&mut engine, 5, "*.rs");
    set_text_field(&mut engine, 6, "target");
    let Field::Checkbox(fixed_strings) = &mut engine.app.search_fields.fields[2].field else {
        panic!("fixed strings should be a checkbox");
    };
    fixed_strings.checked = true;
    engine.app.search_fields.highlighted = 6;
    assert_frame(&mut engine, "fields_populated", STANDARD_SIZE);
}

#[test]
fn field_error_popup_and_closed_error() {
    let (_fixture, mut engine) = engine_with_fixture("alpha\n");
    type_text(&mut engine, "(");
    wait_until_invalid_search(&mut engine);
    press(&mut engine, "enter", 0);
    assert_frame(&mut engine, "field_error_popup", STANDARD_SIZE);
    press(&mut engine, "esc", 0);
    assert_frame(&mut engine, "field_error_after_popup_closed", STANDARD_SIZE);
}

#[test]
fn completed_search_layouts_and_focus() {
    let (_fixture, mut engine) = engine_with_fixture("alpha one\nalpha two\nalpha three\n");
    complete_search(&mut engine, "alpha");
    assert_frame(&mut engine, "search_complete_fields_narrow", STANDARD_SIZE);
    assert_frame(&mut engine, "search_complete_fields_wide", WIDE_SIZE);
    press(&mut engine, "enter", 0);
    assert_frame(&mut engine, "search_complete_results_narrow", STANDARD_SIZE);
    assert_frame(&mut engine, "search_complete_results_wide", WIDE_SIZE);
}

#[test]
fn previews_cover_single_line_multiline_and_wrapping() {
    let (_fixture, mut engine) =
        engine_with_fixture("before context\nalpha bright world\nafter context\n");
    search_with_replacement(&mut engine, "alpha", "OMEGA");
    assert_frame(
        &mut engine,
        "preview_single_line_word_emphasis",
        STANDARD_SIZE,
    );

    let (_fixture, mut engine) = engine_with_fixture(
        "before context\nfirst matched line\nsecond matched line\nafter context\n",
    );
    press(&mut engine, "m", 4);
    set_text_field(&mut engine, 0, "first matched line\nsecond matched line");
    press(&mut engine, "enter", 0);
    wait_until_complete(&mut engine);
    press(&mut engine, "esc", 0);
    press(&mut engine, "tab", 0);
    type_text(&mut engine, "joined");
    wait_until_preview_updated(&mut engine);
    press(&mut engine, "enter", 0);
    wait_until_toast_dismissed(&mut engine);
    assert_frame(&mut engine, "preview_multiline_diff", STANDARD_SIZE);

    let long_line = format!("alpha {}\n", "long preview context ".repeat(12));
    let (_fixture, mut engine) = engine_with_fixture(&long_line);
    search_with_replacement(&mut engine, "alpha", "OMEGA");
    assert_frame(&mut engine, "preview_wrapping_off", STANDARD_SIZE);
    press(&mut engine, "l", 2);
    wait_until_toast_dismissed(&mut engine);
    assert_frame(&mut engine, "preview_wrapping_on", STANDARD_SIZE);
}

#[test]
fn highlighted_rust_preview_snapshots_and_cache() {
    let source = concat!(
        "pub fn context_before_with_a_deliberately_long_name() { let value = 1; }\n",
        "let alpha = value;\n",
        "pub fn context_after() -> usize { 2 }\n",
    );
    let (fixture, mut engine) = engine_with_named_fixture("fixture.rs", source);
    search_with_replacement(&mut engine, "alpha", "OMEGA");

    let wide = engine.render(WIDE_SIZE.0, WIDE_SIZE.1);
    assert!(wide.runs.iter().any(|run| run.tag.as_str().starts_with("s:")));
    let highlight_computations = engine.highlight_computations();
    let file_reads = engine.preview_file_reads();
    let content_hashes = engine.preview_content_hashes();
    assert_eq!(file_reads, 1);
    assert_eq!(content_hashes, 0);
    let _ = engine.render(WIDE_SIZE.0, WIDE_SIZE.1);
    assert_eq!(engine.highlight_computations(), highlight_computations);
    assert_eq!(engine.preview_file_reads(), file_reads);
    assert_eq!(engine.preview_content_hashes(), content_hashes);

    assert_frame(&mut engine, "highlighted_preview_wide", WIDE_SIZE);
    assert_frame(&mut engine, "highlighted_preview_narrow", STANDARD_SIZE);

    press(&mut engine, "l", 2);
    wait_until_toast_dismissed(&mut engine);
    assert_frame(&mut engine, "highlighted_preview_wrapping", STANDARD_SIZE);

    // Metadata is checked on every render, but a changed file replaces the
    // content Arc rather than retaining stale preview text.
    std::thread::sleep(std::time::Duration::from_millis(10));
    fs::write(fixture.path().join("fixture.rs"), "let changed = true;\n").unwrap();
    let _ = engine.render(WIDE_SIZE.0, WIDE_SIZE.1);
    assert_eq!(engine.preview_file_reads(), file_reads + 1);

}

#[test]
fn previews_show_file_errors_and_change_guards() {
    let (fixture, mut engine) = engine_with_fixture("alpha original\n");
    complete_search(&mut engine, "alpha");
    press(&mut engine, "enter", 0);
    fs::remove_file(fixture.path().join("fixture.txt")).expect("delete fixture after search");
    assert_frame(&mut engine, "preview_deleted_file_error", STANDARD_SIZE);

    let (fixture, mut engine) = engine_with_fixture("alpha original\n");
    complete_search(&mut engine, "alpha");
    press(&mut engine, "enter", 0);
    fs::write(fixture.path().join("fixture.txt"), "changed after search\n")
        .expect("change fixture after search");
    assert_frame(&mut engine, "preview_file_changed_guard", STANDARD_SIZE);
}

#[test]
fn multiselect_ranges_render_primary_direction_and_exclusion() {
    let (_fixture, mut engine) = engine_with_fixture("alpha 1\nalpha 2\nalpha 3\nalpha 4\n");
    complete_search(&mut engine, "alpha");
    press(&mut engine, "enter", 0);
    press(&mut engine, "v", 0);
    press(&mut engine, "j", 0);
    press(&mut engine, "j", 0);
    assert_frame(&mut engine, "multiselect_primary_bottom", WIDE_SIZE);
    press(&mut engine, ";", 4);
    assert_frame(&mut engine, "multiselect_flipped_direction", WIDE_SIZE);

    let (_fixture, mut engine) = engine_with_fixture("alpha 1\nalpha 2\nalpha 3\nalpha 4\n");
    complete_search(&mut engine, "alpha");
    press(&mut engine, "enter", 0);
    press(&mut engine, "j", 0);
    press(&mut engine, " ", 0);
    press(&mut engine, "k", 0);
    press(&mut engine, "v", 0);
    press(&mut engine, "j", 0);
    press(&mut engine, "j", 0);
    assert_frame(&mut engine, "multiselect_excluded_inside_range", WIDE_SIZE);
}

#[test]
fn help_popups_reflect_focus_context() {
    let (_fixture, mut engine) = engine_with_fixture("alpha\n");
    press(&mut engine, "h", 2);
    assert_frame(&mut engine, "help_popup_fields_focus", STANDARD_SIZE);

    let (_fixture, mut engine) = engine_with_fixture("alpha\n");
    complete_search(&mut engine, "alpha");
    press(&mut engine, "enter", 0);
    press(&mut engine, "h", 2);
    assert_frame(&mut engine, "help_popup_results_focus", STANDARD_SIZE);
}

#[test]
fn multiline_toast_is_visible() {
    let (_fixture, mut engine) = engine_with_fixture("alpha\n");
    press(&mut engine, "m", 4);
    assert_frame(&mut engine, "toast_multiline_on", STANDARD_SIZE);
}

#[test]
fn performing_and_completed_replacement_screens() {
    let (_fixture, mut engine) = engine_with_fixture("alpha\n");
    let (_sender, receiver) = mpsc::unbounded_channel();
    engine.app.ui_state.current_screen =
        Screen::PerformingReplacement(PerformingReplacementState::new(
            receiver,
            Arc::new(AtomicBool::new(false)),
            Arc::new(AtomicUsize::new(3)),
            7,
        ));
    assert_frame(&mut engine, "performing_replacement", STANDARD_SIZE);

    let (_fixture, mut engine) = engine_with_fixture("alpha\n");
    engine.app.ui_state.current_screen = Screen::Results(ReplaceState {
        num_successes: 7,
        num_ignored: 2,
        errors: Vec::new(),
        replacement_errors_pos: 0,
    });
    assert_frame(&mut engine, "replacement_results_success", STANDARD_SIZE);
}

#[test]
fn replacement_error_results_support_scrolling() {
    let errors = vec![
        error_result("first.txt", 4, "permission denied"),
        error_result("second.txt", 9, "disk full"),
    ];
    let (_fixture, mut engine) = engine_with_fixture("alpha\n");
    engine.app.ui_state.current_screen = Screen::Results(ReplaceState {
        num_successes: 1,
        num_ignored: 2,
        errors: errors.clone(),
        replacement_errors_pos: 0,
    });
    assert_frame(
        &mut engine,
        "replacement_results_errors_unscrolled",
        STANDARD_SIZE,
    );
    engine.app.ui_state.current_screen = Screen::Results(ReplaceState {
        num_successes: 1,
        num_ignored: 2,
        errors,
        replacement_errors_pos: 1,
    });
    assert_frame(
        &mut engine,
        "replacement_results_errors_scrolled",
        STANDARD_SIZE,
    );
}

#[test]
fn empty_and_pending_search_banners() {
    let (_fixture, mut engine) = engine_with_fixture("alpha one\nalpha two\n");
    assert_frame(&mut engine, "empty_search_banner", STANDARD_SIZE);

    complete_search(&mut engine, "alpha");
    press(&mut engine, "!", 0);
    assert_frame(&mut engine, "search_pending_debounce_banner", STANDARD_SIZE);
}
