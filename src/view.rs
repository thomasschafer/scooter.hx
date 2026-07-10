//! Native frame model for Scooter's search-fields screen.

use std::{cmp::min, path::Path, time::Duration};

use scooter_core::{
    app::{App, FocussedSection, InputSource, Screen, SearchPhase, SearchState},
    fields::{Field, NUM_SEARCH_FIELDS, SearchField},
    utils::relative_path,
};
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

/// A semantic styled string positioned relative to the Steel popup content.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Run {
    pub(crate) x: usize,
    pub(crate) y: usize,
    pub(crate) text: String,
    pub(crate) tag: String,
}

/// Complete frame, including an optional text cursor position.
#[derive(Debug, Default, PartialEq, Eq)]
pub(crate) struct Frame {
    pub(crate) runs: Vec<Run>,
    pub(crate) cursor: Option<(usize, usize)>,
}

#[derive(Debug, Clone, Copy)]
struct FieldsLayout {
    x: usize,
    y: usize,
    width: usize,
    count: usize,
    banner_y: usize,
}

const FIELD_HEIGHT: usize = 3;
const FIELD_COUNT_WHEN_RESULTS_FOCUSSED: usize = 2;
const BANNER_HEIGHT: usize = 2;

/// Render the search fields, result banner, and the small S2 result list.
pub(crate) fn render(app: &mut App, width: usize, height: usize) -> Frame {
    let mut frame = Frame::default();
    if width == 0 || height == 0 {
        return frame;
    }

    let Screen::SearchFields(search_fields_state) = &app.ui_state.current_screen else {
        return frame;
    };

    let fields_focussed = search_fields_state.focussed_section == FocussedSection::SearchFields;
    let requested_count = if fields_focussed {
        NUM_SEARCH_FIELDS as usize
    } else {
        FIELD_COUNT_WHEN_RESULTS_FOCUSSED
    };
    let layout = fields_layout(width, height, requested_count);

    for (index, field) in app
        .search_fields
        .fields
        .iter()
        .take(layout.count)
        .enumerate()
    {
        let highlighted = fields_focussed && index == app.search_fields.highlighted;
        render_field(
            &mut frame.runs,
            field,
            layout,
            index,
            highlighted,
            width,
            height,
        );
    }

    frame.cursor = field_cursor(app, layout, height);

    if layout.banner_y >= height {
        return frame;
    }

    let search_is_empty = app.search_fields.search().text().is_empty();
    let search_is_invalid = app.search_fields.fields[0].error().is_some();
    let base_path = match &app.input_source {
        InputSource::Directory(directory) => directory.as_path(),
        InputSource::Stdin(_) => Path::new("."),
    };
    let Screen::SearchFields(search_fields_state) = &mut app.ui_state.current_screen else {
        return frame;
    };
    let replacements_in_progress = search_fields_state.replacements_in_progress();

    if let Some(search_state) = search_fields_state.search_state.as_mut() {
        render_results(
            &mut frame.runs,
            base_path,
            search_state,
            replacements_in_progress,
            layout,
            width,
            height,
        );
    } else if search_is_empty {
        render_banner(
            &mut frame.runs,
            layout,
            0,
            "Search is empty",
            "error",
            None,
            false,
            replacements_in_progress,
            width,
            height,
        );
    } else if search_is_invalid {
        render_banner(
            &mut frame.runs,
            layout,
            0,
            "Invalid search",
            "error",
            None,
            false,
            replacements_in_progress,
            width,
            height,
        );
    }

    frame
}

/// Return the cursor that corresponds to [`render`] without changing view state.
pub(crate) fn cursor(app: &App, width: usize, height: usize) -> Option<(usize, usize)> {
    if width == 0 || height == 0 {
        return None;
    }

    let Screen::SearchFields(search_fields_state) = &app.ui_state.current_screen else {
        return None;
    };
    let requested_count = if search_fields_state.focussed_section == FocussedSection::SearchFields {
        NUM_SEARCH_FIELDS as usize
    } else {
        FIELD_COUNT_WHEN_RESULTS_FOCUSSED
    };
    field_cursor(app, fields_layout(width, height, requested_count), height)
}

fn field_cursor(app: &App, layout: FieldsLayout, height: usize) -> Option<(usize, usize)> {
    if app.show_popup() {
        return None;
    }

    let Screen::SearchFields(search_fields_state) = &app.ui_state.current_screen else {
        return None;
    };
    if search_fields_state.focussed_section != FocussedSection::SearchFields {
        return None;
    }

    let index = app.search_fields.highlighted;
    let cursor_offset = app.search_fields.highlighted_field().cursor_pos()?;
    let field_y = layout.y + index * FIELD_HEIGHT;
    if index >= layout.count || layout.width <= 2 || field_y.saturating_add(1) >= height {
        return None;
    }

    let inner_left = layout.x + 1;
    let inner_right = layout.x + layout.width - 2;
    Some((
        min(inner_left.saturating_add(cursor_offset), inner_right),
        field_y + 1,
    ))
}

fn fields_layout(frame_width: usize, height: usize, requested_count: usize) -> FieldsLayout {
    let count = requested_count.min(height / FIELD_HEIGHT);
    let fields_height = count * FIELD_HEIGHT;
    // Match the TUI: fields at the top of the content area, a one-row gap,
    // then the results banner and list fill the remaining height.
    let width_percentage = if frame_width >= 300 { 80 } else { 90 };
    let width = (frame_width * width_percentage / 100).clamp(1, frame_width);
    let x = (frame_width - width) / 2;

    FieldsLayout {
        x,
        y: 0,
        width,
        count,
        banner_y: fields_height + 1,
    }
}

fn render_field(
    runs: &mut Vec<Run>,
    field: &SearchField,
    layout: FieldsLayout,
    index: usize,
    highlighted: bool,
    frame_width: usize,
    frame_height: usize,
) {
    let y = layout.y + index * FIELD_HEIGHT;
    let border_tag = if highlighted { "active" } else { "text" };

    match &field.field {
        Field::Text(text) => render_text_field(
            runs,
            field,
            text.text(),
            layout.x,
            y,
            layout.width,
            border_tag,
            frame_width,
            frame_height,
        ),
        Field::Checkbox(checkbox) => render_checkbox_field(
            runs,
            field,
            checkbox.checked,
            layout.x,
            y,
            layout.width,
            border_tag,
            frame_width,
            frame_height,
        ),
    }
}

#[allow(clippy::too_many_arguments)]
fn render_text_field(
    runs: &mut Vec<Run>,
    field: &SearchField,
    value: &str,
    x: usize,
    y: usize,
    field_width: usize,
    border_tag: &'static str,
    frame_width: usize,
    frame_height: usize,
) {
    if field_width == 0 || y >= frame_height {
        return;
    }

    if field_width == 1 {
        render_narrow_box(
            runs,
            x,
            y,
            field_width,
            border_tag,
            frame_width,
            frame_height,
        );
        return;
    }
    if field_width == 2 {
        render_plain_box(
            runs,
            x,
            y,
            field_width,
            value,
            border_tag,
            frame_width,
            frame_height,
        );
        return;
    }

    let end_x = x + field_width;
    let mut title_x = x;
    add_segment(runs, &mut title_x, y, "┌─", border_tag, end_x, frame_height);
    add_title_segments(
        runs,
        &mut title_x,
        y,
        field,
        border_tag,
        end_x.saturating_sub(1),
        frame_height,
    );
    let trailing_border = format!("{}┐", "─".repeat(end_x.saturating_sub(title_x + 1)));
    add_segment(
        runs,
        &mut title_x,
        y,
        &trailing_border,
        border_tag,
        end_x,
        frame_height,
    );

    add_run(runs, x, y + 1, "│", border_tag, frame_width, frame_height);
    let value = truncate(value, field_width.saturating_sub(2));
    add_run(
        runs,
        x + 1,
        y + 1,
        &value,
        "text",
        frame_width,
        frame_height,
    );
    add_run(
        runs,
        end_x - 1,
        y + 1,
        "│",
        border_tag,
        frame_width,
        frame_height,
    );
    add_run(
        runs,
        x,
        y + 2,
        &format!("└{}┘", "─".repeat(field_width.saturating_sub(2))),
        border_tag,
        frame_width,
        frame_height,
    );
}

#[allow(clippy::too_many_arguments)]
fn render_checkbox_field(
    runs: &mut Vec<Run>,
    field: &SearchField,
    checked: bool,
    x: usize,
    y: usize,
    field_width: usize,
    border_tag: &'static str,
    frame_width: usize,
    frame_height: usize,
) {
    if field_width == 0 || y >= frame_height {
        return;
    }

    // This intentionally remains a fixed five-cell control, as in the TUI.
    // On very narrow frames it safely shrinks instead of spilling into the
    // title area.
    let checkbox_width = field_width.min(5);
    render_plain_box(
        runs,
        x,
        y,
        checkbox_width,
        if checked { " X " } else { "" },
        border_tag,
        frame_width,
        frame_height,
    );

    let mut title_x = x.saturating_add(checkbox_width).saturating_add(1);
    let end_x = x + field_width;
    add_title_segments(
        runs,
        &mut title_x,
        y + 1,
        field,
        border_tag,
        end_x,
        frame_height,
    );
}

#[allow(clippy::too_many_arguments)]
fn render_plain_box(
    runs: &mut Vec<Run>,
    x: usize,
    y: usize,
    box_width: usize,
    contents: &str,
    border_tag: &'static str,
    frame_width: usize,
    frame_height: usize,
) {
    if box_width == 0 || y >= frame_height {
        return;
    }
    if box_width == 1 {
        render_narrow_box(runs, x, y, box_width, border_tag, frame_width, frame_height);
        return;
    }

    let end_x = x + box_width;
    add_run(
        runs,
        x,
        y,
        &format!("┌{}┐", "─".repeat(box_width.saturating_sub(2))),
        border_tag,
        frame_width,
        frame_height,
    );
    add_run(runs, x, y + 1, "│", border_tag, frame_width, frame_height);
    add_run(
        runs,
        x + 1,
        y + 1,
        &truncate(contents, box_width.saturating_sub(2)),
        "text",
        frame_width,
        frame_height,
    );
    add_run(
        runs,
        end_x - 1,
        y + 1,
        "│",
        border_tag,
        frame_width,
        frame_height,
    );
    add_run(
        runs,
        x,
        y + 2,
        &format!("└{}┘", "─".repeat(box_width.saturating_sub(2))),
        border_tag,
        frame_width,
        frame_height,
    );
}

fn render_narrow_box(
    runs: &mut Vec<Run>,
    x: usize,
    y: usize,
    box_width: usize,
    border_tag: &'static str,
    frame_width: usize,
    frame_height: usize,
) {
    if box_width == 1 {
        for row in 0..FIELD_HEIGHT {
            add_run(runs, x, y + row, "│", border_tag, frame_width, frame_height);
        }
    }
}

fn add_title_segments(
    runs: &mut Vec<Run>,
    x: &mut usize,
    y: usize,
    field: &SearchField,
    title_tag: &'static str,
    end_x: usize,
    frame_height: usize,
) {
    add_segment(
        runs,
        x,
        y,
        field.name.title(),
        title_tag,
        end_x,
        frame_height,
    );
    if let Some(error) = field.error() {
        add_segment(
            runs,
            x,
            y,
            &format!(" (Error: {})", error.short),
            "error",
            end_x,
            frame_height,
        );
    }
}

fn render_results(
    runs: &mut Vec<Run>,
    base_path: &Path,
    search_state: &mut SearchState,
    replacements_in_progress: Option<(usize, usize)>,
    layout: FieldsLayout,
    width: usize,
    height: usize,
) {
    let (status, tag) = status(search_state.phase);
    let is_complete = matches!(search_state.phase, SearchPhase::Complete { .. });
    render_banner(
        runs,
        layout,
        search_state.results.len(),
        status,
        tag,
        search_state.phase.elapsed(),
        is_complete,
        replacements_in_progress,
        width,
        height,
    );

    let list_y = layout.banner_y.saturating_add(BANNER_HEIGHT);
    if list_y >= height {
        return;
    }

    let num_to_render = height - list_y;
    search_state.num_displayed = Some(num_to_render);
    clamp_result_offset(search_state, num_to_render);

    for (index, result) in search_state
        .results
        .iter()
        .enumerate()
        .skip(search_state.view_offset)
        .take(num_to_render)
    {
        let selected = search_state.is_primary_selected(index);
        let prefix = if selected { "> " } else { "  " };
        let included = if result.search_result.included {
            "[x]"
        } else {
            "[ ]"
        };
        let path = result.search_result.path.as_ref().map_or_else(
            || "<stdin>".to_string(),
            |path| relative_path(base_path, path),
        );
        let text = format!(
            "{prefix}{included} {path}:{}",
            result.search_result.start_line_number()
        );
        add_run(
            runs,
            layout.x,
            list_y + index.saturating_sub(search_state.view_offset),
            &truncate(&text, layout.width),
            if selected { "selection" } else { "text" },
            width,
            height,
        );
    }
}

#[allow(clippy::too_many_arguments)]
fn render_banner(
    runs: &mut Vec<Run>,
    layout: FieldsLayout,
    num_results: usize,
    status: &str,
    status_tag: &'static str,
    time_taken: Option<Duration>,
    is_complete: bool,
    replacements_in_progress: Option<(usize, usize)>,
    width: usize,
    height: usize,
) {
    let end_x = layout.x + layout.width;
    let left_number = format!("Results: {num_results}");
    let left_status = format!(" [{status}]");
    let left_width = display_width(&left_number) + display_width(&left_status);
    let right = time_taken.map(|duration| format!("[Time taken: {}]", format_duration(duration)));
    let right_width = right.as_deref().map_or(0, display_width);
    let show_right = right.is_some() && left_width + right_width <= layout.width;
    let right_x = if show_right {
        end_x - right_width
    } else {
        end_x
    };

    let mut left_x = layout.x;
    add_segment(
        runs,
        &mut left_x,
        layout.banner_y,
        &left_number,
        "text",
        right_x,
        height,
    );
    add_segment(
        runs,
        &mut left_x,
        layout.banner_y,
        &left_status,
        status_tag,
        right_x,
        height,
    );

    if let Some(right) = right.filter(|_| show_right) {
        let mut time_x = right_x;
        add_segment(
            runs,
            &mut time_x,
            layout.banner_y,
            &right,
            if is_complete { "diff-added" } else { "info" },
            end_x,
            height,
        );
    }

    if let Some(updating) = preview_update_status(replacements_in_progress) {
        let available = right_x.saturating_sub(left_x);
        let updating = truncate(&updating, available);
        let updating_width = display_width(&updating);
        let mut updating_x = left_x + available.saturating_sub(updating_width) / 2;
        add_segment(
            runs,
            &mut updating_x,
            layout.banner_y,
            &updating,
            "info",
            right_x,
            height,
        );
    }

    debug_assert!(end_x <= width);
}

fn preview_update_status(replacements_in_progress: Option<(usize, usize)>) -> Option<String> {
    replacements_in_progress.and_then(|(complete, total)| {
        (total >= 10_000).then(|| preview_percentage(complete, total))
    })
}

#[allow(clippy::cast_precision_loss)]
fn preview_percentage(complete: usize, total: usize) -> String {
    format!(
        "[Updating preview: {complete}/{total} ({:.2}%)]",
        (complete as f64 / total as f64) * 100.0
    )
}

fn status(phase: SearchPhase) -> (&'static str, &'static str) {
    match phase {
        SearchPhase::Invalid => ("Invalid search", "error"),
        SearchPhase::Complete { .. } => ("Search complete", "diff-added"),
        SearchPhase::Pending | SearchPhase::Running { .. } => ("Still searching...", "info"),
    }
}

fn format_duration(duration: Duration) -> String {
    format!("{}.{:03}s", duration.as_secs(), duration.subsec_millis())
}

fn clamp_result_offset(search_state: &mut SearchState, num_to_render: usize) {
    let num_results = search_state.results.len();
    let selected = search_state.primary_selected_pos();
    if selected < search_state.view_offset + 1 {
        search_state.view_offset = selected.saturating_sub(1);
    } else if selected > (search_state.view_offset + num_to_render).saturating_sub(2)
        || search_state.view_offset + num_to_render > num_results
    {
        search_state.view_offset = min(
            selected.saturating_add(2).saturating_sub(num_to_render),
            num_results.saturating_sub(num_to_render),
        );
    }
}

fn add_segment(
    runs: &mut Vec<Run>,
    x: &mut usize,
    y: usize,
    text: &str,
    tag: &str,
    end_x: usize,
    height: usize,
) {
    let clipped = truncate(text, end_x.saturating_sub(*x));
    let clipped_width = display_width(&clipped);
    if !clipped.is_empty() && y < height {
        runs.push(Run {
            x: *x,
            y,
            text: clipped,
            tag: tag.to_owned(),
        });
    }
    *x = x.saturating_add(clipped_width);
}

fn add_run(
    runs: &mut Vec<Run>,
    x: usize,
    y: usize,
    text: &str,
    tag: &str,
    width: usize,
    height: usize,
) {
    if x >= width || y >= height {
        return;
    }

    let text = truncate(text, width - x);
    if !text.is_empty() {
        runs.push(Run {
            x,
            y,
            text,
            tag: tag.to_string(),
        });
    }
}

fn truncate(text: &str, max_width: usize) -> String {
    let mut used = 0;
    let mut result = String::new();
    for character in text.chars() {
        let character_width = UnicodeWidthChar::width(character).unwrap_or(0);
        if used + character_width > max_width {
            break;
        }
        result.push(character);
        used += character_width;
    }
    result
}

fn display_width(text: &str) -> usize {
    UnicodeWidthStr::width(text)
}

#[cfg(test)]
mod tests {
    use std::{
        collections::BTreeMap,
        fs, thread,
        time::{Duration, Instant},
    };

    use scooter_core::app::{Screen, SearchPhase};
    use tempfile::tempdir;

    use crate::engine::ScooterEngine;

    use super::{Frame, display_width, truncate};

    #[test]
    fn truncates_by_display_width() {
        assert_eq!(truncate("a界b", 3), "a界");
        assert_eq!(truncate("a界b", 2), "a");
    }

    #[test]
    fn field_title_never_overflows_its_box() {
        assert_eq!(display_width(&box_top_for_test("Search text", 12)), 12);
    }

    #[test]
    fn renders_active_search_within_bounds_at_every_supported_size() {
        let fixture = tempdir().expect("fixture directory");
        fs::write(fixture.path().join("matches.txt"), "alpha\nalphabet\n").expect("write fixture");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        for character in "alpha".chars() {
            engine.handle_key(&character.to_string(), 0);
        }
        wait_until_complete(&mut engine);

        let mut widths = (0..=3).collect::<Vec<_>>();
        widths.extend([10, 24, 60, 79, 80, 81, 110, 111, 160, 250]);
        let mut heights = (0..=3).collect::<Vec<_>>();
        heights.extend([4, 10, 23, 24, 40, 55, 80]);

        for width in widths {
            for &height in &heights {
                let frame = engine.render(width, height);
                assert_frame_is_well_formed(&frame, width, height);
            }
        }
    }

    #[test]
    fn cursor_stays_inside_the_text_field_in_render_and_cursor_queries() {
        let fixture = tempdir().expect("fixture directory");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        for _ in 0..80 {
            engine.handle_key("a", 0);
        }

        let frame = engine.render(24, 10);
        let cursor = engine.cursor(24, 10);
        assert_eq!(frame.cursor, cursor);
        assert_eq!(cursor, Some((20, 1)));
    }

    fn wait_until_complete(engine: &mut ScooterEngine) {
        let deadline = Instant::now() + Duration::from_secs(10);
        while Instant::now() < deadline {
            engine.pump();
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

    fn assert_frame_is_well_formed(frame: &Frame, width: usize, height: usize) {
        let mut rows = BTreeMap::new();
        for run in &frame.runs {
            assert!(
                run.y < height,
                "run exceeds height: {run:?} at {width}x{height}"
            );
            assert!(
                run.x + display_width(&run.text) <= width,
                "run exceeds width: {run:?} at {width}x{height}"
            );
            rows.entry(run.y).or_insert_with(Vec::new).push(run);
        }

        for (y, runs) in &mut rows {
            runs.sort_by_key(|run| run.x);
            for pair in runs.windows(2) {
                let [previous, next] = pair else {
                    unreachable!()
                };
                assert!(
                    previous.x + display_width(&previous.text) <= next.x,
                    "overlapping runs on row {y} at {width}x{height}: {previous:?} then {next:?}"
                );
            }
        }
    }

    fn box_top_for_test(title: &str, width: usize) -> String {
        let mut result = String::from("┌─");
        result.push_str(&truncate(title, width.saturating_sub(3)));
        result.push_str(&"─".repeat(width.saturating_sub(display_width(&result) + 1)));
        result.push('┐');
        result
    }
}
