//! Native frame model for Scooter's search-fields screen.

use std::{cmp::min, path::Path, sync::{Arc, atomic::Ordering}};

use crate::highlight::{HighlightEngine, HighlightSpan};

use scooter_core::{
    app::{App, FocussedSection, InputSource, Popup, Screen, SearchPhase, SearchState},
    diff::line_diff,
    fields::{Field, NUM_SEARCH_FIELDS, SearchField},
    replace::{PerformingReplacementState, ReplaceState},
    search::{MatchContent, SearchResultWithReplacement},
    keyboard::{KeyCode, KeyEvent, KeyModifiers},
    utils::{read_lines_range, relative_path, strip_control_chars},
};
use unicode_width::UnicodeWidthChar;
mod banner;
mod canvas;
mod layout;

use banner::{format_duration, render_banner, render_footer, status};
pub(crate) use canvas::{Frame, Run, StyleTag};
use canvas::{add_centered_run, add_run, add_segment, display_width, truncate};
use layout::{
    FieldsLayout, PopupArea, TitleAlignment, default_content_width, fields_layout, popup_area,
    popup_inner,
};

const FIELD_HEIGHT: usize = 3;
const FIELD_COUNT_WHEN_RESULTS_FOCUSSED: usize = 2;
const BANNER_HEIGHT: usize = 2;
const NARROW_RESULTS_WIDTH: usize = 110;
const NARROW_LIST_HEIGHT: usize = 5;
const MULTILINE_DETAILED_DIFF_MAX_BYTES: usize = 20_000;
const WRAPPED_LINE_PREFIX: &str = "  ↪ ";

#[derive(Debug, Clone)]
struct PopupLine {
    text: String,
    tag: StyleTag,
}

#[derive(Debug, Clone)]
struct PreviewSegment {
    text: String,
    tag: StyleTag,
}

#[derive(Debug, Clone, Default)]
struct PreviewLine {
    segments: Vec<PreviewSegment>,
}

#[derive(Debug)]
struct PreviewSections {
    before: Vec<PreviewLine>,
    diff: Vec<PreviewLine>,
    after: Vec<PreviewLine>,
}

#[derive(Debug, Clone)]
struct IndexedLine {
    number: usize,
    text: String,
    byte_offset: Option<usize>,
}
type ContextLines = Vec<IndexedLine>;

struct PreviewRead {
    lines: ContextLines,
    spans: Option<Arc<[HighlightSpan]>>,
}

/// Render Scooter's active screen plus its footer and transient overlays.
#[allow(clippy::too_many_lines)]
pub(crate) fn render(
    app: &mut App,
    highlight_engine: &HighlightEngine,
    syntax_highlighting: bool,
    open_in_editor_bg: Option<KeyEvent>,
    hide: &[KeyEvent],
    width: usize,
    height: usize,
) -> Frame {
    let mut frame = Frame::default();
    if width == 0 || height == 0 {
        return frame;
    }

    // The footer is a permanent final row, so every screen layout receives
    // only the rows above it.  This also leaves the toast a stable anchor.
    let content_height = height.saturating_sub(1);
    frame.cursor = cursor(app, width, height);
    match &mut app.ui_state.current_screen {
        Screen::SearchFields(search_fields_state) => {
            let fields_focussed =
                search_fields_state.focussed_section == FocussedSection::SearchFields;
            let requested_count = if fields_focussed {
                NUM_SEARCH_FIELDS as usize
            } else {
                FIELD_COUNT_WHEN_RESULTS_FOCUSSED
            };
            let layout = fields_layout(width, content_height, requested_count, FIELD_HEIGHT);

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
                    content_height,
                );
            }

            if layout.banner_y < content_height {
                let search_is_empty = app.search_fields.search().text().is_empty();
                let search_is_invalid = app.search_fields.fields[0].error().is_some();
                let wrap_preview_text = app.config.preview.wrap_text;
                let input_source = &app.input_source;
                let replacements_in_progress = search_fields_state.replacements_in_progress();

                if let Some(search_state) = search_fields_state.search_state.as_mut() {
                    render_results(
                        &mut frame.runs,
                        input_source,
                        wrap_preview_text,
                        highlight_engine,
                        syntax_highlighting,
                        search_fields_state.focussed_section == FocussedSection::SearchResults,
                        search_state,
                        replacements_in_progress,
                        layout,
                        width,
                        content_height,
                    );
                } else if search_is_empty {
                    render_banner(
                        &mut frame.runs,
                        layout,
                        0,
                        "Search is empty",
                        StyleTag::Error,
                        None,
                        false,
                        replacements_in_progress,
                        width,
                        content_height,
                    );
                } else if search_is_invalid {
                    render_banner(
                        &mut frame.runs,
                        layout,
                        0,
                        "Invalid search",
                        StyleTag::Error,
                        None,
                        false,
                        replacements_in_progress,
                        width,
                        content_height,
                    );
                }
            }
        }
        Screen::PerformingReplacement(state) => {
            render_performing_replacement(&mut frame.runs, state, width, content_height);
        }
        Screen::Results(state) => {
            render_replacement_results(&mut frame.runs, state, width, content_height);
        }
    }

    render_footer(&mut frame.runs, app, width, height - 1);

    if let Some(popup) = app.popup() {
        render_popup(
            &mut frame.runs,
            app,
            popup,
            open_in_editor_bg,
            hide,
            width,
            content_height,
        );
    }
    if let Some(message) = app.toast_message() {
        render_toast(&mut frame.runs, message, width, content_height);
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
    let content_height = height.saturating_sub(1);
    field_cursor(
        app,
        fields_layout(width, content_height, requested_count, FIELD_HEIGHT),
        content_height,
    )
}

fn render_popup(
    runs: &mut Vec<Run>,
    app: &App,
    popup: &Popup,
    open_in_editor_bg: Option<KeyEvent>,
    hide: &[KeyEvent],
    width: usize,
    height: usize,
) {
    match popup {
        Popup::Error => {
            let errors = app.errors();
            let mut lines = Vec::new();
            for (index, error) in errors.iter().enumerate() {
                lines.push(PopupLine {
                    text: error.name.clone(),
                    tag: StyleTag::Active,
                });
                lines.extend(error.long.split('\n').map(|text| PopupLine {
                    text: text.to_string(),
                    tag: StyleTag::Error,
                }));
                if index + 1 < errors.len() {
                    lines.push(PopupLine {
                        text: String::new(),
                        tag: StyleTag::Text,
                    });
                }
            }
            render_paragraph_popup(runs, "Errors", &lines, width, height);
        }
        Popup::Help => render_help_popup(
            runs,
            &help_keymaps(app, open_in_editor_bg, hide),
            width,
            height,
        ),
        Popup::Text { title, body } => {
            let lines = body
                .split('\n')
                .map(|text| PopupLine {
                    text: text.to_string(),
                    tag: StyleTag::Text,
                })
                .collect::<Vec<_>>();
            render_paragraph_popup(runs, title, &lines, width, height);
        }
    }
}

fn help_keymaps(
    app: &App,
    open_in_editor_bg: Option<KeyEvent>,
    hide: &[KeyEvent],
) -> Vec<(String, String)> {
    let mut keymaps = app.keymaps_all();
    let on_results = matches!(
        &app.ui_state.current_screen,
        Screen::SearchFields(state) if state.focussed_section == FocussedSection::SearchResults
    );
    if on_results && let Some(binding) = open_in_editor_bg {
        let insertion = keymaps
            .iter()
            .position(|(_, action)| action == "open in editor")
            .map_or(keymaps.len(), |index| index + 1);
        keymaps.insert(
            insertion,
            (format!("<{binding}>"), "open in background".to_string()),
        );
    }
    let default_hide = [KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE)];
    if hide != default_hide {
        let binding = hide
            .iter()
            .map(|binding| format!("<{binding}>"))
            .collect::<Vec<_>>()
            .join(", ");
        let insertion = keymaps
            .iter()
            .position(|(_, action)| action.contains("back to search fields"))
            .map_or(keymaps.len(), |index| index + 1);
        keymaps.insert(insertion, (binding, "hide Scooter".to_string()));
    }
    keymaps
}

fn render_paragraph_popup(
    runs: &mut Vec<Run>,
    title: &str,
    lines: &[PopupLine],
    width: usize,
    height: usize,
) {
    let area = popup_area(width, height, lines.len());
    draw_popup_box(runs, area, title, width, height);
    let (x, y, content_width, content_height) = popup_inner(area);
    for (offset, line) in lines.iter().take(content_height).enumerate() {
        add_run(
            runs,
            x,
            y + offset,
            &line.text,
            line.tag.clone(),
            x + content_width,
            height,
        );
    }
}

fn render_help_popup(
    runs: &mut Vec<Run>,
    keymaps: &[(String, String)],
    width: usize,
    height: usize,
) {
    let area = popup_area(width, height, keymaps.len());
    draw_popup_box(runs, area, "Help", width, height);
    let (x, y, content_width, content_height) = popup_inner(area);
    let max_key_width = keymaps
        .iter()
        .map(|(key, _)| display_width(key))
        .max()
        .unwrap_or(0);
    let key_width = max_key_width.min(content_width.saturating_sub(2));

    for (offset, (key, action)) in keymaps.iter().take(content_height).enumerate() {
        let key = truncate(key, key_width);
        let key_x = x + key_width.saturating_sub(display_width(&key));
        add_run(
            runs,
            key_x,
            y + offset,
            &key,
            StyleTag::Info,
            x + content_width,
            height,
        );

        let action_x = x + key_width.saturating_add(1);
        if action_x < x + content_width {
            add_run(
                runs,
                action_x,
                y + offset,
                action,
                StyleTag::Text,
                x + content_width,
                height,
            );
        }
    }
}

fn draw_popup_box(
    runs: &mut Vec<Run>,
    area: PopupArea,
    title: &str,
    frame_width: usize,
    frame_height: usize,
) {
    if frame_width == 0 || frame_height == 0 || area.height == 0 {
        return;
    }

    // This is intentionally emitted before all border/content runs.  Steel
    // blits in order, so it clears the underlying screen without requiring a
    // separate clear primitive across the FFI boundary.
    for row in 0..area.height {
        add_run(
            runs,
            area.x,
            area.y + row,
            &" ".repeat(area.width),
            StyleTag::Popup,
            frame_width,
            frame_height,
        );
    }
    draw_box_border(
        runs,
        area,
        Some(title),
        TitleAlignment::Center,
        &(StyleTag::PopupBorder, StyleTag::Popup),
        frame_width,
        frame_height,
    );
}

fn draw_box_border(
    runs: &mut Vec<Run>,
    area: PopupArea,
    title: Option<&str>,
    title_alignment: TitleAlignment,
    tags: &(StyleTag, StyleTag),
    frame_width: usize,
    frame_height: usize,
) {
    if area.width == 0 || area.height == 0 {
        return;
    }
    if area.width == 1 {
        for row in 0..area.height {
            add_run(
                runs,
                area.x,
                area.y + row,
                "│",
                tags.0.clone(),
                frame_width,
                frame_height,
            );
        }
        return;
    }

    add_run(
        runs,
        area.x,
        area.y,
        &format!("┌{}┐", "─".repeat(area.width.saturating_sub(2))),
        tags.0.clone(),
        frame_width,
        frame_height,
    );
    if let Some(title) = title.filter(|_| area.width > 2) {
        match title_alignment {
            TitleAlignment::Center => add_centered_run(
                runs,
                area.y,
                title,
                tags.1.clone(),
                area.x + 1,
                area.width.saturating_sub(2),
                frame_width,
                frame_height,
            ),
            TitleAlignment::Left => add_run(
                runs,
                area.x + 1,
                area.y,
                title,
                tags.1.clone(),
                area.x + area.width.saturating_sub(1),
                frame_height,
            ),
        }
    }
    for row in 1..area.height.saturating_sub(1) {
        add_run(
            runs,
            area.x,
            area.y + row,
            "│",
            tags.0.clone(),
            frame_width,
            frame_height,
        );
        add_run(
            runs,
            area.x + area.width - 1,
            area.y + row,
            "│",
            tags.0.clone(),
            frame_width,
            frame_height,
        );
    }
    if area.height > 1 {
        add_run(
            runs,
            area.x,
            area.y + area.height - 1,
            &format!("└{}┘", "─".repeat(area.width.saturating_sub(2))),
            tags.0.clone(),
            frame_width,
            frame_height,
        );
    }
}

fn render_toast(runs: &mut Vec<Run>, message: &str, width: usize, height: usize) {
    if width == 0 || height == 0 {
        return;
    }
    let toast_width = display_width(message).saturating_add(4).min(width);
    let toast_height = 3.min(height);
    let area = PopupArea {
        x: (width - toast_width) / 2,
        y: height.saturating_sub(toast_height + 2),
        width: toast_width,
        height: toast_height,
    };

    for row in 0..area.height {
        add_run(
            runs,
            area.x,
            area.y + row,
            &" ".repeat(area.width),
            StyleTag::Popup,
            width,
            height,
        );
    }
    draw_box_border(
        runs,
        area,
        None,
        TitleAlignment::Center,
        &(StyleTag::ToastBorder, StyleTag::ToastBorder),
        width,
        height,
    );
    if area.height > 1 {
        add_centered_run(
            runs,
            area.y + 1,
            message,
            StyleTag::Text,
            area.x + 1,
            area.width.saturating_sub(2),
            width,
            height,
        );
    }
}

fn render_performing_replacement(
    runs: &mut Vec<Run>,
    state: &PerformingReplacementState,
    width: usize,
    height: usize,
) {
    let completed = state.num_replacements_completed.load(Ordering::Relaxed);
    #[allow(clippy::cast_precision_loss)]
    let percentage = (completed as f64 / state.total_replacements.max(1) as f64) * 100.0;
    let lines = [
        ("Performing replacement...".to_string(), StyleTag::Text),
        (String::new(), StyleTag::Text),
        (
            format!(
                "Completed: {completed}/{} ({percentage:.2}%)",
                state.total_replacements
            ),
            StyleTag::Info,
        ),
        (
            format!(
                "Time: {}",
                format_duration(state.replacement_started.elapsed())
            ),
            StyleTag::Info,
        ),
    ];
    let start_y = height.saturating_sub(lines.len()) / 2;
    for (offset, (text, tag)) in lines.iter().enumerate() {
        add_centered_run(
            runs,
            start_y + offset,
            text,
            tag.clone(),
            0,
            width,
            width,
            height,
        );
    }
}

fn render_replacement_results(
    runs: &mut Vec<Run>,
    state: &ReplaceState,
    width: usize,
    height: usize,
) {
    let (x, content_width) = default_content_width(width);
    if state.errors.is_empty() {
        let start_y = height.saturating_sub(10) / 2;
        add_centered_run(
            runs,
            start_y,
            "Success!",
            StyleTag::DiffAdded,
            x,
            content_width,
            width,
            height,
        );
        render_results_tallies(runs, state, x, start_y + 1, content_width, width, height);
        return;
    }

    render_results_tallies(runs, state, x, 0, content_width, width, height);
    let list_title_y = 9;
    add_run(
        runs,
        x,
        list_title_y,
        "Errors:",
        StyleTag::Text,
        x + content_width,
        height,
    );

    let mut y = list_title_y + 1;
    for result in state.errors.iter().skip(state.replacement_errors_pos) {
        if y.saturating_add(2) >= height {
            break;
        }
        let (path, error) = result.display_error();
        add_run(
            runs,
            x,
            y + 1,
            &path,
            StyleTag::Text,
            x + content_width,
            height,
        );
        add_run(
            runs,
            x,
            y + 2,
            error,
            StyleTag::Error,
            x + content_width,
            height,
        );
        y += 3;
    }
}

fn render_results_tallies(
    runs: &mut Vec<Run>,
    state: &ReplaceState,
    x: usize,
    y: usize,
    width: usize,
    frame_width: usize,
    frame_height: usize,
) {
    for (index, (title, number)) in [
        ("Successful replacements (lines):", state.num_successes),
        ("Ignored (lines):", state.num_ignored),
        ("Errors:", state.errors.len()),
    ]
    .into_iter()
    .enumerate()
    {
        let area = PopupArea {
            x,
            y: y + index * 3,
            width,
            height: 3,
        };
        draw_box_border(
            runs,
            area,
            Some(title),
            TitleAlignment::Left,
            &(StyleTag::Text, StyleTag::Text),
            frame_width,
            frame_height,
        );
        add_run(
            runs,
            x + 1,
            area.y + 1,
            &number.to_string(),
            StyleTag::Text,
            x + width.saturating_sub(1),
            frame_height,
        );
    }
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
    let border_tag = if highlighted {
        StyleTag::Active
    } else {
        StyleTag::Text
    };

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
    border_tag: StyleTag,
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
            &border_tag,
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
    add_segment(
        runs,
        &mut title_x,
        y,
        "┌─",
        border_tag.clone(),
        end_x,
        frame_height,
    );
    add_title_segments(
        runs,
        &mut title_x,
        y,
        field,
        border_tag.clone(),
        end_x.saturating_sub(1),
        frame_height,
    );
    let trailing_border = format!("{}┐", "─".repeat(end_x.saturating_sub(title_x + 1)));
    add_segment(
        runs,
        &mut title_x,
        y,
        &trailing_border,
        border_tag.clone(),
        end_x,
        frame_height,
    );

    add_run(
        runs,
        x,
        y + 1,
        "│",
        border_tag.clone(),
        frame_width,
        frame_height,
    );
    let value = truncate(value, field_width.saturating_sub(2));
    add_run(
        runs,
        x + 1,
        y + 1,
        &value,
        StyleTag::Text,
        frame_width,
        frame_height,
    );
    add_run(
        runs,
        end_x - 1,
        y + 1,
        "│",
        border_tag.clone(),
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
    border_tag: StyleTag,
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
        border_tag.clone(),
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
    border_tag: StyleTag,
    frame_width: usize,
    frame_height: usize,
) {
    if box_width == 0 || y >= frame_height {
        return;
    }
    if box_width == 1 {
        render_narrow_box(
            runs,
            x,
            y,
            box_width,
            &border_tag,
            frame_width,
            frame_height,
        );
        return;
    }

    let end_x = x + box_width;
    add_run(
        runs,
        x,
        y,
        &format!("┌{}┐", "─".repeat(box_width.saturating_sub(2))),
        border_tag.clone(),
        frame_width,
        frame_height,
    );
    add_run(
        runs,
        x,
        y + 1,
        "│",
        border_tag.clone(),
        frame_width,
        frame_height,
    );
    add_run(
        runs,
        x + 1,
        y + 1,
        &truncate(contents, box_width.saturating_sub(2)),
        StyleTag::Text,
        frame_width,
        frame_height,
    );
    add_run(
        runs,
        end_x - 1,
        y + 1,
        "│",
        border_tag.clone(),
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
    border_tag: &StyleTag,
    frame_width: usize,
    frame_height: usize,
) {
    if box_width == 1 {
        for row in 0..FIELD_HEIGHT {
            add_run(
                runs,
                x,
                y + row,
                "│",
                border_tag.clone(),
                frame_width,
                frame_height,
            );
        }
    }
}

fn add_title_segments(
    runs: &mut Vec<Run>,
    x: &mut usize,
    y: usize,
    field: &SearchField,
    title_tag: StyleTag,
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
            StyleTag::Error,
            end_x,
            frame_height,
        );
    }
}

#[allow(clippy::too_many_arguments, clippy::too_many_lines)]
fn render_results(
    runs: &mut Vec<Run>,
    input_source: &InputSource,
    wrap_preview_text: bool,
    highlight_engine: &HighlightEngine,
    syntax_highlighting: bool,
    results_focussed: bool,
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

    let results_y = layout.banner_y.saturating_add(BANNER_HEIGHT);
    if results_y >= height || layout.width == 0 {
        return;
    }

    let results_height = height - results_y;
    let narrow = layout.width <= NARROW_RESULTS_WIDTH;
    let (
        list_x,
        list_y,
        list_width,
        list_height,
        preview_x,
        preview_y,
        preview_width,
        preview_height,
        num_to_render,
    ) = if narrow {
        let list_height = results_height.min(NARROW_LIST_HEIGHT);
        let preview_y = results_y
            .saturating_add(list_height)
            .saturating_add(usize::from(results_height > list_height));
        (
            layout.x,
            results_y,
            layout.width,
            list_height,
            layout.x,
            preview_y,
            layout.width,
            height.saturating_sub(preview_y),
            NARROW_LIST_HEIGHT,
        )
    } else {
        let content_width = layout.width.saturating_sub(1);
        let list_width = content_width * 2 / 5;
        let preview_x = layout.x + list_width + 1;
        (
            layout.x,
            results_y,
            list_width,
            results_height,
            preview_x,
            results_y,
            layout
                .x
                .saturating_add(layout.width)
                .saturating_sub(preview_x),
            results_height,
            results_height,
        )
    };

    search_state.num_displayed = Some(num_to_render);
    clamp_result_offset(search_state, num_to_render);

    let base_path = match input_source {
        InputSource::Directory(directory) => directory.as_path(),
        InputSource::Stdin(_) => Path::new("."),
    };
    for (index, result) in search_state
        .results
        .iter()
        .enumerate()
        .skip(search_state.view_offset)
        .take(list_height)
    {
        render_result_row(
            runs,
            result,
            base_path,
            index,
            results_focussed,
            search_state.is_selected(index),
            search_state.is_primary_selected(index),
            list_x,
            list_y + index.saturating_sub(search_state.view_offset),
            list_width,
            height,
        );
    }

    if preview_width == 0 || preview_height == 0 {
        return;
    }
    // This deliberately uses the editor background rather than the popup
    // surface. Context, scopes, and diff text inherit this fill.
    for row in 0..preview_height {
        add_run(
            runs,
            preview_x,
            preview_y + row,
            &" ".repeat(preview_width),
            StyleTag::Preview,
            preview_x + preview_width,
            height,
        );
    }
    if let Some(selected) = search_state
        .results
        .get(search_state.primary_selected_pos())
    {
        render_preview(
            runs,
            input_source,
            selected,
            wrap_preview_text,
            highlight_engine,
            syntax_highlighting,
            preview_x,
            preview_y,
            preview_width,
            preview_height,
            height,
        );
    }

    debug_assert!(list_x + list_width <= width);
    debug_assert!(preview_x + preview_width <= width);
}

#[allow(clippy::too_many_arguments)]
fn render_result_row(
    runs: &mut Vec<Run>,
    result: &SearchResultWithReplacement,
    base_path: &Path,
    index: usize,
    results_focussed: bool,
    selected: bool,
    primary_selected: bool,
    x: usize,
    y: usize,
    row_width: usize,
    frame_height: usize,
) {
    let row_tag = result_selection_tag(result, results_focussed, selected, primary_selected);
    let marker = if result.search_result.included {
        "[x] "
    } else {
        "[ ] "
    };
    let path = result.search_result.path.as_ref().map_or_else(
        || "stdin".to_string(),
        |path| relative_path(base_path, path),
    );
    let line_number = format!(":{}", result.search_result.start_line_number());
    let index_text = format!(" ({})", index + 1);
    let index_width = display_width(&index_text);
    let row_end_x = x + row_width;
    let left_end_x = row_end_x.saturating_sub(index_width);
    let path_space =
        left_end_x.saturating_sub(x + display_width(marker) + display_width(&line_number));
    let path = truncate_path_from_start(&path, path_space);
    let mut row_x = x;
    let (marker_tag, path_tag, accessory_tag) = row_tag.clone().map_or(
        (StyleTag::Info, StyleTag::Text, StyleTag::Info),
        |selection_tag| (selection_tag.clone(), selection_tag.clone(), selection_tag),
    );

    if let Some(selection_tag) = row_tag {
        // The TUI paints the selected row's background from edge to edge.
        // Render this first so the following text segments remain readable
        // while the empty spacer keeps the selection visually continuous.
        add_run(
            runs,
            x,
            y,
            &" ".repeat(row_width),
            selection_tag,
            row_end_x,
            frame_height,
        );
    }

    add_segment(
        runs,
        &mut row_x,
        y,
        marker,
        marker_tag,
        left_end_x,
        frame_height,
    );
    add_segment(
        runs,
        &mut row_x,
        y,
        &path,
        path_tag,
        left_end_x,
        frame_height,
    );
    add_segment(
        runs,
        &mut row_x,
        y,
        &line_number,
        accessory_tag.clone(),
        left_end_x,
        frame_height,
    );
    add_run(
        runs,
        row_end_x.saturating_sub(index_width),
        y,
        &index_text,
        accessory_tag,
        row_end_x,
        frame_height,
    );
}

fn result_selection_tag(
    result: &SearchResultWithReplacement,
    results_focussed: bool,
    selected: bool,
    primary_selected: bool,
) -> Option<StyleTag> {
    if !results_focussed || !selected {
        return None;
    }

    Some(match (primary_selected, result.search_result.included) {
        (true, true) => StyleTag::Selection,
        (false, true) => StyleTag::SelectionSecondary,
        (true, false) => StyleTag::SelectionExcluded,
        (false, false) => StyleTag::SelectionSecondaryExcluded,
    })
}

fn truncate_path_from_start(path: &str, max_width: usize) -> String {
    if display_width(path) <= max_width {
        return path.to_string();
    }
    if max_width == 0 {
        return String::new();
    }
    if max_width == 1 {
        return "…".to_string();
    }

    let mut width = 1;
    let mut tail = Vec::new();
    for character in path.chars().rev() {
        let character_width = UnicodeWidthChar::width(character).unwrap_or(0);
        if width + character_width > max_width {
            break;
        }
        width += character_width;
        tail.push(character);
    }
    tail.reverse();
    format!("…{}", tail.into_iter().collect::<String>())
}

#[allow(clippy::too_many_arguments)]
fn render_preview(
    runs: &mut Vec<Run>,
    input_source: &InputSource,
    result: &SearchResultWithReplacement,
    wrap_text: bool,
    highlight_engine: &HighlightEngine,
    syntax_highlighting: bool,
    x: usize,
    y: usize,
    width: usize,
    height: usize,
    frame_height: usize,
) {
    if let Some(error) = &result.preview_error {
        add_run(
            runs,
            x,
            y,
            &format!("Error generating preview: {error}"),
            StyleTag::Error,
            x + width,
            frame_height,
        );
        return;
    }

    let preview = match build_preview_sections(
        input_source,
        result,
        height,
        highlight_engine,
        syntax_highlighting,
    ) {
        Ok(preview) => preview,
        Err(error) => {
            add_run(
                runs,
                x,
                y,
                &format!("Error generating preview: {error}"),
                StyleTag::Error,
                x + width,
                frame_height,
            );
            return;
        }
    };
    let lines = fit_preview_lines(preview, width, height, wrap_text);
    for (line_offset, line) in lines.into_iter().enumerate() {
        let mut line_x = x;
        for segment in line.segments {
            add_segment(
                runs,
                &mut line_x,
                y + line_offset,
                &segment.text,
                segment.tag,
                x + width,
                frame_height,
            );
        }
    }
}

fn build_preview_sections(
    input_source: &InputSource,
    result: &SearchResultWithReplacement,
    preview_height: usize,
    highlight_engine: &HighlightEngine,
    syntax_highlighting: bool,
) -> Result<PreviewSections, String> {
    let diff = diff_lines(result);
    let context_height = preview_height
        .saturating_sub(diff.len().saturating_sub(1))
        .max(1);
    let line_index = result.search_result.start_line_number().saturating_sub(1);
    let preview_read = read_preview_window(
        input_source,
        result,
        line_index,
        preview_height,
        highlight_engine,
        syntax_highlighting,
    )?;
    let selected_position = preview_read.lines
        .iter()
        .position(|line| line.number == line_index)
        .ok_or_else(|| "File content has changed".to_string())?;
    let expected = expected_first_line_content(result);
    if preview_read.lines[selected_position].text != expected {
        return Err("File content has changed".to_string());
    }

    let (before, after) = centered_context_lines(preview_read.lines, selected_position, context_height);
    let end_line_index = result.search_result.end_line_number().saturating_sub(1);
    Ok(PreviewSections {
        before: context_preview_lines(&before, preview_read.spans.as_deref()),
        diff,
        after: context_preview_lines(
            &after
                .into_iter()
                .filter(|line| line.number > end_line_index)
                .collect::<ContextLines>(),
            preview_read.spans.as_deref(),
        ),
    })
}

fn read_preview_window(
    input_source: &InputSource,
    result: &SearchResultWithReplacement,
    line_index: usize,
    preview_height: usize,
    highlight_engine: &HighlightEngine,
    syntax_highlighting: bool,
) -> Result<PreviewRead, String> {
    let start = line_index.saturating_sub(preview_height);
    let end = line_index.saturating_add(preview_height);
    match input_source {
        InputSource::Directory(_) => {
            let path = result
                .search_result
                .path
                .as_deref()
                .ok_or_else(|| "Missing file path for preview".to_string())?;
            if syntax_highlighting {
                // Read once: this supplies both the visible context and the
                // complete source Tree-sitter needs for correct parsing. A
                // non-UTF-8 read intentionally falls through to the core's
                // lossy window reader, matching the TUI's plain preview.
                if let Ok(Some(content)) = highlight_engine.read_preview_content(path) {
                    let spans = highlight_engine.highlight_preview_content(path, &content);
                    let lines = indexed_lines_in_window(&content, start, end);
                    return Ok(PreviewRead { lines, spans });
                }
            }
            let lines = read_lines_range(path, start, end)
                .map_err(|error| error.to_string())?
                .map(|(number, text)| IndexedLine {
                    number,
                    text,
                    byte_offset: None,
                })
                .collect();
            Ok(PreviewRead { lines, spans: None })
        }
        InputSource::Stdin(stdin) => Ok(PreviewRead {
            lines: stdin.lines().enumerate().skip(start)
                .take(end.saturating_sub(start).saturating_add(1))
                .map(|(number, text)| IndexedLine {
                    number,
                    text: text.to_string(),
                    byte_offset: None,
                })
                .collect(),
            spans: None,
        }),
    }
}

fn indexed_lines_in_window(content: &str, start: usize, end: usize) -> ContextLines {
    let mut byte_offset = 0usize;
    let mut lines = Vec::new();
    for (number, raw_line) in content.split_inclusive('\n').enumerate() {
        if number > end {
            break;
        }
        let offset = byte_offset;
        byte_offset = byte_offset.saturating_add(raw_line.len());
        if number < start {
            continue;
        }
        let without_newline = raw_line.strip_suffix('\n').unwrap_or(raw_line);
        let text = without_newline.strip_suffix('\r').unwrap_or(without_newline);
        lines.push(IndexedLine {
            number,
            text: text.to_string(),
            byte_offset: Some(offset),
        });
    }
    lines
}

fn centered_context_lines(
    indexed_lines: ContextLines,
    selected_position: usize,
    max_lines: usize,
) -> (ContextLines, ContextLines) {
    let mut start = selected_position;
    let mut end = selected_position;
    let mut count = 1;
    while count < max_lines && (start > 0 || end + 1 < indexed_lines.len()) {
        if end + 1 < indexed_lines.len() {
            end += 1;
            count += 1;
        }
        if count < max_lines && start > 0 {
            start -= 1;
            count += 1;
        }
    }

    let mut window = indexed_lines
        .into_iter()
        .skip(start)
        .take(end.saturating_sub(start).saturating_add(1))
        .collect::<Vec<_>>();
    let selected_in_window = selected_position.saturating_sub(start);
    let after = window.split_off(selected_in_window.saturating_add(1));
    let _selected = window.pop();
    (window, after)
}

fn expected_first_line_content(result: &SearchResultWithReplacement) -> &str {
    match &result.search_result.content {
        MatchContent::Line { content, .. } => content,
        MatchContent::ByteRange { lines, .. } => &lines[0].1.content,
    }
}

fn context_preview_lines(lines: &[IndexedLine], spans: Option<&[HighlightSpan]>) -> Vec<PreviewLine> {
    let Some(spans) = spans else {
        return lines
            .iter()
            .map(|line| context_preview_line(line, None))
            .collect();
    };
    let first_offset = lines.first().and_then(|line| line.byte_offset).unwrap_or(0);
    // A span ending exactly at a line start belongs to the previous line, not
    // this one. Starting here and advancing one cursor makes the whole
    // visible window linear in spans instead of rescanning the full file per
    // line.
    let mut cursor = spans.partition_point(|span| span.byte_range.end <= first_offset);
    lines
        .iter()
        .map(|line| {
            let Some(line_start) = line.byte_offset else {
                return context_preview_line(line, None);
            };
            let line_end = line_start.saturating_add(line.text.len());
            while cursor < spans.len() && spans[cursor].byte_range.end <= line_start {
                cursor += 1;
            }
            let mut end = cursor;
            while end < spans.len() && spans[end].byte_range.start < line_end {
                end += 1;
            }
            context_preview_line(line, Some(&spans[cursor..end]))
        })
        .collect()
}

fn context_preview_line(source: &IndexedLine, spans: Option<&[HighlightSpan]>) -> PreviewLine {
    let mut preview_line = PreviewLine::default();
    push_preview_segment(&mut preview_line, "  ", StyleTag::Text);
    let Some(byte_offset) = source.byte_offset else {
        return plain_context_preview_line(source);
    };
    let Some(spans) = spans else {
        return plain_context_preview_line(source);
    };
    let line_end = byte_offset.saturating_add(source.text.len());
    let mut position = byte_offset;
    for span in spans {
        let start = span.byte_range.start.max(byte_offset);
        let end = span.byte_range.end.min(line_end);
        if start >= end || start < position {
            continue;
        }
        let Some(plain) = source.text.get(position.saturating_sub(byte_offset)..start.saturating_sub(byte_offset))
        else {
            return plain_context_preview_line(source);
        };
        let Some(highlighted) = source.text.get(start.saturating_sub(byte_offset)..end.saturating_sub(byte_offset))
        else {
            return plain_context_preview_line(source);
        };
        push_preview_segment(
            &mut preview_line,
            plain,
            StyleTag::Text,
        );
        push_preview_segment(
            &mut preview_line,
            highlighted,
            StyleTag::Scope(Arc::clone(&span.scope)),
        );
        position = end;
    }
    let Some(tail) = source.text.get(position.saturating_sub(byte_offset)..) else {
        return plain_context_preview_line(source);
    };
    push_preview_segment(
        &mut preview_line,
        tail,
        StyleTag::Text,
    );
    preview_line
}

fn plain_context_preview_line(source: &IndexedLine) -> PreviewLine {
    let mut preview_line = PreviewLine::default();
    push_preview_segment(&mut preview_line, "  ", StyleTag::Text);
    push_preview_segment(&mut preview_line, &source.text, StyleTag::Text);
    preview_line
}

fn diff_lines(result: &SearchResultWithReplacement) -> Vec<PreviewLine> {
    match &result.search_result.content {
        MatchContent::Line { content, .. } => {
            let (old, new) = line_diff(content, &result.replacement);
            let old = old
                .iter()
                .map(|diff| {
                    (
                        diff.text.as_str(),
                        if diff.bg_colour.is_some() {
                            StyleTag::DiffRemovedEmph
                        } else {
                            StyleTag::DiffRemoved
                        },
                    )
                })
                .collect();
            let new = new
                .iter()
                .map(|diff| {
                    (
                        diff.text.as_str(),
                        if diff.bg_colour.is_some() {
                            StyleTag::DiffAddedEmph
                        } else {
                            StyleTag::DiffAdded
                        },
                    )
                })
                .collect();
            diff_lines_from_segments("- ", &StyleTag::DiffRemoved, old)
                .into_iter()
                .chain(diff_lines_from_segments("+ ", &StyleTag::DiffAdded, new))
                .collect()
        }
        MatchContent::ByteRange {
            lines,
            match_start_in_first_line,
            match_end_in_last_line,
            content,
            ..
        } if content.len() + result.replacement.len() <= MULTILINE_DETAILED_DIFF_MAX_BYTES => {
            detailed_multiline_diff(
                lines,
                *match_start_in_first_line,
                *match_end_in_last_line,
                &result.replacement,
            )
        }
        MatchContent::ByteRange {
            lines,
            match_start_in_first_line,
            match_end_in_last_line,
            ..
        } => simple_multiline_diff(
            lines,
            *match_start_in_first_line,
            *match_end_in_last_line,
            &result.replacement,
        ),
    }
}

fn detailed_multiline_diff(
    lines: &[(usize, scooter_core::search::Line)],
    match_start_in_first_line: usize,
    match_end_in_last_line: usize,
    replacement: &str,
) -> Vec<PreviewLine> {
    let mut result = Vec::new();
    for (index, (_, line)) in lines.iter().enumerate() {
        let start = if index == 0 {
            match_start_in_first_line
        } else {
            0
        };
        let end = if index + 1 == lines.len() {
            match_end_in_last_line
        } else {
            line.content.len()
        };
        result.extend(diff_lines_from_segments(
            "- ",
            &StyleTag::DiffRemoved,
            vec![
                (&line.content[..start], StyleTag::DiffRemoved),
                (&line.content[start..end], StyleTag::DiffRemovedEmph),
                (&line.content[end..], StyleTag::DiffRemoved),
            ],
        ));
    }

    let first = &lines[0].1.content;
    let last = &lines[lines.len() - 1].1.content;
    result.extend(diff_lines_from_segments(
        "+ ",
        &StyleTag::DiffAdded,
        vec![
            (&first[..match_start_in_first_line], StyleTag::DiffAdded),
            (replacement, StyleTag::DiffAddedEmph),
            (&last[match_end_in_last_line..], StyleTag::DiffAdded),
        ],
    ));
    result
}

fn simple_multiline_diff(
    lines: &[(usize, scooter_core::search::Line)],
    match_start_in_first_line: usize,
    match_end_in_last_line: usize,
    replacement: &str,
) -> Vec<PreviewLine> {
    let mut result = lines
        .iter()
        .flat_map(|(_, line)| {
            diff_lines_from_segments(
                "- ",
                &StyleTag::DiffRemoved,
                vec![(&line.content, StyleTag::DiffRemoved)],
            )
        })
        .collect::<Vec<_>>();
    let first = &lines[0].1.content;
    let last = &lines[lines.len() - 1].1.content;
    result.extend(diff_lines_from_segments(
        "+ ",
        &StyleTag::DiffAdded,
        vec![
            (&first[..match_start_in_first_line], StyleTag::DiffAdded),
            (replacement, StyleTag::DiffAdded),
            (&last[match_end_in_last_line..], StyleTag::DiffAdded),
        ],
    ));
    result
}

fn diff_lines_from_segments(
    prefix: &str,
    prefix_tag: &StyleTag,
    segments: Vec<(&str, StyleTag)>,
) -> Vec<PreviewLine> {
    let mut lines = Vec::new();
    let mut current = PreviewLine::default();
    push_preview_segment(&mut current, prefix, prefix_tag.clone());
    for (text, tag) in segments {
        for part in text.split_inclusive('\n') {
            let ends_line = part.ends_with('\n');
            let part = part.strip_suffix('\n').unwrap_or(part);
            push_preview_segment(&mut current, part, tag.clone());
            if ends_line {
                lines.push(current);
                current = PreviewLine::default();
                push_preview_segment(&mut current, prefix, prefix_tag.clone());
            }
        }
    }
    lines.push(current);
    lines
}

fn push_preview_segment(line: &mut PreviewLine, text: &str, tag: StyleTag) {
    let text = strip_control_chars(text);
    if text.is_empty() {
        return;
    }
    if let Some(previous) = line.segments.last_mut()
        && previous.tag == tag
    {
        previous.text.push_str(&text);
        return;
    }
    line.segments.push(PreviewSegment {
        text: text.into_owned(),
        tag,
    });
}

fn fit_preview_lines(
    preview: PreviewSections,
    width: usize,
    height: usize,
    wrap_text: bool,
) -> Vec<PreviewLine> {
    if width == 0 || height == 0 {
        return Vec::new();
    }
    let transform = |lines: Vec<PreviewLine>| -> Vec<PreviewLine> {
        if wrap_text {
            lines
                .iter()
                .flat_map(|line| wrap_preview_line(line, width))
                .collect()
        } else {
            lines
                .iter()
                .map(|line| truncate_preview_line(line, width))
                .collect()
        }
    };
    let before = transform(preview.before);
    let diff = transform(preview.diff);
    let after = transform(preview.after);
    let focus = before.len() + diff.len().saturating_sub(1) / 2;
    let lines = before
        .into_iter()
        .chain(diff)
        .chain(after)
        .collect::<Vec<_>>();
    if lines.len() <= height {
        return lines;
    }

    let start = focus.saturating_sub(height / 2).min(lines.len() - height);
    lines.into_iter().skip(start).take(height).collect()
}

fn truncate_preview_line(line: &PreviewLine, width: usize) -> PreviewLine {
    let mut truncated = PreviewLine::default();
    let mut used = 0;
    for segment in &line.segments {
        let mut text = String::new();
        for character in segment.text.chars() {
            let character_width = UnicodeWidthChar::width(character).unwrap_or(0);
            if used + character_width > width {
                break;
            }
            used += character_width;
            text.push(character);
        }
        push_preview_segment(&mut truncated, &text, segment.tag.clone());
        if used >= width {
            break;
        }
    }
    truncated
}

fn wrap_preview_line(line: &PreviewLine, width: usize) -> Vec<PreviewLine> {
    if width <= display_width(WRAPPED_LINE_PREFIX) {
        return Vec::new();
    }

    let mut wrapped = Vec::new();
    let mut current = PreviewLine::default();
    let mut used = 0;
    for segment in &line.segments {
        for character in segment.text.chars() {
            let character_width = UnicodeWidthChar::width(character).unwrap_or(0);
            if used + character_width > width && used > 0 {
                wrapped.push(current);
                (current, used) = wrapped_preview_continuation();
            }
            if used + character_width > width {
                continue;
            }
            push_preview_segment(&mut current, &character.to_string(), segment.tag.clone());
            used += character_width;
        }
    }
    if !current.segments.is_empty() || line.segments.is_empty() {
        wrapped.push(current);
    }
    wrapped
}

fn wrapped_preview_continuation() -> (PreviewLine, usize) {
    let mut line = PreviewLine::default();
    let prefix_width = display_width(WRAPPED_LINE_PREFIX);

    push_preview_segment(&mut line, WRAPPED_LINE_PREFIX, StyleTag::Dim);
    (line, prefix_width)
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

#[cfg(test)]
mod tests {
    use std::{
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
        app::{InputSource, Popup, Screen, SearchPhase},
        errors::AppError,
        line_reader::LineEnding,
        replace::{PerformingReplacementState, ReplaceResult, ReplaceState},
        search::{ByteRangeParams, Line, SearchResult, SearchResultWithReplacement},
    };
    use tempfile::tempdir;
    use tokio::sync::mpsc;

    use crate::{engine::ScooterEngine, highlight::HighlightEngine};

    use super::{
        Frame, HighlightSpan, IndexedLine, PopupLine, StyleTag, context_preview_line, context_preview_lines,
        diff_lines, display_width, read_preview_window, render_paragraph_popup, render_results_tallies, truncate,
    };

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
    fn one_column_fields_keep_the_reachable_narrow_box_path() {
        let fixture = tempdir().expect("fixture directory");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        let frame = engine.render(1, 4);
        assert!(frame.runs.iter().any(|run| run.text == "│"));
    }

    #[test]
    fn detailed_multiline_preview_marks_only_the_replaced_regions() {
        let result = SearchResultWithReplacement {
            search_result: SearchResult::new_byte_range(ByteRangeParams {
                path: None,
                lines: vec![
                    (
                        3,
                        Line {
                            content: "prefix old".to_string(),
                            line_ending: LineEnding::Lf,
                        },
                    ),
                    (
                        4,
                        Line {
                            content: "second suffix".to_string(),
                            line_ending: LineEnding::Lf,
                        },
                    ),
                ],
                match_start_in_first_line: 7,
                match_end_in_last_line: 6,
                byte_start: 0,
                byte_end: 17,
                content: "old\nsecond".to_string(),
                included: true,
            }),
            replacement: "new\nreplacement".to_string(),
            replace_result: None,
            preview_error: None,
        };

        let preview = diff_lines(&result);
        let lines = preview
            .iter()
            .map(|line| {
                line.segments
                    .iter()
                    .map(|segment| segment.text.as_str())
                    .collect::<String>()
            })
            .collect::<Vec<_>>();
        assert_eq!(
            lines,
            vec![
                "- prefix old".to_string(),
                "- second suffix".to_string(),
                "+ prefix new".to_string(),
                "+ replacement suffix".to_string(),
            ]
        );
        assert!(preview.iter().any(|line| {
            line.segments
                .iter()
                .any(|segment| segment.tag == StyleTag::DiffRemovedEmph && segment.text == "old")
        }));
        assert!(preview.iter().any(|line| {
            line.segments
                .iter()
                .any(|segment| segment.tag == StyleTag::DiffAddedEmph && segment.text == "new")
        }));
    }

    #[test]
    fn context_preview_lines_use_a_two_space_prefix_without_line_numbers() {
        let line = context_preview_line(
            &IndexedLine {
                number: 41,
                text: "context text".to_string(),
                byte_offset: None,
            },
            None,
        );
        assert_eq!(
            line.segments
                .iter()
                .map(|segment| segment.text.as_str())
                .collect::<String>(),
            "  context text"
        );
        assert!(
            line.segments
                .iter()
                .all(|segment| segment.tag == StyleTag::Text)
        );
    }

    #[test]
    fn malformed_highlight_offsets_render_the_entire_context_line_plain() {
        let line = context_preview_line(
            &IndexedLine {
                number: 0,
                text: "éclair".to_string(),
                byte_offset: Some(0),
            },
            Some(&[HighlightSpan {
                byte_range: 1..2,
                scope: Arc::from("keyword"),
            }]),
        );
        assert_eq!(
            line.segments.iter().map(|segment| segment.text.as_str()).collect::<String>(),
            "  éclair"
        );
        assert!(line.segments.iter().all(|segment| segment.tag == StyleTag::Text));
    }

    #[test]
    fn context_span_cursor_respects_line_boundaries_and_zero_length_spans() {
        let lines = vec![
            IndexedLine { number: 0, text: "a".to_string(), byte_offset: Some(0) },
            IndexedLine { number: 1, text: "b".to_string(), byte_offset: Some(2) },
            IndexedLine { number: 2, text: "c".to_string(), byte_offset: Some(4) },
        ];
        let scope = Arc::from("keyword");
        let previews = context_preview_lines(
            &lines,
            Some(&[
                HighlightSpan { byte_range: 0..0, scope: Arc::clone(&scope) },
                HighlightSpan { byte_range: 0..2, scope: Arc::clone(&scope) },
                HighlightSpan { byte_range: 1..5, scope: Arc::clone(&scope) },
            ]),
        );
        assert!(previews[0].segments.iter().any(|segment| segment.tag == StyleTag::Scope(Arc::clone(&scope))));
        assert!(previews[1].segments.iter().any(|segment| segment.tag == StyleTag::Scope(Arc::clone(&scope))));
        assert!(previews[2].segments.iter().any(|segment| segment.tag == StyleTag::Scope(Arc::clone(&scope))));
    }

    #[test]
    fn non_utf8_preview_falls_back_to_the_core_lossy_plain_window() {
        let fixture = tempdir().expect("fixture directory");
        let path = fixture.path().join("invalid.rs");
        fs::write(
            &path,
            b"before context\ninvalid \xff alpha target\nafter context\n",
        )
        .expect("write non-UTF-8 fixture");
        let result = SearchResultWithReplacement {
            search_result: SearchResult::new_line(
                Some(path),
                2,
                "invalid � alpha target".to_string(),
                LineEnding::Lf,
                true,
            ),
            replacement: String::new(),
            replace_result: None,
            preview_error: None,
        };
        let preview = read_preview_window(
            &InputSource::Directory(fixture.path().to_path_buf()),
            &result,
            1,
            2,
            &HighlightEngine::new(None),
            true,
        )
        .expect("plain fallback succeeds");
        assert_eq!(preview.lines[1].text, "invalid � alpha target");
        assert!(preview.spans.is_none());
    }

    #[test]
    fn popup_titles_are_centred_and_content_keeps_one_cell_horizontal_padding() {
        let mut runs = Vec::new();
        render_paragraph_popup(
            &mut runs,
            "Notice",
            &[PopupLine {
                text: "body".to_string(),
                tag: StyleTag::Text,
            }],
            100,
            40,
        );

        let title = runs
            .iter()
            .find(|run| run.text == "Notice")
            .expect("popup title");
        assert_eq!((title.x, title.y), (46, 18));
        assert_eq!(title.tag, StyleTag::Popup);
        assert!(runs.iter().any(|run| {
            run.x == 7
                && run.y == 18
                && run.text.starts_with('┌')
                && run.tag == StyleTag::PopupBorder
        }));
        let body = runs
            .iter()
            .find(|run| run.text == "body")
            .expect("popup body");
        assert_eq!((body.x, body.y), (9, 19));
    }

    #[test]
    fn replacement_tally_titles_are_left_aligned_inside_their_borders() {
        let state = ReplaceState {
            num_successes: 3,
            num_ignored: 1,
            errors: vec![],
            replacement_errors_pos: 0,
        };
        let mut runs = Vec::new();
        render_results_tallies(&mut runs, &state, 5, 4, 50, 60, 20);

        let title = runs
            .iter()
            .find(|run| run.text == "Successful replacements (lines):")
            .expect("success tally title");
        assert_eq!((title.x, title.y), (6, 4));
    }

    #[test]
    fn renders_results_and_preview_within_bounds_at_every_supported_size() {
        let fixture = tempdir().expect("fixture directory");
        fs::write(fixture.path().join("matches.txt"), "alpha\nalphabet\n").expect("write fixture");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        for character in "alpha".chars() {
            engine.handle_key(&character.to_string(), 0);
        }
        wait_until_complete(&mut engine);

        let preview = engine.render(160, 45);
        assert!(
            preview
                .runs
                .iter()
                .any(|run| run.tag == StyleTag::DiffRemoved)
        );
        assert!(
            preview
                .runs
                .iter()
                .any(|run| run.tag == StyleTag::DiffAdded)
        );

        assert_all_sizes_are_well_formed(&mut engine);
    }

    #[test]
    fn overlays_and_replacement_screens_stay_inside_the_render_grid() {
        let fixture = tempdir().expect("fixture directory");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");

        assert_eq!(engine.handle_key("h", 2), "rerender");
        assert!(
            engine
                .render(100, 36)
                .runs
                .iter()
                .any(|run| run.tag == StyleTag::Popup)
        );
        assert_all_sizes_are_well_formed(&mut engine);
        assert_eq!(engine.handle_key("esc", 0), "rerender");

        engine.app.add_error(AppError {
            name: "Search error".to_string(),
            long: "first detail\nsecond detail".to_string(),
        });
        assert_all_sizes_are_well_formed(&mut engine);
        assert_eq!(engine.handle_key("esc", 0), "rerender");

        engine.app.ui_state.popup = Some(Popup::Text {
            title: "Notice".to_string(),
            body: "first line\nsecond line".to_string(),
        });
        assert_all_sizes_are_well_formed(&mut engine);
        assert_eq!(engine.handle_key("esc", 0), "rerender");

        assert_eq!(engine.handle_key("m", 4), "rerender");
        assert!(
            engine
                .render(100, 36)
                .runs
                .iter()
                .any(|run| run.text.contains("Multiline: ON"))
        );
        assert_all_sizes_are_well_formed(&mut engine);
        wait_until_toast_dismissed(&mut engine);

        let (_sender, receiver) = mpsc::unbounded_channel();
        engine.app.ui_state.current_screen =
            Screen::PerformingReplacement(PerformingReplacementState::new(
                receiver,
                Arc::new(AtomicBool::new(false)),
                Arc::new(AtomicUsize::new(1)),
                2,
            ));
        assert_all_sizes_are_well_formed(&mut engine);

        engine.app.ui_state.current_screen = Screen::Results(ReplaceState {
            num_successes: 3,
            num_ignored: 1,
            errors: vec![],
            replacement_errors_pos: 0,
        });
        assert_all_sizes_are_well_formed(&mut engine);

        engine.app.ui_state.current_screen = Screen::Results(ReplaceState {
            num_successes: 3,
            num_ignored: 1,
            errors: vec![error_result("failed.txt", 7, "permission denied")],
            replacement_errors_pos: 0,
        });
        assert_all_sizes_are_well_formed(&mut engine);
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

    fn wait_until_toast_dismissed(engine: &mut ScooterEngine) {
        let deadline = Instant::now() + Duration::from_secs(3);
        while Instant::now() < deadline {
            engine.pump();
            if engine.app.toast_message().is_none() {
                return;
            }
            thread::sleep(Duration::from_millis(10));
        }
        panic!("toast did not dismiss");
    }

    fn assert_all_sizes_are_well_formed(engine: &mut ScooterEngine) {
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

    fn assert_frame_is_well_formed(frame: &Frame, width: usize, height: usize) {
        for run in &frame.runs {
            assert!(
                run.y < height,
                "run exceeds height: {run:?} at {width}x{height}"
            );
            assert!(
                run.x + display_width(&run.text) <= width,
                "run exceeds width: {run:?} at {width}x{height}"
            );
        }
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

    fn box_top_for_test(title: &str, width: usize) -> String {
        let mut result = String::from("┌─");
        result.push_str(&truncate(title, width.saturating_sub(3)));
        result.push_str(&"─".repeat(width.saturating_sub(display_width(&result) + 1)));
        result.push('┐');
        result
    }
}
