//! Native frame model for Scooter's search-fields screen.

use std::{cmp::min, path::Path, sync::atomic::Ordering, time::Duration};

use scooter_core::{
    app::{App, FocussedSection, InputSource, Popup, Screen, SearchPhase, SearchState},
    diff::line_diff,
    fields::{Field, NUM_SEARCH_FIELDS, SearchField},
    replace::{PerformingReplacementState, ReplaceState},
    search::{MatchContent, SearchResultWithReplacement},
    utils::{read_lines_range, relative_path, strip_control_chars},
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
const NARROW_RESULTS_WIDTH: usize = 110;
const NARROW_LIST_HEIGHT: usize = 5;
const MULTILINE_DETAILED_DIFF_MAX_BYTES: usize = 20_000;
const WRAPPED_LINE_PREFIX: &str = "↪ ";

#[derive(Debug, Clone, Copy)]
struct PopupArea {
    x: usize,
    y: usize,
    width: usize,
    height: usize,
}

#[derive(Debug, Clone, Copy)]
enum TitleAlignment {
    Center,
    Left,
}

#[derive(Debug, Clone)]
struct PopupLine {
    text: String,
    tag: &'static str,
}

#[derive(Debug, Clone)]
struct PreviewSegment {
    text: String,
    tag: &'static str,
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

type IndexedLine = (usize, String);
type ContextLines = Vec<IndexedLine>;

/// Render Scooter's active screen plus its footer and transient overlays.
pub(crate) fn render(app: &mut App, width: usize, height: usize) -> Frame {
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
            let layout = fields_layout(width, content_height, requested_count);

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
                        "error",
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
                        "error",
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
        render_popup(&mut frame.runs, app, popup, width, content_height);
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
        fields_layout(width, content_height, requested_count),
        content_height,
    )
}

fn render_footer(runs: &mut Vec<Run>, app: &App, width: usize, y: usize) {
    let hints = app
        .keymaps_compact()
        .into_iter()
        .map(|(key, action)| format!("{key} {action}"))
        .collect::<Vec<_>>()
        .join(" / ");
    add_centered_run(runs, y, &hints, "info", 0, width, width, y + 1);
}

fn render_popup(runs: &mut Vec<Run>, app: &App, popup: &Popup, width: usize, height: usize) {
    match popup {
        Popup::Error => {
            let errors = app.errors();
            let mut lines = Vec::new();
            for (index, error) in errors.iter().enumerate() {
                lines.push(PopupLine {
                    text: error.name.clone(),
                    tag: "active",
                });
                lines.extend(error.long.split('\n').map(|text| PopupLine {
                    text: text.to_string(),
                    tag: "error",
                }));
                if index + 1 < errors.len() {
                    lines.push(PopupLine {
                        text: String::new(),
                        tag: "text",
                    });
                }
            }
            render_paragraph_popup(runs, "Errors", &lines, width, height);
        }
        Popup::Help => render_help_popup(runs, &app.keymaps_all(), width, height),
        Popup::Text { title, body } => {
            let lines = body
                .split('\n')
                .map(|text| PopupLine {
                    text: text.to_string(),
                    tag: "text",
                })
                .collect::<Vec<_>>();
            render_paragraph_popup(runs, title, &lines, width, height);
        }
    }
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
            line.tag,
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
            "info",
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
                "text",
                x + content_width,
                height,
            );
        }
    }
}

fn popup_area(width: usize, height: usize, content_height: usize) -> PopupArea {
    if width == 0 || height == 0 {
        return PopupArea {
            x: 0,
            y: 0,
            width: 0,
            height: 0,
        };
    }
    let popup_width = (width * 85 / 100).clamp(1, width.min(125));
    let max_height = (height * 80 / 100).clamp(1, height);
    let popup_height = content_height.saturating_add(2).max(1).min(max_height);
    PopupArea {
        x: width.saturating_sub(popup_width) / 2,
        y: height.saturating_sub(popup_height) / 2,
        width: popup_width,
        height: popup_height,
    }
}

fn popup_inner(area: PopupArea) -> (usize, usize, usize, usize) {
    (
        area.x.saturating_add(2).min(area.x + area.width),
        area.y.saturating_add(1),
        area.width.saturating_sub(4),
        area.height.saturating_sub(2),
    )
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
            "popup",
            frame_width,
            frame_height,
        );
    }
    draw_box_border(
        runs,
        area,
        Some(title),
        TitleAlignment::Center,
        ("popup-border", "popup"),
        frame_width,
        frame_height,
    );
}

fn draw_box_border(
    runs: &mut Vec<Run>,
    area: PopupArea,
    title: Option<&str>,
    title_alignment: TitleAlignment,
    tags: (&str, &str),
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
                tags.0,
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
        tags.0,
        frame_width,
        frame_height,
    );
    if let Some(title) = title.filter(|_| area.width > 2) {
        match title_alignment {
            TitleAlignment::Center => add_centered_run(
                runs,
                area.y,
                title,
                tags.1,
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
                tags.1,
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
            tags.0,
            frame_width,
            frame_height,
        );
        add_run(
            runs,
            area.x + area.width - 1,
            area.y + row,
            "│",
            tags.0,
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
            tags.0,
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
            "popup",
            width,
            height,
        );
    }
    draw_box_border(
        runs,
        area,
        None,
        TitleAlignment::Center,
        ("toast-border", "toast-border"),
        width,
        height,
    );
    if area.height > 1 {
        add_centered_run(
            runs,
            area.y + 1,
            message,
            "text",
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
        ("Performing replacement...".to_string(), "text"),
        (String::new(), "text"),
        (
            format!(
                "Completed: {completed}/{} ({percentage:.2}%)",
                state.total_replacements
            ),
            "info",
        ),
        (
            format!(
                "Time: {}",
                format_duration(state.replacement_started.elapsed())
            ),
            "info",
        ),
    ];
    let start_y = height.saturating_sub(lines.len()) / 2;
    for (offset, (text, tag)) in lines.iter().enumerate() {
        add_centered_run(runs, start_y + offset, text, tag, 0, width, width, height);
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
            "diff-added",
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
        "text",
        x + content_width,
        height,
    );

    let mut y = list_title_y + 1;
    for result in state.errors.iter().skip(state.replacement_errors_pos) {
        if y.saturating_add(2) >= height {
            break;
        }
        let (path, error) = result.display_error();
        add_run(runs, x, y + 1, &path, "text", x + content_width, height);
        add_run(runs, x, y + 2, error, "error", x + content_width, height);
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
            ("text", "text"),
            frame_width,
            frame_height,
        );
        add_run(
            runs,
            x + 1,
            area.y + 1,
            &number.to_string(),
            "text",
            x + width.saturating_sub(1),
            frame_height,
        );
    }
}

fn default_content_width(width: usize) -> (usize, usize) {
    if width == 0 {
        return (0, 0);
    }
    let percentage = if width >= 300 { 80 } else { 90 };
    let content_width = (width * percentage / 100).clamp(1, width);
    ((width - content_width) / 2, content_width)
}

#[allow(clippy::too_many_arguments)]
fn add_centered_run(
    runs: &mut Vec<Run>,
    y: usize,
    text: &str,
    tag: &str,
    x: usize,
    width: usize,
    frame_width: usize,
    frame_height: usize,
) {
    let text = truncate(text, width);
    let text_width = display_width(&text);
    add_run(
        runs,
        x + width.saturating_sub(text_width) / 2,
        y,
        &text,
        tag,
        frame_width,
        frame_height,
    );
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

#[allow(clippy::too_many_arguments, clippy::too_many_lines)]
fn render_results(
    runs: &mut Vec<Run>,
    input_source: &InputSource,
    wrap_preview_text: bool,
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
    if let Some(selected) = search_state
        .results
        .get(search_state.primary_selected_pos())
    {
        render_preview(
            runs,
            input_source,
            selected,
            wrap_preview_text,
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
    let (marker_tag, path_tag, accessory_tag) = row_tag
        .map_or(("info", "text", "info"), |selection_tag| {
            (selection_tag, selection_tag, selection_tag)
        });

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
        accessory_tag,
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
) -> Option<&'static str> {
    if !results_focussed || !selected {
        return None;
    }

    Some(match (primary_selected, result.search_result.included) {
        (true, true) => "selection",
        (false, true) => "selection-secondary",
        (true, false) => "selection-excluded",
        (false, false) => "selection-secondary-excluded",
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
            "error",
            x + width,
            frame_height,
        );
        return;
    }

    let preview = match build_preview_sections(input_source, result, height) {
        Ok(preview) => preview,
        Err(error) => {
            add_run(
                runs,
                x,
                y,
                &format!("Error generating preview: {error}"),
                "error",
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
) -> Result<PreviewSections, String> {
    let diff = diff_lines(result);
    let context_height = preview_height
        .saturating_sub(diff.len().saturating_sub(1))
        .max(1);
    let line_index = result.search_result.start_line_number().saturating_sub(1);
    let indexed_lines = read_preview_window(input_source, result, line_index, preview_height)?;
    let selected_position = indexed_lines
        .iter()
        .position(|(index, _)| *index == line_index)
        .ok_or_else(|| "File content has changed".to_string())?;
    let expected = expected_first_line_content(result);
    if indexed_lines[selected_position].1 != expected {
        return Err("File content has changed".to_string());
    }

    let (before, after) = centered_context_lines(indexed_lines, selected_position, context_height);
    let end_line_index = result.search_result.end_line_number().saturating_sub(1);
    Ok(PreviewSections {
        before: before
            .into_iter()
            .map(|(number, text)| context_preview_line(number, &text))
            .collect(),
        diff,
        after: after
            .into_iter()
            .filter(|(number, _)| *number > end_line_index)
            .map(|(number, text)| context_preview_line(number, &text))
            .collect(),
    })
}

fn read_preview_window(
    input_source: &InputSource,
    result: &SearchResultWithReplacement,
    line_index: usize,
    preview_height: usize,
) -> Result<Vec<(usize, String)>, String> {
    let start = line_index.saturating_sub(preview_height);
    let end = line_index.saturating_add(preview_height);
    match input_source {
        InputSource::Directory(_) => {
            let path = result
                .search_result
                .path
                .as_deref()
                .ok_or_else(|| "Missing file path for preview".to_string())?;
            let lines = read_lines_range(path, start, end).map_err(|error| error.to_string())?;
            Ok(lines.collect())
        }
        InputSource::Stdin(stdin) => Ok(stdin
            .lines()
            .enumerate()
            .skip(start)
            .take(end.saturating_sub(start).saturating_add(1))
            .map(|(number, text)| (number, text.to_string()))
            .collect()),
    }
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

fn context_preview_line(_number: usize, text: &str) -> PreviewLine {
    let mut line = PreviewLine::default();
    push_preview_segment(&mut line, "  ", "text");
    push_preview_segment(&mut line, text, "text");
    line
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
                            "diff-removed-emph"
                        } else {
                            "diff-removed"
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
                            "diff-added-emph"
                        } else {
                            "diff-added"
                        },
                    )
                })
                .collect();
            diff_lines_from_segments("- ", "diff-removed", old)
                .into_iter()
                .chain(diff_lines_from_segments("+ ", "diff-added", new))
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
            "diff-removed",
            vec![
                (&line.content[..start], "diff-removed"),
                (&line.content[start..end], "diff-removed-emph"),
                (&line.content[end..], "diff-removed"),
            ],
        ));
    }

    let first = &lines[0].1.content;
    let last = &lines[lines.len() - 1].1.content;
    result.extend(diff_lines_from_segments(
        "+ ",
        "diff-added",
        vec![
            (&first[..match_start_in_first_line], "diff-added"),
            (replacement, "diff-added-emph"),
            (&last[match_end_in_last_line..], "diff-added"),
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
            diff_lines_from_segments("- ", "diff-removed", vec![(&line.content, "diff-removed")])
        })
        .collect::<Vec<_>>();
    let first = &lines[0].1.content;
    let last = &lines[lines.len() - 1].1.content;
    result.extend(diff_lines_from_segments(
        "+ ",
        "diff-added",
        vec![
            (&first[..match_start_in_first_line], "diff-added"),
            (replacement, "diff-added"),
            (&last[match_end_in_last_line..], "diff-added"),
        ],
    ));
    result
}

fn diff_lines_from_segments(
    prefix: &str,
    prefix_tag: &'static str,
    segments: Vec<(&str, &'static str)>,
) -> Vec<PreviewLine> {
    let mut lines = Vec::new();
    let mut current = PreviewLine::default();
    push_preview_segment(&mut current, prefix, prefix_tag);
    for (text, tag) in segments {
        for part in text.split_inclusive('\n') {
            let ends_line = part.ends_with('\n');
            let part = part.strip_suffix('\n').unwrap_or(part);
            push_preview_segment(&mut current, part, tag);
            if ends_line {
                lines.push(current);
                current = PreviewLine::default();
                push_preview_segment(&mut current, prefix, prefix_tag);
            }
        }
    }
    lines.push(current);
    lines
}

fn push_preview_segment(line: &mut PreviewLine, text: &str, tag: &'static str) {
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
        push_preview_segment(&mut truncated, &text, segment.tag);
        if used >= width {
            break;
        }
    }
    truncated
}

fn wrap_preview_line(line: &PreviewLine, width: usize) -> Vec<PreviewLine> {
    let prefix_width = display_width(WRAPPED_LINE_PREFIX);
    if width <= prefix_width {
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
                current = PreviewLine::default();
                push_preview_segment(&mut current, WRAPPED_LINE_PREFIX, "dim");
                used = prefix_width;
            }
            if used + character_width > width {
                continue;
            }
            push_preview_segment(&mut current, &character.to_string(), segment.tag);
            used += character_width;
        }
    }
    if !current.segments.is_empty() || line.segments.is_empty() {
        wrapped.push(current);
    }
    wrapped
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
        app::{Popup, Screen, SearchPhase},
        errors::AppError,
        line_reader::LineEnding,
        replace::{PerformingReplacementState, ReplaceResult, ReplaceState},
        search::{ByteRangeParams, Line, SearchResult, SearchResultWithReplacement},
    };
    use tempfile::tempdir;
    use tokio::sync::mpsc;

    use crate::engine::ScooterEngine;

    use super::{
        Frame, PopupLine, context_preview_line, diff_lines, display_width, render_paragraph_popup,
        render_results_tallies, truncate,
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
                .any(|segment| segment.tag == "diff-removed-emph" && segment.text == "old")
        }));
        assert!(preview.iter().any(|line| {
            line.segments
                .iter()
                .any(|segment| segment.tag == "diff-added-emph" && segment.text == "new")
        }));
    }

    #[test]
    fn context_preview_lines_use_a_two_space_prefix_without_line_numbers() {
        let line = context_preview_line(41, "context text");
        assert_eq!(
            line.segments
                .iter()
                .map(|segment| segment.text.as_str())
                .collect::<String>(),
            "  context text"
        );
        assert!(line.segments.iter().all(|segment| segment.tag == "text"));
    }

    #[test]
    fn popup_titles_are_centred_and_content_keeps_one_cell_horizontal_padding() {
        let mut runs = Vec::new();
        render_paragraph_popup(
            &mut runs,
            "Notice",
            &[PopupLine {
                text: "body".to_string(),
                tag: "text",
            }],
            100,
            40,
        );

        let title = runs
            .iter()
            .find(|run| run.text == "Notice")
            .expect("popup title");
        assert_eq!((title.x, title.y), (46, 18));
        assert_eq!(title.tag, "popup");
        assert!(runs.iter().any(|run| {
            run.x == 7 && run.y == 18 && run.text.starts_with('┌') && run.tag == "popup-border"
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
        assert!(preview.runs.iter().any(|run| run.tag == "diff-removed"));
        assert!(preview.runs.iter().any(|run| run.tag == "diff-added"));

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
                .any(|run| run.tag == "popup")
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
