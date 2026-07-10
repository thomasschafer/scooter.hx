//! Minimal native frame model for the S2 search-fields and results screens.

use std::{cmp::min, path::Path, time::Duration};

use scooter_core::{
    app::{App, FocussedSection, InputSource, Screen, SearchPhase, SearchState},
    fields::{Field, SearchField, NUM_SEARCH_FIELDS},
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

/// Complete S2 frame, including an optional text cursor position.
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

/// Render the deliberately small S2 view.
pub(crate) fn render(app: &mut App, width: usize, height: usize) -> Frame {
    let mut frame = Frame::default();
    if width == 0 || height == 0 {
        return frame;
    }

    let Screen::SearchFields(search_fields_state) = &app.ui_state.current_screen else {
        return frame;
    };

    let fields_focussed = search_fields_state.focussed_section == FocussedSection::SearchFields;
    let count = if fields_focussed {
        NUM_SEARCH_FIELDS as usize
    } else {
        FIELD_COUNT_WHEN_RESULTS_FOCUSSED
    };
    let layout = fields_layout(width, height, count);

    for (index, field) in app.search_fields.fields.iter().take(layout.count).enumerate() {
        let highlighted = fields_focussed && index == app.search_fields.highlighted;
        render_field(&mut frame.runs, field, layout, index, highlighted, width, height);
    }

    if fields_focussed
        && !app.show_popup()
        && let Some(cursor_offset) = app.search_fields.highlighted_field().cursor_pos()
    {
        let field_y = layout.y + app.search_fields.highlighted * FIELD_HEIGHT;
        if app.search_fields.highlighted < layout.count
            && layout.width > 2
            && field_y.saturating_add(1) < height
        {
            frame.cursor = Some((
                min(layout.x.saturating_add(1 + cursor_offset), width.saturating_sub(1)),
                field_y + 1,
            ));
        }
    }

    if layout.banner_y < height {
        let search_is_empty = app.search_fields.search().text().is_empty();
        let base_path = match &app.input_source {
            InputSource::Directory(directory) => directory.as_path(),
            InputSource::Stdin(_) => Path::new("."),
        };
        let Screen::SearchFields(search_fields_state) = &mut app.ui_state.current_screen else {
            return frame;
        };
        if let Some(search_state) = search_fields_state.search_state.as_mut() {
            render_results(
                &mut frame.runs,
                base_path,
                search_state,
                layout.banner_y,
                width,
                height,
            );
        } else {
            let (status, tag) = if search_is_empty {
                ("Search is empty", "error")
            } else {
                ("Still searching...", "info")
            };
            add_run(
                &mut frame.runs,
                0,
                layout.banner_y,
                &format!("Results: 0 [{status}]"),
                tag,
                width,
                height,
            );
        }
    }

    frame
}

/// Return the cursor that corresponds to [`render`] without changing view state.
pub(crate) fn cursor(app: &App, width: usize, height: usize) -> Option<(usize, usize)> {
    if width == 0 || height == 0 || app.show_popup() {
        return None;
    }

    let Screen::SearchFields(search_fields_state) = &app.ui_state.current_screen else {
        return None;
    };
    if search_fields_state.focussed_section != FocussedSection::SearchFields {
        return None;
    }

    let layout = fields_layout(width, height, NUM_SEARCH_FIELDS as usize);
    let index = app.search_fields.highlighted;
    let cursor_offset = app.search_fields.highlighted_field().cursor_pos()?;
    let field_y = layout.y + index * FIELD_HEIGHT;
    if index >= layout.count || layout.width <= 2 || field_y.saturating_add(1) >= height {
        return None;
    }

    Some((
        min(layout.x.saturating_add(1 + cursor_offset), width.saturating_sub(1)),
        field_y + 1,
    ))
}

fn fields_layout(frame_width: usize, height: usize, requested_count: usize) -> FieldsLayout {
    let count = requested_count.min(height / FIELD_HEIGHT);
    let fields_height = count * FIELD_HEIGHT;
    let banner_and_results = usize::from(height > fields_height);
    let spare_height = height.saturating_sub(fields_height + banner_and_results);
    // Match the TUI's centred field stack while reserving room for the results below it.
    let y = spare_height / 3;
    let width = (frame_width.max(1) * 9 / 10).clamp(1, frame_width.max(1));
    let x = frame_width.saturating_sub(width) / 2;

    FieldsLayout {
        x,
        y,
        width,
        count,
        banner_y: y + fields_height,
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
    if y >= frame_height || layout.width == 0 {
        return;
    }

    let border_tag = if highlighted { "active" } else { "text" };
    if layout.width == 1 {
        add_run(runs, layout.x, y, "│", border_tag, frame_width, frame_height);
        return;
    }

    let top = box_top(field.name.title(), layout.width);
    let bottom = format!("└{}┘", "─".repeat(layout.width.saturating_sub(2)));
    add_run(runs, layout.x, y, &top, border_tag, frame_width, frame_height);
    add_run(
        runs,
        layout.x,
        y + 1,
        "│",
        border_tag,
        frame_width,
        frame_height,
    );
    add_run(
        runs,
        layout.x + layout.width.saturating_sub(1),
        y + 1,
        "│",
        border_tag,
        frame_width,
        frame_height,
    );
    add_run(
        runs,
        layout.x,
        y + 2,
        &bottom,
        border_tag,
        frame_width,
        frame_height,
    );

    let value = match &field.field {
        Field::Text(text) => text.text().to_owned(),
        Field::Checkbox(checkbox) => {
            let state = if checkbox.checked { "[X]" } else { "[ ]" };
            format!("{state} {}", field.name.title())
        }
    };
    let value = truncate(&value, layout.width.saturating_sub(2));
    add_run(
        runs,
        layout.x + 1,
        y + 1,
        &value,
        "text",
        frame_width,
        frame_height,
    );
}

fn box_top(title: &str, width: usize) -> String {
    if width <= 2 {
        return "┌┐".to_string();
    }

    let prefix = "┌─";
    let title = truncate(title, width.saturating_sub(display_width(prefix) + 1));
    let used = display_width(prefix) + display_width(&title) + 1;
    format!("{prefix}{title}{}┐", "─".repeat(width.saturating_sub(used)))
}

fn render_results(
    runs: &mut Vec<Run>,
    base_path: &Path,
    search_state: &mut SearchState,
    banner_y: usize,
    width: usize,
    height: usize,
) {
    let (status, tag) = status(search_state.phase);
    let elapsed = search_state.phase.elapsed().map(format_duration).unwrap_or_default();
    add_run(
        runs,
        0,
        banner_y,
        &format!(
            "Results: {} [{status}]{elapsed}",
            search_state.results.len()
        ),
        tag,
        width,
        height,
    );

    let list_y = banner_y.saturating_add(1);
    let num_to_render = height.saturating_sub(list_y);
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
        let included = if result.search_result.included { "[x]" } else { "[ ]" };
        let path = result
            .search_result
            .path
            .as_ref()
            .map_or_else(|| "<stdin>".to_string(), |path| relative_path(base_path, path));
        let text = format!(
            "{prefix}{included} {path}:{}",
            result.search_result.start_line_number()
        );
        add_run(
            runs,
            0,
            list_y + index.saturating_sub(search_state.view_offset),
            &text,
            if selected { "selection" } else { "text" },
            width,
            height,
        );
    }
}

fn status(phase: SearchPhase) -> (&'static str, &'static str) {
    match phase {
        SearchPhase::Invalid => ("Invalid search", "error"),
        SearchPhase::Complete { .. } => ("Search complete", "diff-added"),
        SearchPhase::Pending | SearchPhase::Running { .. } => ("Still searching...", "info"),
    }
}

fn format_duration(duration: Duration) -> String {
    format!(
        " [Time taken: {}.{:03}s]",
        duration.as_secs(),
        duration.subsec_millis()
    )
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
    use super::{box_top, truncate};

    #[test]
    fn truncates_by_display_width() {
        assert_eq!(truncate("a界b", 3), "a界");
        assert_eq!(truncate("a界b", 2), "a");
    }

    #[test]
    fn field_title_never_overflows_its_box() {
        assert_eq!(super::display_width(&box_top("Search text", 12)), 12);
    }
}
