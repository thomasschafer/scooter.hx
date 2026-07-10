//! Status banners and compact key hints.

use std::time::Duration;

use scooter_core::app::{App, SearchPhase};

use super::{
    FieldsLayout, Run, StyleTag,
    canvas::{add_centered_run, add_segment, display_width, truncate},
};

pub(super) fn render_footer(runs: &mut Vec<Run>, app: &App, width: usize, y: usize) {
    let hints = app
        .keymaps_compact()
        .into_iter()
        .map(|(key, action)| format!("{key} {action}"))
        .collect::<Vec<_>>()
        .join(" / ");
    add_centered_run(runs, y, &hints, StyleTag::Info, 0, width, width, y + 1);
}

#[allow(clippy::too_many_arguments)]
pub(super) fn render_banner(
    runs: &mut Vec<Run>,
    layout: FieldsLayout,
    num_results: usize,
    status: &str,
    status_tag: StyleTag,
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
    let right_x = if show_right { end_x - right_width } else { end_x };

    let mut left_x = layout.x;
    add_segment(
        runs,
        &mut left_x,
        layout.banner_y,
        &left_number,
        StyleTag::Text,
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
            if is_complete { StyleTag::DiffAdded } else { StyleTag::Info },
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
            StyleTag::Info,
            right_x,
            height,
        );
    }

    debug_assert!(end_x <= width);
}

pub(super) fn status(phase: SearchPhase) -> (&'static str, StyleTag) {
    match phase {
        SearchPhase::Invalid => ("Invalid search", StyleTag::Error),
        SearchPhase::Complete { .. } => ("Search complete", StyleTag::DiffAdded),
        SearchPhase::Pending | SearchPhase::Running { .. } => ("Still searching...", StyleTag::Info),
    }
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

pub(super) fn format_duration(duration: Duration) -> String {
    format!("{}.{:03}s", duration.as_secs(), duration.subsec_millis())
}
