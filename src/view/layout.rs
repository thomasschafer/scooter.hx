//! Geometry shared by the native screens and overlays.

#[derive(Debug, Clone, Copy)]
pub(super) struct FieldsLayout {
    pub(super) x: usize,
    pub(super) y: usize,
    pub(super) width: usize,
    pub(super) count: usize,
    pub(super) banner_y: usize,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct PopupArea {
    pub(super) x: usize,
    pub(super) y: usize,
    pub(super) width: usize,
    pub(super) height: usize,
}

#[derive(Debug, Clone, Copy)]
pub(super) enum TitleAlignment {
    Center,
    Left,
}

pub(super) fn default_content_width(width: usize) -> (usize, usize) {
    if width == 0 {
        return (0, 0);
    }
    let percentage = if width >= 300 { 80 } else { 90 };
    let content_width = (width * percentage / 100).clamp(1, width);
    ((width - content_width) / 2, content_width)
}

pub(super) fn fields_layout(
    frame_width: usize,
    height: usize,
    requested_count: usize,
    field_height: usize,
) -> FieldsLayout {
    let count = requested_count.min(height / field_height);
    let fields_height = count * field_height;
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

pub(super) fn popup_area(width: usize, height: usize, content_height: usize) -> PopupArea {
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

pub(super) fn popup_inner(area: PopupArea) -> (usize, usize, usize, usize) {
    (
        area.x.saturating_add(2).min(area.x + area.width),
        area.y.saturating_add(1),
        area.width.saturating_sub(4),
        area.height.saturating_sub(2),
    )
}
