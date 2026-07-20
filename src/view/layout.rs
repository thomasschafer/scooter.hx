//! Geometry shared by the native screens and overlays.

#[derive(Debug, Clone, Copy)]
pub(super) struct FieldsLayout {
    pub(super) x: usize,
    pub(super) y: usize,
    pub(super) width: usize,
    /// Index of the first search field rendered in this viewport.
    pub(super) first: usize,
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
    if width <= 4 {
        return (0, width);
    }
    (2, width - 4)
}

pub(super) fn fields_layout(
    frame_width: usize,
    height: usize,
    requested_count: usize,
    field_height: usize,
    focussed_field: Option<usize>,
) -> FieldsLayout {
    let count = requested_count.min(height / field_height);
    let fields_height = count * field_height;
    // Match the TUI: fields at the top of the content area, a one-row gap,
    // then the results banner and list fill the remaining height.
    let (x, width) = default_content_width(frame_width);
    // Preserve the conventional first-N layout until focus leaves it.  Then
    // move the viewport just far enough to keep the focussed field visible.
    let first = focussed_field
        .filter(|&index| index >= count)
        .map_or(0, |index| index + 1 - count);

    FieldsLayout {
        x,
        y: 0,
        width,
        first,
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

#[cfg(test)]
mod tests {
    use super::{default_content_width, fields_layout};

    #[test]
    fn content_uses_two_cell_horizontal_gutters() {
        assert_eq!(default_content_width(80), (2, 76));

        let fields = fields_layout(80, 20, 2, 3, None);
        assert_eq!((fields.x, fields.width), (2, 76));
    }

    #[test]
    fn field_viewport_keeps_focussed_field_visible() {
        let fields = fields_layout(80, 11, 7, 3, Some(6));
        assert_eq!((fields.first, fields.count), (4, 3));

        let fields = fields_layout(80, 11, 7, 3, Some(2));
        assert_eq!((fields.first, fields.count), (0, 3));

        // Results focus deliberately retains the first two fields.
        let fields = fields_layout(80, 11, 2, 3, None);
        assert_eq!((fields.first, fields.count), (0, 2));
    }

    #[test]
    fn very_narrow_content_uses_all_available_columns() {
        assert_eq!(default_content_width(0), (0, 0));
        assert_eq!(default_content_width(1), (0, 1));
        assert_eq!(default_content_width(2), (0, 2));
        assert_eq!(default_content_width(3), (0, 3));
        assert_eq!(default_content_width(4), (0, 4));
    }
}
