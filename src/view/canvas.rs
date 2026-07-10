//! Frame primitives and semantic style tags.

use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

/// Semantic styles shared with the Steel layer's style table.
///
/// Keep the textual wire values in one place: adding a renderer style now
/// requires an intentional corresponding Steel mapping or its safe fallback.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum StyleTag {
    Text,
    Dim,
    Selection,
    SelectionSecondary,
    SelectionExcluded,
    SelectionSecondaryExcluded,
    Active,
    Popup,
    PopupBorder,
    ToastBorder,
    Error,
    Info,
    DiffAdded,
    DiffAddedEmph,
    DiffRemoved,
    DiffRemovedEmph,
}

impl StyleTag {
    pub(crate) const fn as_str(self) -> &'static str {
        match self {
            Self::Text => "text",
            Self::Dim => "dim",
            Self::Selection => "selection",
            Self::SelectionSecondary => "selection-secondary",
            Self::SelectionExcluded => "selection-excluded",
            Self::SelectionSecondaryExcluded => "selection-secondary-excluded",
            Self::Active => "active",
            Self::Popup => "popup",
            Self::PopupBorder => "popup-border",
            Self::ToastBorder => "toast-border",
            Self::Error => "error",
            Self::Info => "info",
            Self::DiffAdded => "diff-added",
            Self::DiffAddedEmph => "diff-added-emph",
            Self::DiffRemoved => "diff-removed",
            Self::DiffRemovedEmph => "diff-removed-emph",
        }
    }
}

/// A semantic styled string positioned relative to the Steel popup content.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Run {
    pub(crate) x: usize,
    pub(crate) y: usize,
    pub(crate) text: String,
    pub(crate) tag: StyleTag,
}

/// Complete frame, including an optional text cursor position.
#[derive(Debug, Default, PartialEq, Eq)]
pub(crate) struct Frame {
    pub(crate) runs: Vec<Run>,
    pub(crate) cursor: Option<(usize, usize)>,
}

pub(super) fn add_segment(
    runs: &mut Vec<Run>,
    x: &mut usize,
    y: usize,
    text: &str,
    tag: StyleTag,
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
            tag,
        });
    }
    *x = x.saturating_add(clipped_width);
}

pub(super) fn add_run(
    runs: &mut Vec<Run>,
    x: usize,
    y: usize,
    text: &str,
    tag: StyleTag,
    width: usize,
    height: usize,
) {
    if x >= width || y >= height {
        return;
    }

    let text = truncate(text, width - x);
    if !text.is_empty() {
        runs.push(Run { x, y, text, tag });
    }
}

#[allow(clippy::too_many_arguments)]
pub(super) fn add_centered_run(
    runs: &mut Vec<Run>,
    y: usize,
    text: &str,
    tag: StyleTag,
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

pub(super) fn truncate(text: &str, max_width: usize) -> String {
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

pub(super) fn display_width(text: &str) -> usize {
    UnicodeWidthStr::width(text)
}
