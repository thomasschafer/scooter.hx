use scooter_core::fields::TextField;
use steel::{
    declare_module,
    steel_vm::ffi::{FFIModule, RegisterFFIFn},
};

#[cfg(test)]
#[macro_use]
mod test_utils;

mod logging;
pub mod scooter_hx;
mod unicode;
mod validation;

declare_module!(create_module);

#[allow(clippy::too_many_lines)]
fn create_module() -> FFIModule {
    let mut module = FFIModule::new("steel/scooter");

    module
        // Scooter
        .register_fn("Scooter-new", scooter_hx::ScooterHx::new)
        .register_fn("Scooter-reset", scooter_hx::ScooterHx::reset)
        .register_fn("Scooter-start-search", scooter_hx::ScooterHx::start_search)
        .register_fn(
            "Scooter-cancel-search",
            scooter_hx::ScooterHx::cancel_search,
        )
        .register_fn(
            "Scooter-search-complete?",
            scooter_hx::ScooterHx::search_complete,
        )
        .register_fn(
            "Scooter-search-result-count",
            scooter_hx::ScooterHx::search_result_count,
        )
        .register_fn(
            "Scooter-search-results-window",
            scooter_hx::ScooterHx::search_results_window,
        )
        .register_fn(
            "Scooter-toggle-inclusion",
            scooter_hx::ScooterHx::toggle_inclusion,
        )
        .register_fn("Scooter-toggle-all", scooter_hx::ScooterHx::toggle_all)
        .register_fn(
            "Scooter-start-replace",
            scooter_hx::ScooterHx::start_replace,
        )
        .register_fn(
            "Scooter-num-replacements-complete",
            scooter_hx::ScooterHx::num_replacements_complete,
        )
        .register_fn(
            "Scooter-replacement-complete?",
            scooter_hx::ScooterHx::replacement_complete,
        )
        .register_fn(
            "Scooter-cancel-replacement",
            scooter_hx::ScooterHx::cancel_replacement,
        )
        .register_fn(
            "Scooter-replacement-stats",
            scooter_hx::ScooterHx::replacement_stats,
        )
        .register_fn(
            "Scooter-replacement-errors",
            scooter_hx::ScooterHx::replacement_errors,
        )
        // SteelSearchResult
        .register_fn(
            "SteelSearchResult-display-path",
            scooter_hx::SteelSearchResult::display_path,
        )
        .register_fn(
            "SteelSearchResult-full-path",
            scooter_hx::SteelSearchResult::full_path,
        )
        .register_fn(
            "SteelSearchResult-line-num",
            scooter_hx::SteelSearchResult::line_num,
        )
        .register_fn(
            "SteelSearchResult-included",
            scooter_hx::SteelSearchResult::included,
        )
        .register_fn(
            "SteelSearchResult-build-preview",
            scooter_hx::SteelSearchResult::build_preview,
        )
        .register_fn(
            "SteelSearchResult-display-error",
            scooter_hx::SteelSearchResult::display_error,
        )
        // ReplacementStats
        .register_fn(
            "ReplacementStats-num-successes",
            scooter_hx::ReplacementStats::num_successes,
        )
        .register_fn(
            "ReplacementStats-num-ignored",
            scooter_hx::ReplacementStats::num_ignored,
        )
        .register_fn(
            "ReplacementStats-num-errors",
            scooter_hx::ReplacementStats::num_errors,
        )
        // unicode
        .register_fn("unicode-display-width", unicode::display_width)
        .register_fn("unicode-truncate-to-width", unicode::truncate_to_width)
        // TextField
        .register_type::<scooter_core::fields::TextField>("TextField?")
        .register_fn("TextField-new", scooter_core::fields::TextField::new)
        .register_fn("TextField-text", |tf: &TextField| {
            scooter_core::fields::TextField::text(tf).to_owned()
        })
        .register_fn(
            "TextField-cursor-pos",
            scooter_core::fields::TextField::visual_cursor_pos,
        )
        .register_fn(
            "TextField-move-cursor-left",
            scooter_core::fields::TextField::move_cursor_left,
        )
        .register_fn(
            "TextField-move-cursor-start",
            scooter_core::fields::TextField::move_cursor_start,
        )
        .register_fn(
            "TextField-move-cursor-right",
            scooter_core::fields::TextField::move_cursor_right,
        )
        .register_fn(
            "TextField-move-cursor-end",
            scooter_core::fields::TextField::move_cursor_end,
        )
        .register_fn(
            "TextField-enter-char",
            scooter_core::fields::TextField::enter_char,
        )
        .register_fn(
            "TextField-delete-char",
            scooter_core::fields::TextField::delete_char,
        )
        .register_fn(
            "TextField-delete-char-forward",
            scooter_core::fields::TextField::delete_char_forward,
        )
        .register_fn(
            "TextField-move-cursor-back-word",
            scooter_core::fields::TextField::move_cursor_back_word,
        )
        .register_fn(
            "TextField-delete-word-backward",
            scooter_core::fields::TextField::delete_word_backward,
        )
        .register_fn(
            "TextField-move-cursor-forward-word",
            scooter_core::fields::TextField::move_cursor_forward_word,
        )
        .register_fn(
            "TextField-delete-word-forward",
            scooter_core::fields::TextField::delete_word_forward,
        )
        .register_fn("TextField-clear", scooter_core::fields::TextField::clear)
        .register_fn(
            "TextField-delete-to-start",
            scooter_core::fields::TextField::delete_to_start,
        )
        .register_fn(
            "TextField-insert-text",
            scooter_core::fields::TextField::insert_text,
        );

    module
}
