use steel::{
    declare_module,
    steel_vm::ffi::{FFIModule, RegisterFFIFn},
};

#[cfg(test)]
#[macro_use]
mod test_utils;

pub mod scooter_hx;
mod validation;

declare_module!(create_module);

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("steel/scooter");

    module
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
            "Scooter-search-results-window",
            scooter_hx::ScooterHx::search_results_window,
        )
        .register_fn(
            "Scooter-toggle-inclusion",
            scooter_hx::ScooterHx::toggle_inclusion,
        )
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
        );

    module
}
