use steel::{
    declare_module,
    steel_vm::ffi::{FFIModule, RegisterFFIFn},
};

mod scooter_hx;

declare_module!(create_module);

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("steel/sys-info");

    module
        .register_fn("Scooter-search", scooter_hx::ScooterHx::search)
        .register_fn(
            "Scooter-search-progressing?",
            scooter_hx::ScooterHx::search_is_progressing,
        )
        .register_fn(
            "Scooter-search-results-window",
            scooter_hx::ScooterHx::search_results_window,
        )
        .register_fn(
            "Scooter-toggle-inclusion",
            scooter_hx::ScooterHx::toggle_inclusion,
        )
        .register_fn("Scooter-replace", scooter_hx::ScooterHx::replace);

    module
}
