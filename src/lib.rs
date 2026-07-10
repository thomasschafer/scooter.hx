//! Steel FFI boundary for the native Scooter rewrite.

mod engine;
mod key;
mod view;

use std::panic::{AssertUnwindSafe, catch_unwind};

use abi_stable::std_types::RVec;
use engine::ScooterEngine;
use steel::{
    declare_module,
    rvals::Custom,
    steel_vm::ffi::{FFIModule, FFIValue, RegisterFFIFn},
};
use view::Run;

impl Custom for ScooterEngine {
    fn fmt_ffi(&self) -> Option<abi_stable::std_types::RString> {
        Some("#<ScooterEngine>".into())
    }
}

/// Keep Rust unwinding inside the dylib boundary. Every exported operation uses
/// this helper and returns its operation-specific safe fallback after logging.
fn ffi_guard<T>(
    entry_point: &str,
    operation: impl FnOnce() -> T,
    fallback: impl FnOnce() -> T,
) -> T {
    match catch_unwind(AssertUnwindSafe(operation)) {
        Ok(value) => value,
        Err(payload) => {
            let message = payload.downcast_ref::<&str>().map_or_else(
                || {
                    payload
                        .downcast_ref::<String>()
                        .map_or("non-string panic payload", String::as_str)
                },
                |message| *message,
            );
            eprintln!("scooter-hx: panic in {entry_point}: {message}");
            fallback()
        }
    }
}

fn scooter_engine_new(directory: &str) -> Result<ScooterEngine, String> {
    ffi_guard(
        "Scooter-engine-new",
        || ScooterEngine::new(directory).map_err(|error| error.to_string()),
        || Err("Scooter engine creation panicked".to_string()),
    )
}

fn scooter_handle_key(engine: &mut ScooterEngine, code: &str, modifiers: usize) -> String {
    ffi_guard(
        "Scooter-handle-key",
        || engine.handle_key(code, modifiers),
        || "rerender".to_string(),
    )
}

fn scooter_pump(engine: &mut ScooterEngine) -> String {
    ffi_guard("Scooter-pump", || engine.pump(), || "idle".to_string())
}

fn scooter_busy(engine: &ScooterEngine) -> bool {
    ffi_guard("Scooter-busy?", || engine.busy(), || false)
}

fn scooter_render(engine: &mut ScooterEngine, width: usize, height: usize) -> FFIValue {
    ffi_guard(
        "Scooter-render",
        || frame_to_ffi(engine.render(width, height).runs),
        empty_frame,
    )
}

fn scooter_cursor(engine: &ScooterEngine, width: usize, height: usize) -> FFIValue {
    ffi_guard(
        "Scooter-cursor",
        || match engine.cursor(width, height) {
            Some((x, y)) => position_to_ffi(x, y),
            None => FFIValue::from(false),
        },
        || FFIValue::from(false),
    )
}

fn scooter_reset(engine: &mut ScooterEngine) {
    ffi_guard("Scooter-reset", || engine.reset(), || ());
}

fn scooter_quit(engine: &mut ScooterEngine) {
    ffi_guard("Scooter-quit", || engine.quit(), || ());
}

fn frame_to_ffi(runs: Vec<Run>) -> FFIValue {
    let mut frame = RVec::with_capacity(runs.len());
    for Run { x, y, text, tag } in runs {
        let mut run = RVec::with_capacity(4);
        run.push(FFIValue::from(x));
        run.push(FFIValue::from(y));
        run.push(FFIValue::from(text));
        run.push(FFIValue::from(tag));
        frame.push(FFIValue::from(run));
    }
    FFIValue::from(frame)
}

fn position_to_ffi(x: usize, y: usize) -> FFIValue {
    let mut position = RVec::with_capacity(2);
    position.push(FFIValue::from(x));
    position.push(FFIValue::from(y));
    FFIValue::from(position)
}

fn empty_frame() -> FFIValue {
    FFIValue::from(RVec::new())
}

declare_module!(create_module);

fn create_module() -> FFIModule {
    ffi_guard("steel/scooter module initialization", build_module, || {
        FFIModule::new("steel/scooter")
    })
}

fn build_module() -> FFIModule {
    let mut module = FFIModule::new("steel/scooter");
    module
        .register_fn("Scooter-engine-new", scooter_engine_new)
        .register_fn("Scooter-handle-key", scooter_handle_key)
        .register_fn("Scooter-pump", scooter_pump)
        .register_fn("Scooter-busy?", scooter_busy)
        .register_fn("Scooter-render", scooter_render)
        .register_fn("Scooter-cursor", scooter_cursor)
        .register_fn("Scooter-reset", scooter_reset)
        .register_fn("Scooter-quit", scooter_quit);
    module
}
