//! Minimal Steel bridge used to prove the rewrite toolchain end to end.

use std::panic::{AssertUnwindSafe, catch_unwind};

use abi_stable::std_types::RVec;
use steel::{
    declare_module,
    rvals::Custom,
    steel_vm::ffi::{FFIModule, FFIValue, RegisterFFIFn},
};

const CTRL_MODIFIER: usize = 2;

/// Opaque state passed between Steel and this dylib.
#[derive(Default)]
struct ScooterEngine;

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

fn scooter_engine_new() -> ScooterEngine {
    ffi_guard(
        "Scooter-engine-new",
        ScooterEngine::default,
        ScooterEngine::default,
    )
}

fn clip(text: &str, width: usize) -> String {
    text.chars().take(width).collect()
}

#[derive(Debug)]
struct Run {
    x: usize,
    y: usize,
    text: String,
    tag: String,
}

fn add_run(runs: &mut Vec<Run>, x: usize, y: usize, text: &str, tag: &str, width: usize) {
    if x >= width {
        return;
    }

    let text = clip(text, width - x);
    if !text.is_empty() {
        runs.push(Run {
            x,
            y,
            text,
            tag: tag.to_string(),
        });
    }
}

fn demo_frame(width: usize, height: usize) -> Vec<Run> {
    if width == 0 || height == 0 {
        return Vec::new();
    }

    let mut runs = Vec::with_capacity(height.saturating_mul(2).saturating_add(8));
    let horizontal = "-".repeat(width);
    add_run(&mut runs, 0, 0, &horizontal, "dim", width);
    if height > 1 {
        add_run(&mut runs, 0, height - 1, &horizontal, "dim", width);
    }

    for y in 1..height.saturating_sub(1) {
        add_run(&mut runs, 0, y, "|", "dim", width);
        if width > 1 {
            add_run(&mut runs, width - 1, y, "|", "dim", width);
        }
    }

    let content = [
        (1, "S1 TOOLCHAIN SPIKE", "active"),
        (3, "Search:  demo query", "text"),
        (5, " > fixtures/alpha.txt:2 selected result", "selection"),
        (7, "- before: old value", "diff-removed"),
        (8, "+ after:  new value", "diff-added"),
        (10, "info: STATIC FRAME READY", "info"),
        (12, "error: demo error style", "error"),
    ];

    for (y, text, tag) in content {
        if y < height.saturating_sub(1) {
            add_run(&mut runs, 2, y, text, tag, width);
        }
    }

    runs
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

fn empty_frame() -> FFIValue {
    FFIValue::from(RVec::new())
}

fn scooter_render(_engine: &ScooterEngine, width: usize, height: usize) -> FFIValue {
    ffi_guard(
        "Scooter-render",
        || frame_to_ffi(demo_frame(width, height)),
        empty_frame,
    )
}

fn scooter_handle_key(_engine: &ScooterEngine, code: &str, modifiers: usize) -> String {
    ffi_guard(
        "Scooter-handle-key",
        || {
            if code == "esc" {
                "hide".to_string()
            } else if code == "c" && modifiers & CTRL_MODIFIER != 0 {
                "quit".to_string()
            } else {
                "consumed".to_string()
            }
        },
        || "consumed".to_string(),
    )
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
        .register_fn("Scooter-render", scooter_render)
        .register_fn("Scooter-handle-key", scooter_handle_key);
    module
}

#[cfg(test)]
mod tests {
    use super::{ScooterEngine, demo_frame, scooter_handle_key};

    #[test]
    fn demo_frame_uses_every_style_tag() {
        let tags = demo_frame(80, 20)
            .into_iter()
            .map(|run| run.tag)
            .collect::<Vec<_>>();

        for tag in [
            "text",
            "dim",
            "selection",
            "active",
            "error",
            "info",
            "diff-added",
            "diff-removed",
        ] {
            assert!(tags.iter().any(|actual| actual == tag), "missing {tag}");
        }
    }

    #[test]
    fn key_statuses_follow_the_ffi_contract() {
        let engine = ScooterEngine;
        assert_eq!(scooter_handle_key(&engine, "esc", 0), "hide");
        assert_eq!(scooter_handle_key(&engine, "c", 2), "quit");
        assert_eq!(scooter_handle_key(&engine, "c", 0), "consumed");
    }
}
