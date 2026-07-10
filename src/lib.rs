//! Steel FFI boundary for the native Scooter rewrite.

mod engine;
mod key;
mod logging;
mod options;
mod view;

#[cfg(test)]
mod snapshot_tests;

use std::panic::{AssertUnwindSafe, catch_unwind};

use abi_stable::std_types::RVec;
use engine::{EngineAction, EngineResponse, ScooterEngine};
use options::{EngineOptions, OptionEntry, OptionValue};
use steel::{
    declare_module,
    rvals::Custom,
    steel_vm::ffi::{FFIArg, FFIModule, FFIValue, RegisterFFIFn},
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
            log::warn!("scooter-hx: panic in {entry_point}: {message}");
            fallback()
        }
    }
}

/// Return either a custom engine handle or a human-readable error string.
///
/// A string result, rather than a Steel FFI exception, lets the Scheme layer
/// present core's key-conflict message in Helix and leave its window closed.
fn scooter_engine_new(directory: &str, options: FFIArg<'static>) -> FFIValue {
    ffi_guard(
        "Scooter-engine-new",
        || match parse_engine_options(options) {
            Ok(options) => ScooterEngine::new_with_options(directory, options)
                .map_or_else(FFIValue::from, FFIValue::from),
            Err(error) => FFIValue::from(error),
        },
        || FFIValue::from("Scooter engine creation panicked".to_string()),
    )
}

fn scooter_window_size(engine: &ScooterEngine) -> f64 {
    ffi_guard(
        "Scooter-window-size",
        || engine.window_size(),
        || options::DEFAULT_WINDOW_SIZE,
    )
}

fn parse_engine_options(options: FFIArg<'_>) -> Result<EngineOptions, String> {
    let FFIArg::Vector(entries) = options else {
        return Err("Invalid Scooter options: expected a list of (key value) pairs".to_string());
    };

    let entries = entries
        .into_iter()
        .enumerate()
        .map(|(index, entry)| parse_option_entry(index, entry))
        .collect::<Result<Vec<_>, _>>()?;
    EngineOptions::from_entries(entries)
}

fn parse_option_entry(index: usize, entry: FFIArg<'_>) -> Result<OptionEntry, String> {
    let FFIArg::Vector(pair) = entry else {
        return Err(format!(
            "Invalid Scooter option at position {index}: expected a (key value) pair"
        ));
    };
    if pair.len() != 2 {
        return Err(format!(
            "Invalid Scooter option at position {index}: expected a (key value) pair"
        ));
    }

    let mut pair = pair.into_iter();
    let key = ffi_string(pair.next().expect("length checked"), "option key")?;
    let value = ffi_option_value(pair.next().expect("length checked"), &key)?;
    Ok(OptionEntry { key, value })
}

fn ffi_option_value(value: FFIArg<'_>, key: &str) -> Result<OptionValue, String> {
    match value {
        FFIArg::BoolV(value) => Ok(OptionValue::Bool(value)),
        FFIArg::NumV(value) => Ok(OptionValue::Number(value)),
        FFIArg::Vector(values) => values
            .into_iter()
            .map(|value| ffi_string(value, "key binding"))
            .collect::<Result<Vec<_>, _>>()
            .map(OptionValue::Strings),
        _ => Err(format!(
            "Invalid value for '{key}': expected a boolean, number, or list of strings"
        )),
    }
}

fn ffi_string(value: FFIArg<'_>, kind: &str) -> Result<String, String> {
    match value {
        FFIArg::StringV(value) => Ok(value.into_string()),
        FFIArg::StringRef(value) => Ok(value.to_string()),
        _ => Err(format!("Invalid Scooter {kind}: expected a string")),
    }
}

fn scooter_handle_key(engine: &mut ScooterEngine, code: &str, modifiers: usize) -> FFIValue {
    ffi_guard(
        "Scooter-handle-key",
        || response_to_ffi(engine.handle_key(code, modifiers)),
        || {
            response_to_ffi(EngineResponse {
                status: "rerender",
                actions: Vec::new(),
            })
        },
    )
}

fn scooter_pump(engine: &mut ScooterEngine) -> FFIValue {
    ffi_guard(
        "Scooter-pump",
        || response_to_ffi(engine.pump()),
        || {
            response_to_ffi(EngineResponse {
                status: "idle",
                actions: Vec::new(),
            })
        },
    )
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
        run.push(FFIValue::from(tag.as_str().to_string()));
        frame.push(FFIValue::from(run));
    }
    FFIValue::from(frame)
}

/// Encode an engine result as `(status action...)`, where each action is its
/// own simple list. Keep this portable wire format deliberately narrow: Steel
/// only needs strings and integer line numbers to hand it to Helix in H3.
fn response_to_ffi(response: EngineResponse) -> FFIValue {
    let mut values = RVec::with_capacity(response.actions.len() + 1);
    values.push(FFIValue::from(response.status.to_string()));
    for action in response.actions {
        let mut value = RVec::with_capacity(3);
        match action {
            EngineAction::OpenFile { path, line } => {
                value.push(FFIValue::from("open-file".to_string()));
                value.push(FFIValue::from(path.to_string_lossy().into_owned()));
                value.push(FFIValue::from(line));
            }
        }
        values.push(FFIValue::from(value));
    }
    FFIValue::from(values)
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

#[cfg(test)]
fn scooter_test_panic() -> FFIValue {
    ffi_guard(
        "Scooter-test-panic",
        || panic!("intentional FFI panic for degradation coverage"),
        empty_frame,
    )
}

declare_module!(create_module);

fn create_module() -> FFIModule {
    ffi_guard("steel/scooter module initialization", build_module, || {
        FFIModule::new("steel/scooter")
    })
}

fn build_module() -> FFIModule {
    logging::initialise();
    let mut module = FFIModule::new("steel/scooter");
    module
        .register_fn("Scooter-engine-new", scooter_engine_new)
        .register_fn("Scooter-window-size", scooter_window_size)
        .register_fn("Scooter-handle-key", scooter_handle_key)
        .register_fn("Scooter-pump", scooter_pump)
        .register_fn("Scooter-busy?", scooter_busy)
        .register_fn("Scooter-render", scooter_render)
        .register_fn("Scooter-cursor", scooter_cursor)
        .register_fn("Scooter-reset", scooter_reset)
        .register_fn("Scooter-quit", scooter_quit);
    module
}

#[cfg(test)]
mod tests {
    use tempfile::tempdir;

    use scooter_core::{app::Event, keyboard::KeyCode};

    use super::*;

    fn ffi_option(key: &str, value: FFIArg<'static>) -> FFIArg<'static> {
        FFIArg::Vector(
            vec![FFIArg::StringV(key.into()), value]
                .into_iter()
                .collect(),
        )
    }

    #[test]
    fn ffi_options_decode_pairs_and_preserve_core_key_strings() {
        let options = parse_engine_options(FFIArg::Vector(
            vec![
                ffi_option("search.multiline", FFIArg::BoolV(true)),
                ffi_option("preview.wrap-text", FFIArg::BoolV(true)),
                ffi_option("window.size", FFIArg::NumV(0.7)),
                ffi_option(
                    "keys.search.results.move_down",
                    FFIArg::Vector(
                        vec![FFIArg::StringV("n".into()), FFIArg::StringV("down".into())]
                            .into_iter()
                            .collect(),
                    ),
                ),
            ]
            .into_iter()
            .collect(),
        ))
        .expect("options decode");

        assert!(options.run_config.multiline);
        assert!(options.config.preview.wrap_text);
        assert!((options.window_size - 0.7).abs() < f64::EPSILON);
        assert_eq!(
            options.config.keys.search.results.move_down[0].code,
            KeyCode::Char('n')
        );
        assert_eq!(
            options.config.keys.search.results.move_down[1].code,
            KeyCode::Down
        );
    }

    #[test]
    fn ffi_constructor_returns_core_conflicts_as_displayable_strings() {
        let fixture = tempdir().expect("fixture directory");
        let options = FFIArg::Vector(
            vec![ffi_option(
                "keys.general.quit",
                FFIArg::Vector(
                    vec![FFIArg::StringV("C-r".into())]
                        .into_iter()
                        .collect(),
                ),
            )]
            .into_iter()
            .collect(),
        );

        let result = scooter_engine_new(fixture.path().to_str().expect("UTF-8 path"), options);
        let FFIValue::StringV(error) = result else {
            panic!("conflicting options must return a Steel string");
        };
        assert!(error.contains("Key binding conflict detected!"), "{error}");
        assert!(error.contains("C-r"), "{error}");
    }

    #[test]
    fn pump_encodes_status_followed_by_open_file_actions() {
        let fixture = tempdir().expect("fixture directory");
        let expected_path = fixture.path().join("selected.txt");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        engine
            .app
            .event_channels
            .sender
            .send(Event::LaunchEditor((expected_path.clone(), 23)))
            .expect("engine event receiver lives");

        let response = scooter_pump(&mut engine);
        let FFIValue::Vector(response) = response else {
            panic!("pump response must be a list");
        };
        assert_eq!(response.len(), 2);
        assert!(matches!(
            &response[0],
            FFIValue::StringV(status) if status.as_str() == "rerender"
        ));
        let FFIValue::Vector(action) = &response[1] else {
            panic!("pump action must be a list");
        };
        assert_eq!(action.len(), 3);
        assert!(matches!(
            &action[0],
            FFIValue::StringV(name) if name.as_str() == "open-file"
        ));
        assert!(matches!(
            &action[1],
            FFIValue::StringV(path) if path.as_str() == expected_path.to_string_lossy()
        ));
        assert!(matches!(&action[2], FFIValue::IntV(23)));
    }

    #[test]
    fn ffi_panic_degrades_and_later_calls_remain_safe_across_the_render_grid() {
        assert!(matches!(scooter_test_panic(), FFIValue::Vector(values) if values.is_empty()));

        let fixture = tempdir().expect("fixture directory");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        let _ = scooter_handle_key(&mut engine, "h", 2);
        for (width, height) in [(0, 0), (1, 1), (20, 5), (80, 24), (160, 45)] {
            let _ = scooter_render(&mut engine, width, height);
            let _ = scooter_pump(&mut engine);
            let _ = scooter_busy(&engine);
        }

        let _ = scooter_handle_key(&mut engine, "esc", 0);
        let _ = scooter_handle_key(&mut engine, "m", 4);
        assert!(scooter_busy(&engine));
        for (width, height) in [(0, 0), (1, 1), (20, 5), (80, 24), (160, 45)] {
            let _ = scooter_render(&mut engine, width, height);
            let _ = scooter_pump(&mut engine);
            let _ = scooter_busy(&engine);
        }
    }

    #[test]
    fn ffi_calls_after_quit_are_safe_noops() {
        let fixture = tempdir().expect("fixture directory");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        scooter_quit(&mut engine);

        let FFIValue::Vector(handle_key) = scooter_handle_key(&mut engine, "a", 0) else {
            panic!("post-quit key response must be a list");
        };
        assert!(matches!(
            &handle_key[0],
            FFIValue::StringV(status) if status.as_str() == "rerender"
        ));
        let FFIValue::Vector(pump) = scooter_pump(&mut engine) else {
            panic!("post-quit pump response must be a list");
        };
        assert!(matches!(
            &pump[0],
            FFIValue::StringV(status) if status.as_str() == "idle"
        ));
        assert!(matches!(scooter_render(&mut engine, 120, 40), FFIValue::Vector(frame) if frame.is_empty()));
        assert!(!scooter_busy(&engine));
        assert!(matches!(scooter_cursor(&engine, 120, 40), FFIValue::BoolV(false)));
    }
}
