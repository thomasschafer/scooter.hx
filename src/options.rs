//! Steel-facing configuration translated into scooter-core's native types.

use std::str::FromStr;

use scooter_core::{
    app::AppRunConfig,
    config::{Config, Keys},
    keyboard::KeyEvent,
};

pub(crate) const DEFAULT_WINDOW_SIZE: f64 = 0.9;

/// One already-decoded `(key value)` entry from Steel.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct OptionEntry {
    pub(crate) key: String,
    pub(crate) value: OptionValue,
}

impl OptionEntry {
    #[cfg(test)]
    pub(crate) fn boolean(key: &str, value: bool) -> Self {
        Self {
            key: key.to_string(),
            value: OptionValue::Bool(value),
        }
    }

    #[cfg(test)]
    pub(crate) fn number(key: &str, value: f64) -> Self {
        Self {
            key: key.to_string(),
            value: OptionValue::Number(value),
        }
    }

    #[cfg(test)]
    pub(crate) fn keys(key: &str, value: &[&str]) -> Self {
        Self {
            key: key.to_string(),
            value: OptionValue::Strings(value.iter().map(ToString::to_string).collect()),
        }
    }
}

/// Values supported by Scooter's deliberately narrow FFI configuration wire format.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum OptionValue {
    Bool(bool),
    Number(f64),
    Strings(Vec<String>),
}

/// All configuration that is consumed when a new engine is constructed.
#[derive(Debug)]
pub(crate) struct EngineOptions {
    pub(crate) run_config: AppRunConfig,
    pub(crate) config: Config,
    pub(crate) window_size: f64,
}

impl Default for EngineOptions {
    fn default() -> Self {
        Self {
            run_config: AppRunConfig::default(),
            config: Config::default(),
            window_size: DEFAULT_WINDOW_SIZE,
        }
    }
}

impl EngineOptions {
    /// Apply entries in order, matching Steel's ordinary last-setting-wins semantics.
    pub(crate) fn from_entries(
        entries: impl IntoIterator<Item = OptionEntry>,
    ) -> Result<Self, String> {
        let mut options = Self::default();
        for entry in entries {
            options.apply(entry)?;
        }
        Ok(options)
    }

    fn apply(&mut self, entry: OptionEntry) -> Result<(), String> {
        let OptionEntry { key, value } = entry;
        match key.as_str() {
            "search.multiline" => self.run_config.multiline = boolean(&key, &value)?,
            "search.hidden" => self.run_config.include_hidden = boolean(&key, &value)?,
            "search.advanced-regex" => self.run_config.advanced_regex = boolean(&key, &value)?,
            "search.include-git-folders" => {
                self.run_config.include_git_folders = boolean(&key, &value)?;
            }
            "search.escape-sequences" => {
                self.run_config.interpret_escape_sequences = boolean(&key, &value)?;
            }
            "preview.wrap-text" => self.config.preview.wrap_text = boolean(&key, &value)?,
            "window.size" => self.window_size = window_size(&key, &value)?,
            _ if key.starts_with("keys.") => self.apply_key_binding(&key, value)?,
            _ => return Err(format!("Unknown Scooter option '{key}'")),
        }
        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    fn apply_key_binding(&mut self, path: &str, value: OptionValue) -> Result<(), String> {
        let keys = key_bindings(path, value)?;
        match path {
            "keys.general.quit" => self.config.keys.general.quit = keys,
            "keys.general.reset" => self.config.keys.general.reset = keys,
            "keys.general.show_help_menu" => self.config.keys.general.show_help_menu = keys,

            "keys.search.toggle_preview_wrapping" => {
                self.config.keys.search.toggle_preview_wrapping = keys;
            }
            "keys.search.toggle_hidden_files" => self.config.keys.search.toggle_hidden_files = keys,
            "keys.search.toggle_multiline" => self.config.keys.search.toggle_multiline = keys,
            "keys.search.toggle_interpret_escape_sequences" => {
                self.config.keys.search.toggle_interpret_escape_sequences = keys;
            }

            "keys.search.fields.unlock_prepopulated_fields" => {
                self.config.keys.search.fields.unlock_prepopulated_fields = keys;
            }
            "keys.search.fields.trigger_search" => {
                self.config.keys.search.fields.trigger_search = keys;
            }
            "keys.search.fields.focus_next_field" => {
                self.config.keys.search.fields.focus_next_field = keys;
            }
            "keys.search.fields.focus_previous_field" => {
                self.config.keys.search.fields.focus_previous_field = keys;
            }

            "keys.search.results.trigger_replacement" => {
                self.config.keys.search.results.trigger_replacement = keys;
            }
            "keys.search.results.back_to_fields" => {
                self.config.keys.search.results.back_to_fields = keys;
            }
            "keys.search.results.open_in_editor" => {
                self.config.keys.search.results.open_in_editor = keys;
            }
            "keys.search.results.move_down" => self.config.keys.search.results.move_down = keys,
            "keys.search.results.move_up" => self.config.keys.search.results.move_up = keys,
            "keys.search.results.move_down_half_page" => {
                self.config.keys.search.results.move_down_half_page = keys;
            }
            "keys.search.results.move_up_half_page" => {
                self.config.keys.search.results.move_up_half_page = keys;
            }
            "keys.search.results.move_down_full_page" => {
                self.config.keys.search.results.move_down_full_page = keys;
            }
            "keys.search.results.move_up_full_page" => {
                self.config.keys.search.results.move_up_full_page = keys;
            }
            "keys.search.results.move_top" => self.config.keys.search.results.move_top = keys,
            "keys.search.results.move_bottom" => {
                self.config.keys.search.results.move_bottom = keys;
            }
            "keys.search.results.toggle_selected_inclusion" => {
                self.config.keys.search.results.toggle_selected_inclusion = keys;
            }
            "keys.search.results.toggle_all_selected" => {
                self.config.keys.search.results.toggle_all_selected = keys;
            }
            "keys.search.results.toggle_multiselect_mode" => {
                self.config.keys.search.results.toggle_multiselect_mode = keys;
            }
            "keys.search.results.flip_multiselect_direction" => {
                self.config.keys.search.results.flip_multiselect_direction = keys;
            }

            "keys.results.scroll_errors_down" => self.config.keys.results.scroll_errors_down = keys,
            "keys.results.scroll_errors_up" => self.config.keys.results.scroll_errors_up = keys,
            "keys.results.quit" => self.config.keys.results.quit = keys,
            _ => return Err(format!("Unknown Scooter key binding '{path}'")),
        }
        Ok(())
    }
}

fn boolean(path: &str, value: &OptionValue) -> Result<bool, String> {
    match value {
        OptionValue::Bool(value) => Ok(*value),
        _ => Err(format!("Invalid value for '{path}': expected a boolean")),
    }
}

fn window_size(path: &str, value: &OptionValue) -> Result<f64, String> {
    let OptionValue::Number(value) = value else {
        return Err(format!(
            "Invalid value for '{path}': expected a number from 0.5 to 1.0"
        ));
    };
    if !value.is_finite() || !(0.5..=1.0).contains(value) {
        return Err(format!(
            "Invalid value for '{path}': expected a number from 0.5 to 1.0"
        ));
    }
    Ok(*value)
}

fn key_bindings(path: &str, value: OptionValue) -> Result<Keys, String> {
    let OptionValue::Strings(bindings) = value else {
        return Err(format!(
            "Invalid value for '{path}': expected a list of key strings"
        ));
    };
    let bindings = bindings
        .into_iter()
        .map(|binding| {
            KeyEvent::from_str(&binding)
                .map_err(|error| format!("Invalid key binding '{binding}' for '{path}': {error}"))
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Keys::new(bindings))
}

#[cfg(test)]
mod tests {
    use scooter_core::keyboard::{KeyCode, KeyModifiers};

    use super::{DEFAULT_WINDOW_SIZE, EngineOptions, OptionEntry};

    #[test]
    fn defaults_leave_core_configuration_unchanged() {
        let options = EngineOptions::from_entries([]).expect("defaults parse");
        assert!(!options.run_config.multiline);
        assert!(!options.run_config.include_hidden);
        assert!(!options.config.preview.wrap_text);
        assert!((options.window_size - DEFAULT_WINDOW_SIZE).abs() < f64::EPSILON);
        assert!(
            options
                .config
                .keys
                .search
                .results
                .move_down
                .iter()
                .any(|key| key.code == KeyCode::Char('j'))
        );
    }

    #[test]
    fn behaviour_and_window_options_are_applied() {
        let options = EngineOptions::from_entries([
            OptionEntry::boolean("search.multiline", true),
            OptionEntry::boolean("search.hidden", true),
            OptionEntry::boolean("search.advanced-regex", true),
            OptionEntry::boolean("search.include-git-folders", true),
            OptionEntry::boolean("search.escape-sequences", true),
            OptionEntry::boolean("preview.wrap-text", true),
            OptionEntry::number("window.size", 0.75),
        ])
        .expect("options parse");

        assert!(options.run_config.multiline);
        assert!(options.run_config.include_hidden);
        assert!(options.run_config.advanced_regex);
        assert!(options.run_config.include_git_folders);
        assert!(options.run_config.interpret_escape_sequences);
        assert!(options.config.preview.wrap_text);
        assert!((options.window_size - 0.75).abs() < f64::EPSILON);
    }

    #[test]
    fn every_documented_key_path_accepts_core_key_syntax() {
        let paths = [
            "keys.general.quit",
            "keys.general.reset",
            "keys.general.show_help_menu",
            "keys.search.toggle_preview_wrapping",
            "keys.search.toggle_hidden_files",
            "keys.search.toggle_multiline",
            "keys.search.toggle_interpret_escape_sequences",
            "keys.search.fields.unlock_prepopulated_fields",
            "keys.search.fields.trigger_search",
            "keys.search.fields.focus_next_field",
            "keys.search.fields.focus_previous_field",
            "keys.search.results.trigger_replacement",
            "keys.search.results.back_to_fields",
            "keys.search.results.open_in_editor",
            "keys.search.results.move_down",
            "keys.search.results.move_up",
            "keys.search.results.move_down_half_page",
            "keys.search.results.move_up_half_page",
            "keys.search.results.move_down_full_page",
            "keys.search.results.move_up_full_page",
            "keys.search.results.move_top",
            "keys.search.results.move_bottom",
            "keys.search.results.toggle_selected_inclusion",
            "keys.search.results.toggle_all_selected",
            "keys.search.results.toggle_multiselect_mode",
            "keys.search.results.flip_multiselect_direction",
            "keys.results.scroll_errors_down",
            "keys.results.scroll_errors_up",
            "keys.results.quit",
        ];

        for path in paths {
            let options = EngineOptions::from_entries([OptionEntry::keys(path, &["C-o", "F12"])]);
            assert!(options.is_ok(), "{path} must be accepted");
        }

        let options = EngineOptions::from_entries([OptionEntry::keys(
            "keys.search.results.move_down",
            &["n", "down"],
        )])
        .expect("keys parse");
        assert_eq!(
            options.config.keys.search.results.move_down[0].code,
            KeyCode::Char('n')
        );
        assert_eq!(
            options.config.keys.search.results.move_down[1].modifiers,
            KeyModifiers::NONE
        );
    }

    #[test]
    fn invalid_values_and_paths_have_actionable_errors() {
        let invalid_bool =
            EngineOptions::from_entries([OptionEntry::number("search.multiline", 1.0)])
                .expect_err("number is not a boolean");
        assert_eq!(
            invalid_bool,
            "Invalid value for 'search.multiline': expected a boolean"
        );

        let invalid_size = EngineOptions::from_entries([OptionEntry::number("window.size", 0.49)])
            .expect_err("size is too small");
        assert!(invalid_size.contains("0.5 to 1.0"));

        let invalid_key = EngineOptions::from_entries([OptionEntry::keys(
            "keys.search.results.move_down",
            &["totally-invalid"],
        )])
        .expect_err("key must parse");
        assert!(invalid_key.contains("Invalid key binding 'totally-invalid'"));

        let unknown = EngineOptions::from_entries([OptionEntry::boolean("search.unknown", true)])
            .expect_err("path must be known");
        assert_eq!(unknown, "Unknown Scooter option 'search.unknown'");
    }
}
