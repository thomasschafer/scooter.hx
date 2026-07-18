//! Steel-facing configuration translated into scooter-core's native types.

use std::{path::PathBuf, str::FromStr};

use scooter_core::{
    app::AppRunConfig,
    config::{Config, Keys},
    keyboard::KeyEvent,
};

pub(crate) const DEFAULT_WINDOW_SIZE: f64 = 0.9;

/// A `scooter-set!` option. This table is the single source of truth for the
/// public symbol, its FFI wire path, validation, defaults, and README docs.
#[derive(Debug, Clone, Copy)]
pub struct OptionSpec {
    /// Symbol accepted by `scooter-set!`, without its leading quote.
    pub symbol: &'static str,
    /// Internal `(key value)` path sent from Steel to Rust.
    pub wire_path: &'static str,
    /// Human-readable value type for the generated README.
    pub value_type: &'static str,
    /// Human-readable default for the generated README.
    pub default: &'static str,
    /// Concise user-facing description for the generated README.
    pub description: &'static str,
    default_value: DefaultValue,
    apply: fn(&mut EngineOptions, &str, &OptionValue) -> Result<(), String>,
}

/// A Helix-plugin-only binding accepted by `scooter-keys!`.
#[derive(Debug, Clone, Copy)]
pub struct PluginKeySpec {
    pub path: &'static str,
    pub default: &'static str,
    pub description: &'static str,
}

const PLUGIN_KEY_SPECS: &[PluginKeySpec] = &[PluginKeySpec {
    path: "plugin.open_in_editor_bg",
    default: "A-o",
    description: "Open the selected result in Helix without hiding Scooter.",
}, PluginKeySpec {
    path: "plugin.hide",
    default: "esc",
    description: "Hide Scooter when core has no action for the key in the current context; core bindings take precedence.",
}];

#[derive(Debug, Clone, Copy)]
enum DefaultValue {
    Bool(bool),
    Number(f64),
    RuntimeDiscovery,
}

const OPTION_SPECS: &[OptionSpec] = &[
    OptionSpec {
        symbol: "multiline",
        wire_path: "search.multiline",
        value_type: "boolean",
        default: "`#f`",
        description: "Allow search patterns to match across line boundaries.",
        default_value: DefaultValue::Bool(false),
        apply: set_multiline,
    },
    OptionSpec {
        symbol: "hidden",
        wire_path: "search.hidden",
        value_type: "boolean",
        default: "`#f`",
        description: "Include hidden files and directories.",
        default_value: DefaultValue::Bool(false),
        apply: set_hidden,
    },
    OptionSpec {
        symbol: "advanced-regex",
        wire_path: "search.advanced-regex",
        value_type: "boolean",
        default: "`#f`",
        description: "Enable Scooter's advanced regular-expression engine.",
        default_value: DefaultValue::Bool(false),
        apply: set_advanced_regex,
    },
    OptionSpec {
        symbol: "include-git-folders",
        wire_path: "search.include-git-folders",
        value_type: "boolean",
        default: "`#f`",
        description: "Search Git metadata directories as well as normal files.",
        default_value: DefaultValue::Bool(false),
        apply: set_include_git_folders,
    },
    OptionSpec {
        symbol: "escape-sequences",
        wire_path: "search.escape-sequences",
        value_type: "boolean",
        default: "`#f`",
        description: "Interpret `\\n`, `\\t`, and `\\\\` in replacement text.",
        default_value: DefaultValue::Bool(false),
        apply: set_escape_sequences,
    },
    OptionSpec {
        symbol: "wrap-text",
        wire_path: "preview.wrap-text",
        value_type: "boolean",
        default: "`#f`",
        description: "Wrap long preview lines.",
        default_value: DefaultValue::Bool(false),
        apply: set_wrap_text,
    },
    OptionSpec {
        symbol: "syntax-highlighting",
        wire_path: "preview.syntax-highlighting",
        value_type: "boolean",
        default: "`#t`",
        description: "Highlight preview context with Helix runtime grammars and your Helix theme, rendered on the editor background (a deliberate difference from the TUI).",
        default_value: DefaultValue::Bool(true),
        apply: set_syntax_highlighting,
    },
    OptionSpec {
        symbol: "window-size",
        wire_path: "window.size",
        value_type: "number, `0.5`–`1.0`",
        default: "`0.9`",
        description: "Set the window size as a terminal ratio.",
        default_value: DefaultValue::Number(DEFAULT_WINDOW_SIZE),
        apply: set_window_size,
    },
    OptionSpec {
        symbol: "runtime-dir",
        wire_path: "highlight.runtime-dir",
        value_type: "string path",
        default: "Helix runtime discovery",
        description: "Override the runtime used to load preview syntax grammars; otherwise discovery checks `HELIX_RUNTIME`, then Helix's config-directory runtime (`~/.config/helix/runtime`).",
        default_value: DefaultValue::RuntimeDiscovery,
        apply: set_runtime_dir,
    },
];

/// Return the shared, declarative `scooter-set!` option table.
pub fn option_specs() -> &'static [OptionSpec] {
    OPTION_SPECS
}

/// Return the plugin-only `scooter-keys!` entries for generated docs.
pub fn plugin_key_specs() -> &'static [PluginKeySpec] {
    PLUGIN_KEY_SPECS
}

/// Return the parser wire path for a public `scooter-set!` symbol.
pub(crate) fn setting_path(symbol: &str) -> Option<&'static str> {
    OPTION_SPECS
        .iter()
        .find(|spec| spec.symbol == symbol)
        .map(|spec| spec.wire_path)
}

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

    #[cfg(test)]
    pub(crate) fn string(key: &str, value: &str) -> Self {
        Self {
            key: key.to_string(),
            value: OptionValue::String(value.to_string()),
        }
    }
}

/// Values supported by Scooter's deliberately narrow FFI configuration wire format.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum OptionValue {
    Bool(bool),
    Number(f64),
    String(String),
    Strings(Vec<String>),
}

/// All configuration that is consumed when a new engine is constructed.
#[derive(Debug)]
pub(crate) struct EngineOptions {
    pub(crate) run_config: AppRunConfig,
    pub(crate) config: Config,
    pub(crate) window_size: f64,
    pub(crate) runtime_dir: Option<PathBuf>,
    pub(crate) syntax_highlighting: bool,
    /// Helix-only result action. This intentionally lives outside core's
    /// keymap so it can be validated against, rather than collide within, the
    /// core map.
    pub(crate) open_in_editor_bg: KeyEvent,
    /// Helix-only hide actions. Unlike background-open, these deliberately
    /// overlap core bindings: core wins in every context where it handles one.
    pub(crate) hide: Vec<KeyEvent>,
}

impl Default for EngineOptions {
    fn default() -> Self {
        let mut options = Self {
            run_config: AppRunConfig::default(),
            config: Config::default(),
            window_size: 0.0,
            runtime_dir: None,
            syntax_highlighting: false,
            open_in_editor_bg: "A-o"
                .parse()
                .expect("default background-open binding must be valid"),
            hide: vec!["esc".parse().expect("default hide binding must be valid")],
        };
        for spec in OPTION_SPECS {
            spec.apply_default(&mut options);
        }
        options
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
        if let Some(spec) = OPTION_SPECS.iter().find(|spec| spec.wire_path == key) {
            return (spec.apply)(self, &key, &value);
        }
        if key.starts_with("keys.") {
            self.apply_key_binding(&key, value)?;
        } else {
            return Err(format!("Unknown Scooter option '{key}'"));
        }
        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    fn apply_key_binding(&mut self, path: &str, value: OptionValue) -> Result<(), String> {
        let keys = key_bindings(path, value)?;
        match path {
            "keys.plugin.open_in_editor_bg" => {
                self.open_in_editor_bg = single_key_binding(path, &keys)?;
            }
            "keys.plugin.hide" => self.hide = keys.to_vec(),
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

impl OptionSpec {
    fn apply_default(self, options: &mut EngineOptions) {
        match self.default_value {
            DefaultValue::Bool(value) => {
                (self.apply)(options, self.wire_path, &OptionValue::Bool(value))
                    .expect("option table boolean default must be valid");
            }
            DefaultValue::Number(value) => {
                (self.apply)(options, self.wire_path, &OptionValue::Number(value))
                    .expect("option table number default must be valid");
            }
            DefaultValue::RuntimeDiscovery => {}
        }
    }
}

fn set_multiline(
    options: &mut EngineOptions,
    path: &str,
    value: &OptionValue,
) -> Result<(), String> {
    options.run_config.multiline = boolean(path, value)?;
    Ok(())
}

fn set_hidden(options: &mut EngineOptions, path: &str, value: &OptionValue) -> Result<(), String> {
    options.run_config.include_hidden = boolean(path, value)?;
    Ok(())
}

fn set_advanced_regex(
    options: &mut EngineOptions,
    path: &str,
    value: &OptionValue,
) -> Result<(), String> {
    options.run_config.advanced_regex = boolean(path, value)?;
    Ok(())
}

fn set_include_git_folders(
    options: &mut EngineOptions,
    path: &str,
    value: &OptionValue,
) -> Result<(), String> {
    options.run_config.include_git_folders = boolean(path, value)?;
    Ok(())
}

fn set_escape_sequences(
    options: &mut EngineOptions,
    path: &str,
    value: &OptionValue,
) -> Result<(), String> {
    options.run_config.interpret_escape_sequences = boolean(path, value)?;
    Ok(())
}

fn set_wrap_text(
    options: &mut EngineOptions,
    path: &str,
    value: &OptionValue,
) -> Result<(), String> {
    options.config.preview.wrap_text = boolean(path, value)?;
    Ok(())
}

fn set_syntax_highlighting(
    options: &mut EngineOptions,
    path: &str,
    value: &OptionValue,
) -> Result<(), String> {
    options.syntax_highlighting = boolean(path, value)?;
    Ok(())
}

fn set_window_size(
    options: &mut EngineOptions,
    path: &str,
    value: &OptionValue,
) -> Result<(), String> {
    options.window_size = window_size(path, value)?;
    Ok(())
}

fn set_runtime_dir(
    options: &mut EngineOptions,
    path: &str,
    value: &OptionValue,
) -> Result<(), String> {
    options.runtime_dir = Some(runtime_dir(path, value.clone())?);
    Ok(())
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

fn runtime_dir(path: &str, value: OptionValue) -> Result<PathBuf, String> {
    let OptionValue::String(value) = value else {
        return Err(format!(
            "Invalid value for '{path}': expected a string path"
        ));
    };
    if value.is_empty() {
        return Err(format!(
            "Invalid value for '{path}': expected a non-empty string path"
        ));
    }
    Ok(PathBuf::from(value))
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

fn single_key_binding(path: &str, bindings: &Keys) -> Result<KeyEvent, String> {
    match bindings.as_slice() {
        [binding] => Ok(*binding),
        _ => Err(format!(
            "Invalid value for '{path}': expected exactly one key binding"
        )),
    }
}

#[cfg(test)]
mod tests {
    use scooter_core::keyboard::{KeyCode, KeyModifiers};

    use super::{DEFAULT_WINDOW_SIZE, EngineOptions, OptionEntry, option_specs, setting_path};

    #[test]
    fn setting_symbols_and_wire_paths_share_one_table() {
        for specification in option_specs() {
            assert_eq!(
                setting_path(specification.symbol),
                Some(specification.wire_path)
            );
        }
        assert_eq!(setting_path("not-a-setting"), None);
    }

    #[test]
    fn defaults_leave_core_configuration_unchanged() {
        let options = EngineOptions::from_entries([]).expect("defaults parse");
        assert!(!options.run_config.multiline);
        assert!(!options.run_config.include_hidden);
        assert!(!options.config.preview.wrap_text);
        assert!(options.syntax_highlighting);
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
            OptionEntry::boolean("preview.syntax-highlighting", false),
            OptionEntry::number("window.size", 0.75),
            OptionEntry::string("highlight.runtime-dir", "/tmp/helix-runtime"),
        ])
        .expect("options parse");

        assert!(options.run_config.multiline);
        assert!(options.run_config.include_hidden);
        assert!(options.run_config.advanced_regex);
        assert!(options.run_config.include_git_folders);
        assert!(options.run_config.interpret_escape_sequences);
        assert!(options.config.preview.wrap_text);
        assert!(!options.syntax_highlighting);
        assert_eq!(
            options.runtime_dir.as_deref(),
            Some(std::path::Path::new("/tmp/helix-runtime"))
        );
        assert!((options.window_size - 0.75).abs() < f64::EPSILON);
    }

    #[test]
    fn every_documented_key_path_accepts_core_key_syntax() {
        let paths = [
            "keys.general.quit",
            "keys.plugin.open_in_editor_bg",
            "keys.plugin.hide",
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
            let bindings = if path == "keys.plugin.open_in_editor_bg" {
                &["C-o"][..]
            } else {
                &["C-o", "F12"][..]
            };
            let options = EngineOptions::from_entries([OptionEntry::keys(path, bindings)]);
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
        assert_eq!(options.open_in_editor_bg.to_string(), "A-o");
        assert_eq!(options.hide[0].to_string(), "esc");
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
