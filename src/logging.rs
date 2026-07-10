//! File-only logging for the dylib boundary.

use std::{path::Path, sync::Once};

use etcetera::base_strategy::{BaseStrategy, choose_base_strategy};
const APP_NAME: &str = "scooter.hx";
static INITIALISE: Once = Once::new();

/// Install the cheap, warning-level file logger once for the dylib process.
///
/// A logger may already be installed by Helix.  In that case `simple_log`
/// returns an error and the `log` facade remains a harmless no-op here rather
/// than ever writing to the terminal's stderr stream.
pub(crate) fn initialise() {
    INITIALISE.call_once(|| {
        let Ok(strategy) = choose_base_strategy() else {
            return;
        };
        let path = strategy.cache_dir().join(APP_NAME).join("scooter-hx.log");
        if make_parent_dir(&path).is_ok() {
            let _ = simple_log::file(path.to_string_lossy(), "warn", 100, 10);
        }
    });
}

fn make_parent_dir(path: &Path) -> std::io::Result<()> {
    if let Some(parent) = path.parent()
        && !parent.exists()
    {
        std::fs::create_dir_all(parent)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cache_log_path_is_nested_under_the_application_directory() {
        let strategy = choose_base_strategy().expect("cache base strategy");
        let path = strategy.cache_dir().join(APP_NAME).join("scooter-hx.log");
        assert_eq!(path.file_name().and_then(|name| name.to_str()), Some("scooter-hx.log"));
        assert_eq!(path.parent().and_then(|parent| parent.file_name()).and_then(|name| name.to_str()), Some(APP_NAME));
    }
}
