//! Runtime-owning bridge between Steel and `scooter_core::app::App`.

use std::path::PathBuf;

use scooter_core::{
    app::{
        App, AppRunConfig, Event, EventHandlingResult, FocussedSection, InputSource, Screen,
        SearchPhase,
    },
    config::Config,
    fields::SearchFieldValues,
    keyboard::KeyCode,
};
use tokio::runtime::{Builder, Runtime};

use crate::{key, view};

const DRAIN_LIMIT: usize = 1_000;

/// The single-thread-owned state for one Helix Scooter session.
pub(crate) struct ScooterEngine {
    runtime: Runtime,
    pub(crate) app: App,
}

impl ScooterEngine {
    pub(crate) fn new(directory: impl Into<PathBuf>) -> Result<Self, String> {
        let runtime = Builder::new_multi_thread()
            .worker_threads(2)
            .enable_all()
            .build()
            .map_err(|error| error.to_string())?;
        let _guard = runtime.enter();
        let app = App::new(
            InputSource::Directory(directory.into()),
            &SearchFieldValues::default(),
            AppRunConfig::default(),
            Config::default(),
        )
        .map_err(|error| error.to_string())?;
        Ok(Self { runtime, app })
    }

    pub(crate) fn handle_key(&mut self, code: &str, modifiers: usize) -> String {
        let Some(key_event) = key::decode(code, modifiers) else {
            return "rerender".to_string();
        };

        if key_event.code == KeyCode::Esc && self.should_hide_for_escape() {
            return "hide".to_string();
        }

        let result = {
            let _guard = self.runtime.enter();
            self.app.handle_key_event(key_event)
        };
        let status = if matches!(result, EventHandlingResult::Exit(_)) {
            let _guard = self.runtime.enter();
            self.app.cancel_in_progress_tasks();
            "quit"
        } else {
            "rerender"
        };

        self.drain_ready_events();
        status.to_string()
    }

    pub(crate) fn pump(&mut self) -> String {
        if self.drain_ready_events() {
            "rerender".to_string()
        } else {
            "idle".to_string()
        }
    }

    pub(crate) fn busy(&self) -> bool {
        self.app.toast_message().is_some()
            || match &self.app.ui_state.current_screen {
                Screen::PerformingReplacement(_) => true,
                Screen::SearchFields(state) => {
                    state.search_debounce_timer.is_some()
                        || state.preview_update_state.is_some()
                        || state.search_state.as_ref().is_some_and(|search_state| {
                            !matches!(
                                search_state.phase,
                                SearchPhase::Invalid | SearchPhase::Complete { .. }
                            )
                        })
                }
                Screen::Results(_) => false,
            }
    }

    pub(crate) fn render(&mut self, width: usize, height: usize) -> view::Frame {
        view::render(&mut self.app, width, height)
    }

    pub(crate) fn cursor(&self, width: usize, height: usize) -> Option<(usize, usize)> {
        view::cursor(&self.app, width, height)
    }

    pub(crate) fn reset(&mut self) {
        let _guard = self.runtime.enter();
        self.app.reset();
    }

    pub(crate) fn quit(&mut self) {
        let _guard = self.runtime.enter();
        self.app.cancel_in_progress_tasks();
    }

    fn should_hide_for_escape(&self) -> bool {
        !self.app.show_popup()
            && matches!(
                &self.app.ui_state.current_screen,
                Screen::SearchFields(state)
                    if state.focussed_section == FocussedSection::SearchFields
            )
    }

    fn drain_ready_events(&mut self) -> bool {
        let mut processed = false;

        for _ in 0..DRAIN_LIMIT {
            let Some(event) = self.app.event_channels.try_recv() else {
                break;
            };
            processed = true;
            match event {
                Event::Rerender => {}
                Event::Internal(event) => {
                    let _guard = self.runtime.enter();
                    let _ = self.app.handle_internal_event(event);
                }
                Event::LaunchEditor((path, line)) => {
                    eprintln!(
                        "scooter-hx: ignoring LaunchEditor until H3: {}:{line}",
                        path.display()
                    );
                }
                Event::ExitAndReplace(_) => {
                    eprintln!("scooter-hx: unexpected ExitAndReplace for directory input");
                }
            }
        }

        for _ in 0..DRAIN_LIMIT {
            let event = self
                .app
                .background_processing_reciever()
                .and_then(|receiver| receiver.try_recv().ok());
            let Some(event) = event else {
                break;
            };
            processed = true;
            let _guard = self.runtime.enter();
            let _ = self.app.handle_background_processing_event(event);
        }

        processed
    }
}

#[cfg(test)]
mod tests {
    use std::{
        fs, thread,
        time::{Duration, Instant},
    };

    use scooter_core::app::{FocussedSection, Screen, SearchPhase};
    use tempfile::tempdir;

    use super::ScooterEngine;

    #[test]
    fn headless_fields_renderer_reflects_search_checkbox_errors_and_collapse() {
        let fixture = tempdir().expect("fixture directory");
        fs::write(fixture.path().join("one.txt"), "alpha one\nalphabet one\n").expect("write one");
        fs::write(fixture.path().join("two.txt"), "alpha two\nalphabet two\n").expect("write two");
        fs::write(fixture.path().join("three.txt"), "alpha three\n").expect("write three");

        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        for character in "alpha".chars() {
            assert_eq!(engine.handle_key(&character.to_string(), 0), "rerender");
        }
        wait_until_complete(&mut engine);

        let search_state = search_state(&engine);
        assert_eq!(search_state.results.len(), 5);
        let rendered = joined_runs(&mut engine);
        assert!(rendered.contains("one.txt:1"));
        assert!(rendered.contains("two.txt:2"));
        assert!(rendered.contains("three.txt:1"));

        assert_eq!(engine.handle_key("tab", 0), "rerender");
        let active = engine
            .render(100, 36)
            .runs
            .into_iter()
            .filter(|run| run.tag == "active")
            .map(|run| run.text)
            .collect::<String>();
        assert!(active.contains("Replace text"));

        assert_eq!(engine.handle_key("tab", 0), "rerender");
        assert_eq!(engine.handle_key(" ", 0), "rerender");
        let checkbox_box = engine.render(100, 36);
        assert!(checkbox_box.runs.iter().any(|run| run.text == " X "));

        assert_eq!(engine.handle_key("enter", 0), "rerender");
        assert!(matches!(
            &engine.app.ui_state.current_screen,
            Screen::SearchFields(state)
                if state.focussed_section == FocussedSection::SearchResults
        ));
        let collapsed = joined_runs(&mut engine);
        assert!(collapsed.contains("Search text"));
        assert!(collapsed.contains("Replace text"));
        assert!(!collapsed.contains("Fixed strings"));

        let mut invalid_engine =
            ScooterEngine::new(fixture.path()).expect("invalid engine initialises");
        assert_eq!(invalid_engine.handle_key("(", 0), "rerender");
        wait_until_invalid_search(&mut invalid_engine);
        let invalid_frame = invalid_engine.render(100, 36);
        assert!(
            invalid_frame
                .runs
                .iter()
                .any(|run| run.tag == "error" && run.text.contains("(Error: "))
        );
        assert!(
            invalid_frame
                .runs
                .iter()
                .any(|run| run.tag == "error" && run.text.contains("Invalid search"))
        );
    }

    #[test]
    fn headless_results_preview_tracks_selection_markers_multiselect_and_wrapping() {
        let fixture = tempdir().expect("fixture directory");
        fs::write(
            fixture.path().join("matches.txt"),
            format!(
                "before one\nalpha first {}\nbetween results\nalpha second result\nafter second\n",
                "very-long-context ".repeat(16)
            ),
        )
        .expect("write fixture");

        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        for character in "alpha".chars() {
            engine.handle_key(&character.to_string(), 0);
        }
        wait_until_complete(&mut engine);
        assert_eq!(engine.handle_key("tab", 0), "rerender");
        for character in "OMEGA".chars() {
            engine.handle_key(&character.to_string(), 0);
        }
        wait_until_preview_updated(&mut engine);
        assert_eq!(engine.handle_key("enter", 0), "rerender");

        let initial = rendered_rows(&mut engine, 160, 45).join("\n");
        assert!(initial.contains("(1) before one"));
        assert!(initial.contains("- alpha first"));
        assert!(initial.contains("+ OMEGA first"));

        engine.handle_key("j", 0);
        let second = rendered_rows(&mut engine, 160, 45).join("\n");
        assert!(second.contains("+ OMEGA second result"));
        engine.handle_key("k", 0);
        let first = rendered_rows(&mut engine, 160, 45).join("\n");
        assert!(first.contains("+ OMEGA first"));

        engine.handle_key("j", 0);
        engine.handle_key(" ", 0);
        let toggled = rendered_rows(&mut engine, 160, 45).join("\n");
        assert!(
            toggled
                .lines()
                .any(|line| line.contains("matches.txt:4") && line.contains("[ ]"))
        );
        engine.handle_key("a", 0);
        let after_toggle_all = rendered_rows(&mut engine, 160, 45).join("\n");
        assert!(
            after_toggle_all
                .lines()
                .filter(|line| line.contains("matches.txt:"))
                .all(|line| line.contains("[x]"))
        );

        engine.handle_key("k", 0);
        engine.handle_key("v", 0);
        engine.handle_key("j", 0);
        let multiselect = engine.render(160, 45);
        assert!(
            multiselect.runs.iter().any(|run| {
                run.tag == "selection-secondary" && run.text.contains("matches.txt")
            })
        );
        engine.handle_key("esc", 0);
        engine.handle_key("k", 0);

        let unwrapped = rendered_rows(&mut engine, 160, 45).join("\n");
        assert!(!unwrapped.contains("↪ "));
        engine.handle_key("l", 2);
        let wrapped = rendered_rows(&mut engine, 160, 45).join("\n");
        assert!(wrapped.contains("↪ "));
    }

    fn wait_until_complete(engine: &mut ScooterEngine) {
        let deadline = Instant::now() + Duration::from_secs(10);
        while Instant::now() < deadline {
            let _ = engine.pump();
            if search_complete(engine) {
                return;
            }
            thread::sleep(Duration::from_millis(10));
        }
        panic!("search did not complete");
    }

    fn search_state(engine: &ScooterEngine) -> &scooter_core::app::SearchState {
        let Screen::SearchFields(state) = &engine.app.ui_state.current_screen else {
            panic!("search left the fields screen");
        };
        state.search_state.as_ref().expect("search state")
    }

    fn search_complete(engine: &ScooterEngine) -> bool {
        matches!(
            &engine.app.ui_state.current_screen,
            Screen::SearchFields(state)
                if matches!(
                    state.search_state.as_ref().map(|search| search.phase),
                    Some(SearchPhase::Complete { .. })
                )
        )
    }

    fn wait_until_preview_updated(engine: &mut ScooterEngine) {
        let deadline = Instant::now() + Duration::from_secs(10);
        while Instant::now() < deadline {
            let _ = engine.pump();
            if engine.app.is_preview_updated() {
                return;
            }
            thread::sleep(Duration::from_millis(10));
        }
        panic!("replacement previews did not finish updating");
    }

    fn wait_until_invalid_search(engine: &mut ScooterEngine) {
        let deadline = Instant::now() + Duration::from_secs(10);
        while Instant::now() < deadline {
            let _ = engine.pump();
            if matches!(
                &engine.app.ui_state.current_screen,
                Screen::SearchFields(_)
                    if engine.app.search_fields.fields[0].error().is_some()
            ) {
                return;
            }
            thread::sleep(Duration::from_millis(10));
        }
        panic!("invalid search was not rendered");
    }

    fn joined_runs(engine: &mut ScooterEngine) -> String {
        engine
            .render(100, 36)
            .runs
            .into_iter()
            .map(|run| run.text)
            .collect()
    }

    fn rendered_rows(engine: &mut ScooterEngine, width: usize, height: usize) -> Vec<String> {
        let mut rows = std::collections::BTreeMap::<usize, Vec<_>>::new();
        for run in engine.render(width, height).runs {
            rows.entry(run.y).or_default().push(run);
        }
        rows.into_values()
            .map(|mut row| {
                row.sort_by_key(|run| run.x);
                let mut rendered = String::new();
                for run in row {
                    let current_width = rendered.chars().count();
                    if run.x > current_width {
                        rendered.push_str(&" ".repeat(run.x - current_width));
                    }
                    rendered.push_str(&run.text);
                }
                rendered
            })
            .collect()
    }
}
