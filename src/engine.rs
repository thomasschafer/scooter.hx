//! Runtime-owning bridge between Steel and `scooter_core::app::App`.

use std::{collections::VecDeque, mem, path::PathBuf};

use scooter_core::{
    app::{
        App, BackgroundProcessingEvent, Event, EventHandlingResult, FocussedSection,
        InputSource, InternalEvent, Screen, SearchPhase,
    },
    fields::SearchFieldValues,
    keyboard::{KeyCode, KeyEvent, KeyModifiers},
};
use tokio::runtime::{Builder, Runtime};

use crate::{key, options::EngineOptions, view};

const DRAIN_LIMIT: usize = 1_000;

/// A deferred request for the Steel layer to perform a Helix action.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum EngineAction {
    OpenFile { path: PathBuf, line: usize },
    OpenFileBackground { path: PathBuf, line: usize },
    ReloadDocuments,
}

/// The result returned after a key dispatch or a background-event pump.
///
/// Actions are deliberately batched with the status so Steel can consume one
/// simple list at the FFI boundary. The queue is drained only into this value,
/// never dropped when the event drain reaches [`DRAIN_LIMIT`].
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct EngineResponse {
    pub(crate) status: &'static str,
    pub(crate) actions: Vec<EngineAction>,
}

impl EngineResponse {
    fn new(status: &'static str, actions: Vec<EngineAction>) -> Self {
        Self { status, actions }
    }
}

// Retain concise existing status assertions while tests also inspect the
// queued actions explicitly where that is the behaviour under test.
impl PartialEq<&str> for EngineResponse {
    fn eq(&self, other: &&str) -> bool {
        self.status == *other
    }
}

/// The single-thread-owned state for one Helix Scooter session.
pub(crate) struct ScooterEngine {
    // Taking the runtime lets `quit` use `shutdown_background` rather than
    // Runtime's blocking Drop implementation on Helix's UI thread.
    runtime: Option<Runtime>,
    pub(crate) app: App,
    window_size: f64,
    actions: VecDeque<EngineAction>,
}

impl ScooterEngine {
    #[cfg(test)]
    pub(crate) fn new(directory: impl Into<PathBuf>) -> Result<Self, String> {
        Self::new_with_options(directory, EngineOptions::default())
    }

    pub(crate) fn new_with_options(
        directory: impl Into<PathBuf>,
        options: EngineOptions,
    ) -> Result<Self, String> {
        let runtime = Builder::new_multi_thread()
            .worker_threads(2)
            .enable_all()
            .build()
            .map_err(|error| error.to_string())?;
        let _guard = runtime.enter();
        let app = App::new(
            InputSource::Directory(directory.into()),
            &SearchFieldValues::default(),
            options.run_config,
            options.config,
        )
        .map_err(|error| error.to_string())?;
        Ok(Self {
            runtime: Some(runtime),
            app,
            window_size: options.window_size,
            actions: VecDeque::new(),
        })
    }

    pub(crate) const fn window_size(&self) -> f64 {
        self.window_size
    }

    pub(crate) fn handle_key(&mut self, code: &str, modifiers: usize) -> EngineResponse {
        if self.active_runtime().is_none() {
            return self.response("rerender");
        }

        let Some(key_event) = key::decode(code, modifiers) else {
            return self.response("rerender");
        };

        if key_event.code == KeyCode::Esc && self.should_hide_for_escape() {
            self.drain_ready_events(false);
            return self.response("hide");
        }
        if key_event.code == KeyCode::Esc && self.should_ignore_escape() {
            // The TUI does nothing for Escape while replacement work/results
            // are on screen.  Keep that behaviour native rather than letting
            // core's legacy Escape compatibility popup leak into Helix.
            self.drain_ready_events(false);
            return self.response("rerender");
        }
        if self.should_ignore_background_open_key(key_event) {
            // Alt plus a potential background-open character is never text
            // input. It is intentionally a no-op until results are focussed.
            self.drain_ready_events(false);
            return self.response("rerender");
        }

        // Background open is a Helix-only addition.  Re-submit the configured
        // foreground binding to core so it retains ownership of selection and
        // launch semantics; only the deferred action's tag differs.
        let background_open = self.background_open_key(key_event);
        let key_event = background_open.unwrap_or(key_event);

        let result = {
            let Some(runtime) = self.runtime.as_ref() else {
                return self.response("rerender");
            };
            let _guard = runtime.enter();
            self.app.handle_key_event(key_event)
        };
        let status = if matches!(result, EventHandlingResult::Exit(_)) {
            if let Some(runtime) = self.runtime.as_ref() {
                let _guard = runtime.enter();
                self.app.cancel_in_progress_tasks();
            }
            "quit"
        } else {
            "rerender"
        };

        self.drain_ready_events(background_open.is_some());
        self.response(status)
    }

    pub(crate) fn pump(&mut self) -> EngineResponse {
        if self.active_runtime().is_none() {
            return self.response("idle");
        }

        let status = if self.drain_ready_events(false) {
            "rerender"
        } else {
            "idle"
        };
        self.response(status)
    }

    pub(crate) fn busy(&self) -> bool {
        if self.active_runtime().is_none() {
            return false;
        }

        self.app.toast_message().is_some()
            || match &self.app.ui_state.current_screen {
                Screen::PerformingReplacement(_) => true,
                Screen::SearchFields(state) => {
                    state.search_debounce_timer.is_some()
                        || (state.preview_update_state.is_some() && !self.app.is_preview_updated())
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
        if self.active_runtime().is_none() {
            return view::Frame::default();
        }
        view::render(&mut self.app, width, height)
    }

    pub(crate) fn cursor(&self, width: usize, height: usize) -> Option<(usize, usize)> {
        self.active_runtime()?;
        view::cursor(&self.app, width, height)
    }

    pub(crate) fn reset(&mut self) {
        if let Some(runtime) = self.active_runtime() {
            let _guard = runtime.enter();
            self.app.reset();
        }
        // A reset starts a genuinely fresh session, including no deferred
        // editor launch from the state that was just discarded.
        self.actions.clear();
    }

    pub(crate) fn quit(&mut self) {
        if let Some(runtime) = self.active_runtime() {
            let _guard = runtime.enter();
            self.app.cancel_in_progress_tasks();
        }

        if let Some(runtime) = self.runtime.take() {
            runtime.shutdown_background();
        }
    }

    fn should_hide_for_escape(&self) -> bool {
        !self.app.show_popup()
            && matches!(
                &self.app.ui_state.current_screen,
                Screen::SearchFields(state)
                    if state.focussed_section == FocussedSection::SearchFields
            )
    }

    fn should_ignore_escape(&self) -> bool {
        !self.app.show_popup()
            && matches!(
                self.app.ui_state.current_screen,
                Screen::PerformingReplacement(_) | Screen::Results(_)
            )
    }

    fn active_runtime(&self) -> Option<&Runtime> {
        self.runtime.as_ref()
    }

    /// Return the configured foreground open binding when `key_event` is its
    /// Alt-modified plain-character counterpart in the focussed results list.
    /// Non-character and already-modified bindings deliberately have no
    /// background-open shorthand.
    fn background_open_key(&self, key_event: KeyEvent) -> Option<KeyEvent> {
        let Screen::SearchFields(state) = &self.app.ui_state.current_screen else {
            return None;
        };
        if state.focussed_section != FocussedSection::SearchResults {
            return None;
        }

        self.configured_background_open_key(key_event)
    }

    fn should_ignore_background_open_key(&self, key_event: KeyEvent) -> bool {
        matches!(
            &self.app.ui_state.current_screen,
            Screen::SearchFields(state)
                if state.focussed_section == FocussedSection::SearchFields
        ) && self.configured_background_open_key(key_event).is_some()
    }

    fn configured_background_open_key(&self, key_event: KeyEvent) -> Option<KeyEvent> {
        let configured = self
            .app
            .config
            .keys
            .search
            .results
            .open_in_editor
            .first()
            .copied()?;
        if !matches!(configured.code, KeyCode::Char(_))
            || configured.modifiers != KeyModifiers::NONE
            || key_event.code != configured.code
            || key_event.modifiers != KeyModifiers::ALT
        {
            return None;
        }

        Some(configured)
    }

    fn drain_ready_events(&mut self, tag_launches_as_background: bool) -> bool {
        if self.active_runtime().is_none() {
            return false;
        }
        let mut processed = false;

        for _ in 0..DRAIN_LIMIT {
            let Some(event) = self.app.event_channels.try_recv() else {
                break;
            };
            processed = true;
            match event {
                Event::Rerender => {}
                Event::Internal(event) => {
                    let replacement_completed = matches!(
                        &event,
                        InternalEvent::Background(BackgroundProcessingEvent::ReplacementCompleted(_))
                    );
                    if let Some(runtime) = self.runtime.as_ref() {
                        let _guard = runtime.enter();
                        let result = self.app.handle_internal_event(event);
                        if replacement_completed
                            && matches!(result, EventHandlingResult::Rerender)
                        {
                            self.actions.push_back(EngineAction::ReloadDocuments);
                        }
                    }
                }
                Event::LaunchEditor((path, line)) => {
                    self.actions.push_back(if tag_launches_as_background {
                        EngineAction::OpenFileBackground { path, line }
                    } else {
                        EngineAction::OpenFile { path, line }
                    });
                }
                Event::ExitAndReplace(_) => {
                    log::warn!("scooter-hx: unexpected ExitAndReplace for directory input");
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
            let replacement_completed =
                matches!(&event, BackgroundProcessingEvent::ReplacementCompleted(_));
            if let Some(runtime) = self.runtime.as_ref() {
                let _guard = runtime.enter();
                let result = self.app.handle_background_processing_event(event);
                if replacement_completed && matches!(result, EventHandlingResult::Rerender) {
                    self.actions.push_back(EngineAction::ReloadDocuments);
                }
            }
        }

        processed
    }

    fn response(&mut self, status: &'static str) -> EngineResponse {
        EngineResponse::new(status, mem::take(&mut self.actions).into())
    }
}

impl Drop for ScooterEngine {
    fn drop(&mut self) {
        self.quit();
    }
}

#[cfg(test)]
mod tests {
    use std::{
        fs,
        path::PathBuf,
        sync::{
            Arc,
            atomic::{AtomicBool, AtomicUsize},
        },
        thread,
        time::{Duration, Instant},
    };

    use crate::view::StyleTag;
    use scooter_core::{
        app::{BackgroundProcessingEvent, Event, FocussedSection, Popup, Screen, SearchPhase},
        line_reader::LineEnding,
        replace::{PerformingReplacementState, ReplaceResult, ReplaceState},
        search::{SearchResult, SearchResultWithReplacement},
    };
    use tempfile::tempdir;
    use tokio::sync::mpsc;

    use crate::options::{EngineOptions, OptionEntry};

    use super::{DRAIN_LIMIT, EngineAction, ScooterEngine};

    #[test]
    fn engine_creation_returns_core_key_conflict_errors() {
        let fixture = tempdir().expect("fixture directory");
        let options =
            EngineOptions::from_entries([OptionEntry::keys("keys.general.quit", &["C-r"])])
                .expect("options parse");

        let Err(error) = ScooterEngine::new_with_options(fixture.path(), options) else {
            panic!("conflicting bindings must reject engine creation");
        };
        assert!(error.contains("C-r"), "{error}");
        assert!(error.to_lowercase().contains("conflict"), "{error}");
    }

    #[test]
    fn engine_exposes_its_configured_window_size() {
        let fixture = tempdir().expect("fixture directory");
        let options = EngineOptions::from_entries([OptionEntry::number("window.size", 0.7)])
            .expect("options parse");
        let engine =
            ScooterEngine::new_with_options(fixture.path(), options).expect("engine initialises");
        assert!((engine.window_size() - 0.7).abs() < f64::EPSILON);
    }

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
            .filter(|run| run.tag == StyleTag::Active)
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
                .any(|run| run.tag == StyleTag::Error && run.text.contains("(Error: "))
        );
        assert!(
            invalid_frame
                .runs
                .iter()
                .any(|run| run.tag == StyleTag::Error && run.text.contains("Invalid search"))
        );
    }

    #[test]
    #[allow(clippy::too_many_lines)]
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

        let fields_focussed = engine.render(160, 45);
        assert!(
            !fields_focussed.runs.iter().any(|run| {
                matches!(
                    run.tag,
                    StyleTag::Selection
                        | StyleTag::SelectionSecondary
                        | StyleTag::SelectionExcluded
                        | StyleTag::SelectionSecondaryExcluded
                )
            }),
            "result rows must not be highlighted while fields are focussed"
        );
        let first_index = fields_focussed
            .runs
            .iter()
            .find(|run| run.text == " (1)")
            .expect("first result index is rendered");
        assert_eq!(first_index.tag, StyleTag::Info);
        assert!(
            fields_focussed
                .runs
                .iter()
                .any(|run| run.text == "[x] " && run.tag == StyleTag::Info)
        );
        assert!(
            fields_focussed
                .runs
                .iter()
                .any(|run| run.text == "matches.txt" && run.tag == StyleTag::Text)
        );
        assert!(
            fields_focussed
                .runs
                .iter()
                .any(|run| run.text == ":2" && run.tag == StyleTag::Info)
        );
        // A 160-column frame has a 144-column content block beginning at x=8;
        // its wide results list is floor((144 - 1) * 2 / 5) = 57 cells.
        assert_eq!(first_index.x + first_index.text.len(), 8 + 57);

        assert_eq!(engine.handle_key("tab", 0), "rerender");
        for character in "OMEGA".chars() {
            engine.handle_key(&character.to_string(), 0);
        }
        wait_until_preview_updated(&mut engine);
        assert_eq!(engine.handle_key("enter", 0), "rerender");

        let initial = rendered_rows(&mut engine, 160, 45).join("\n");
        assert!(initial.contains("  before one"));
        assert!(!initial.contains("(1) before one"));
        assert!(initial.contains("- alpha first"));
        assert!(initial.contains("+ OMEGA first"));
        let focussed = engine.render(160, 45);
        assert!(
            focussed
                .runs
                .iter()
                .any(|run| {
                    run.tag == StyleTag::Selection && run.x == 8 && run.text == " ".repeat(57)
                })
        );
        assert!(
            focussed
                .runs
                .iter()
                .any(|run| run.tag == StyleTag::Selection && run.text == " (1)")
        );

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
        let excluded_primary = engine.render(160, 45);
        assert!(
            excluded_primary
                .runs
                .iter()
                .any(|run| run.tag == StyleTag::SelectionExcluded && run.text == "[ ] ")
        );

        engine.handle_key("v", 0);
        engine.handle_key("k", 0);
        let excluded_range = engine.render(160, 45);
        assert!(
            excluded_range
                .runs
                .iter()
                .any(|run| {
                    run.tag == StyleTag::SelectionSecondaryExcluded && run.text == "[ ] "
                })
        );
        engine.handle_key("esc", 0);

        engine.handle_key("a", 0);
        let after_toggle_all = rendered_rows(&mut engine, 160, 45).join("\n");
        assert!(
            after_toggle_all
                .lines()
                .filter(|line| line.contains("matches.txt:"))
                .all(|line| line.contains("[x]"))
        );

        engine.handle_key("v", 0);
        engine.handle_key("j", 0);
        let multiselect = engine.render(160, 45);
        assert!(
            multiselect.runs.iter().any(|run| {
                run.tag == StyleTag::SelectionSecondary && run.text.contains("matches.txt")
            })
        );
        engine.handle_key("esc", 0);
        engine.handle_key("k", 0);

        let unwrapped = rendered_rows(&mut engine, 160, 45).join("\n");
        assert!(!unwrapped.contains("↪ "));
        engine.handle_key("l", 2);
        let wrapped = rendered_rows(&mut engine, 160, 45).join("\n");
        assert!(wrapped.contains("  ↪ "));
        assert!(
            engine
                .render(160, 45)
                .runs
                .iter()
                .any(|run| run.tag == StyleTag::Dim && run.text == "  ↪ ")
        );
        assert!(wrapped.lines().count() > unwrapped.lines().count());
    }

    #[test]
    fn headless_help_toast_and_replacement_flow_render_and_mutate_files() {
        let fixture = tempdir().expect("fixture directory");
        let file = fixture.path().join("matches.txt");
        fs::write(&file, "alpha one\nalphabet two\n").expect("write fixture");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");

        assert_eq!(engine.handle_key("h", 2), "rerender");
        assert!(matches!(engine.app.popup(), Some(Popup::Help)));
        let help = joined_runs(&mut engine);
        assert!(help.contains("Help"));
        assert!(help.contains("jump to results"));
        assert!(help.contains("quit"));
        assert_eq!(engine.handle_key("esc", 0), "rerender");
        assert!(engine.app.popup().is_none());

        assert_eq!(engine.handle_key("m", 4), "rerender");
        assert_eq!(engine.app.toast_message(), Some("Multiline: ON"));
        assert!(joined_runs(&mut engine).contains("Multiline: ON"));
        wait_until_toast_dismissed(&mut engine);

        for character in "alpha".chars() {
            assert_eq!(engine.handle_key(&character.to_string(), 0), "rerender");
        }
        wait_until_complete(&mut engine);
        assert_eq!(engine.handle_key("tab", 0), "rerender");
        for character in "OMEGA".chars() {
            assert_eq!(engine.handle_key(&character.to_string(), 0), "rerender");
        }
        wait_until_preview_updated(&mut engine);

        assert_eq!(engine.handle_key("enter", 0), "rerender");
        assert!(matches!(
            engine.app.ui_state.current_screen,
            Screen::SearchFields(ref state)
                if state.focussed_section == FocussedSection::SearchResults
        ));
        assert_eq!(engine.handle_key("enter", 0), "rerender");
        wait_until_replacement_complete(&mut engine);

        let Screen::Results(state) = &engine.app.ui_state.current_screen else {
            panic!("replacement did not reach the results screen");
        };
        assert_eq!(state.num_successes, 2);
        assert_eq!(state.num_ignored, 0);
        assert!(state.errors.is_empty());
        let results = joined_runs(&mut engine);
        assert!(results.contains("Successful replacements (lines):"));
        assert!(results.contains("Success!"));
        assert!(results.contains('2'));
        assert_eq!(
            fs::read_to_string(&file).expect("read replacement"),
            "OMEGA one\nOMEGAbet two\n"
        );

        assert_eq!(engine.handle_key("esc", 0), "rerender");
        assert!(engine.app.popup().is_none());
        assert_eq!(engine.handle_key("enter", 0), "quit");
    }

    #[test]
    fn results_errors_and_non_search_escape_are_rendered_without_hiding() {
        let fixture = tempdir().expect("fixture directory");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        engine.app.ui_state.current_screen = Screen::Results(ReplaceState {
            num_successes: 1,
            num_ignored: 2,
            errors: vec![error_result("failed.txt", 4, "permission denied")],
            replacement_errors_pos: 0,
        });

        let results = joined_runs(&mut engine);
        assert!(results.contains("Ignored (lines):"));
        assert!(results.contains("Errors:"));
        assert!(results.contains("failed.txt:4"));
        assert!(results.contains("permission denied"));
        assert_eq!(engine.handle_key("esc", 0), "rerender");
        assert!(engine.app.popup().is_none());

        let (_sender, receiver) = mpsc::unbounded_channel();
        engine.app.ui_state.current_screen =
            Screen::PerformingReplacement(PerformingReplacementState::new(
                receiver,
                Arc::new(AtomicBool::new(false)),
                Arc::new(AtomicUsize::new(0)),
                1,
            ));
        assert!(joined_runs(&mut engine).contains("Performing replacement..."));
        assert_eq!(engine.handle_key("esc", 0), "rerender");
        assert!(engine.app.popup().is_none());
    }

    #[test]
    fn handle_key_surfaces_launch_editor_actions_from_its_post_key_drain() {
        let fixture = tempdir().expect("fixture directory");
        let expected_path = fixture.path().join("selected.txt");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        engine
            .app
            .event_channels
            .sender
            .send(Event::LaunchEditor((expected_path.clone(), 7)))
            .expect("engine event receiver lives");

        let response = engine.handle_key("left", 0);
        assert_eq!(response.status, "rerender");
        assert_eq!(
            response.actions,
            vec![EngineAction::OpenFile {
                path: expected_path,
                line: 7,
            }]
        );
    }

    #[test]
    fn alt_first_configured_open_binding_launches_in_background_only_from_results() {
        let fixture = tempdir().expect("fixture directory");
        let expected_path = fixture.path().join("selected.txt");
        fs::write(&expected_path, "alpha result\n").expect("write fixture");
        let options = EngineOptions::from_entries([OptionEntry::keys(
            "keys.search.results.open_in_editor",
            &["o", "e"],
        )])
        .expect("options parse");
        let mut engine =
            ScooterEngine::new_with_options(fixture.path(), options).expect("engine initialises");

        for character in "alpha".chars() {
            assert_eq!(engine.handle_key(&character.to_string(), 0), "rerender");
        }
        wait_until_complete(&mut engine);

        // Alt plus the configured base key has no special meaning while a
        // text field is focussed: it neither launches nor enters a character.
        let fields_response = engine.handle_key("o", 4);
        assert!(fields_response.actions.is_empty());
        assert_eq!(engine.app.search_fields.search().text(), "alpha");

        assert_eq!(engine.handle_key("enter", 0), "rerender");
        assert!(matches!(
            &engine.app.ui_state.current_screen,
            Screen::SearchFields(state)
                if state.focussed_section == FocussedSection::SearchResults
        ));

        // The feature follows the first remapped binding, not a hardcoded e.
        let wrong_key_response = engine.handle_key("e", 4);
        assert!(wrong_key_response.actions.is_empty());
        let response = engine.handle_key("o", 4);
        assert_eq!(
            response.actions,
            vec![EngineAction::OpenFileBackground {
                path: expected_path,
                line: 1,
            }]
        );
    }

    #[test]
    fn replacement_completion_queues_reload_documents_when_pumped_later() {
        let fixture = tempdir().expect("fixture directory");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        let (sender, receiver) = mpsc::unbounded_channel();
        engine.app.ui_state.current_screen =
            Screen::PerformingReplacement(PerformingReplacementState::new(
                receiver,
                Arc::new(AtomicBool::new(false)),
                Arc::new(AtomicUsize::new(0)),
                1,
            ));
        sender
            .send(BackgroundProcessingEvent::ReplacementCompleted(ReplaceState {
                num_successes: 1,
                num_ignored: 0,
                errors: vec![],
                replacement_errors_pos: 0,
            }))
            .expect("replacement receiver lives");

        // A hidden session does not pump. Once resumed, the queued completion
        // both reaches core's results screen and emits its deferred reload.
        let response = engine.pump();
        assert!(matches!(engine.app.ui_state.current_screen, Screen::Results(_)));
        assert_eq!(response.actions, vec![EngineAction::ReloadDocuments]);
    }

    #[test]
    fn actions_queued_while_hidden_are_delivered_by_the_first_resume_pump() {
        let fixture = tempdir().expect("fixture directory");
        let expected_path = fixture.path().join("arrived-while-hidden.txt");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        engine
            .app
            .event_channels
            .sender
            .send(Event::LaunchEditor((expected_path.clone(), 11)))
            .expect("engine event receiver lives");

        // No pump occurs while the component is hidden.
        let response = engine.pump();
        assert_eq!(response.status, "rerender");
        assert_eq!(
            response.actions,
            vec![EngineAction::OpenFile {
                path: expected_path,
                line: 11,
            }]
        );
    }

    #[test]
    fn action_queue_preserves_events_after_the_drain_limit() {
        let fixture = tempdir().expect("fixture directory");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        let path = fixture.path().join("selected.txt");
        for line in 1..=DRAIN_LIMIT + 1 {
            engine
                .app
                .event_channels
                .sender
                .send(Event::LaunchEditor((path.clone(), line)))
                .expect("engine event receiver lives");
        }

        let first = engine.pump();
        assert_eq!(first.actions.len(), DRAIN_LIMIT);
        assert_eq!(
            first.actions.first(),
            Some(&EngineAction::OpenFile {
                path: path.clone(),
                line: 1,
            })
        );
        let second = engine.pump();
        assert_eq!(
            second.actions,
            vec![EngineAction::OpenFile {
                path,
                line: DRAIN_LIMIT + 1,
            }]
        );
    }

    #[test]
    fn quit_and_drop_do_not_wait_for_a_large_in_flight_search() {
        let fixture = lifecycle_fixture(5_000, None);
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        start_search_and_wait_until_running(&mut engine, "needle");

        let started = Instant::now();
        engine.quit();
        drop(engine);
        assert!(
            started.elapsed() < Duration::from_millis(100),
            "quit/drop took {:?}",
            started.elapsed()
        );
    }

    #[test]
    fn hidden_search_completes_and_keeps_results_for_the_resume_pump() {
        let fixture = lifecycle_fixture(3_000, Some("needle"));
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        start_search_and_wait_until_running(&mut engine, "needle");

        // Simulate a hidden component: background work continues, but no
        // receiver is drained until the session is resumed.
        thread::sleep(Duration::from_millis(500));
        let response = engine.pump();
        assert_eq!(response.status, "rerender");
        assert!(search_complete(&engine));
        assert_eq!(search_state(&engine).results.len(), 1);
    }

    #[test]
    fn hidden_toast_dismisses_on_the_resume_pump() {
        let fixture = tempdir().expect("fixture directory");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        assert_eq!(engine.handle_key("m", 4), "rerender");
        assert!(engine.app.toast_message().is_some());

        thread::sleep(Duration::from_millis(1_700));
        let response = engine.pump();
        assert_eq!(response.status, "rerender");
        assert!(engine.app.toast_message().is_none());
    }

    #[test]
    fn reset_mid_search_discards_stale_background_events() {
        let fixture = lifecycle_fixture(3_000, Some("stale"));
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        start_search_and_wait_until_running(&mut engine, "stale");

        engine.reset();
        for _ in 0..30 {
            let response = engine.pump();
            assert!(response.actions.is_empty());
            thread::sleep(Duration::from_millis(10));
        }

        let Screen::SearchFields(state) = &engine.app.ui_state.current_screen else {
            panic!("reset left the fields screen");
        };
        assert!(engine.app.search_fields.search().text().is_empty());
        assert!(state.search_state.is_none());
    }

    #[test]
    fn busy_is_true_while_a_debounce_timer_is_pending() {
        let fixture = tempdir().expect("fixture directory");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        assert_eq!(engine.handle_key("a", 0), "rerender");
        assert!(engine.busy());
    }

    #[test]
    fn busy_is_true_for_pending_and_running_search_phases() {
        let fixture = tempdir().expect("fixture directory");
        fs::write(fixture.path().join("match.txt"), "alpha\nalphabet\n").expect("write fixture");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        start_search_and_wait_until_running(&mut engine, "alpha");
        assert!(engine.busy());

        assert_eq!(engine.handle_key("b", 0), "rerender");
        assert!(matches!(search_state(&engine).phase, SearchPhase::Pending));
        assert!(engine.busy());
    }

    #[test]
    fn busy_is_true_only_while_preview_updates_are_in_flight() {
        let fixture = tempdir().expect("fixture directory");
        fs::write(fixture.path().join("match.txt"), "alpha\nalphabet\n").expect("write fixture");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        for character in "alpha".chars() {
            engine.handle_key(&character.to_string(), 0);
        }
        wait_until_complete(&mut engine);
        assert_eq!(engine.handle_key("tab", 0), "rerender");
        assert_eq!(engine.handle_key("O", 0), "rerender");
        assert!(engine.busy());

        wait_until_preview_updated(&mut engine);
        assert!(!engine.busy());
    }

    #[test]
    fn busy_is_true_while_a_replacement_is_performing() {
        let fixture = tempdir().expect("fixture directory");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        let (_sender, receiver) = mpsc::unbounded_channel();
        engine.app.ui_state.current_screen =
            Screen::PerformingReplacement(PerformingReplacementState::new(
                receiver,
                Arc::new(AtomicBool::new(false)),
                Arc::new(AtomicUsize::new(0)),
                1,
            ));
        assert!(engine.busy());
    }

    #[test]
    fn busy_is_true_while_a_toast_is_visible() {
        let fixture = tempdir().expect("fixture directory");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        assert_eq!(engine.handle_key("m", 4), "rerender");
        assert!(engine.app.toast_message().is_some());
        assert!(engine.busy());
    }

    #[test]
    fn busy_is_false_for_a_completed_search_without_a_toast() {
        let fixture = tempdir().expect("fixture directory");
        fs::write(fixture.path().join("match.txt"), "alpha\n").expect("write fixture");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        start_search_and_wait_until_running(&mut engine, "alpha");
        wait_until_complete(&mut engine);
        assert!(!engine.busy());
    }

    #[test]
    fn busy_is_false_for_an_invalid_search_without_a_toast() {
        let fixture = tempdir().expect("fixture directory");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        assert_eq!(engine.handle_key("(", 0), "rerender");
        wait_until_invalid_search(&mut engine);
        assert!(!engine.busy());
    }

    #[test]
    fn busy_is_false_for_replacement_results_without_a_toast() {
        let fixture = tempdir().expect("fixture directory");
        let mut engine = ScooterEngine::new(fixture.path()).expect("engine initialises");
        engine.app.ui_state.current_screen = Screen::Results(ReplaceState {
            num_successes: 1,
            num_ignored: 0,
            errors: vec![],
            replacement_errors_pos: 0,
        });
        assert!(!engine.busy());
    }

    fn lifecycle_fixture(file_count: usize, matching_text: Option<&str>) -> tempfile::TempDir {
        let fixture = tempdir().expect("fixture directory");
        for index in 0..file_count {
            let contents = if index == 0 {
                matching_text.map_or_else(
                    || format!("ordinary lifecycle fixture {index}\n"),
                    |text| format!("{text} lifecycle fixture {index}\n"),
                )
            } else {
                format!("ordinary lifecycle fixture {index}\n")
            };
            fs::write(fixture.path().join(format!("file-{index}.txt")), contents)
                .expect("write lifecycle fixture file");
        }
        fixture
    }

    fn start_search_and_wait_until_running(engine: &mut ScooterEngine, query: &str) {
        for character in query.chars() {
            assert_eq!(engine.handle_key(&character.to_string(), 0), "rerender");
        }

        let deadline = Instant::now() + Duration::from_secs(10);
        while Instant::now() < deadline {
            let _ = engine.pump();
            if matches!(
                &engine.app.ui_state.current_screen,
                Screen::SearchFields(state)
                    if matches!(
                        state.search_state.as_ref().map(|search| search.phase),
                        Some(SearchPhase::Running { .. })
                    )
            ) {
                return;
            }
            thread::sleep(Duration::from_millis(10));
        }
        panic!("search did not begin running");
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

    fn wait_until_toast_dismissed(engine: &mut ScooterEngine) {
        let deadline = Instant::now() + Duration::from_secs(3);
        while Instant::now() < deadline {
            let _ = engine.pump();
            if engine.app.toast_message().is_none() {
                return;
            }
            thread::sleep(Duration::from_millis(10));
        }
        panic!("toast did not dismiss");
    }

    fn wait_until_replacement_complete(engine: &mut ScooterEngine) {
        let deadline = Instant::now() + Duration::from_secs(10);
        while Instant::now() < deadline {
            let _ = engine.pump();
            if matches!(engine.app.ui_state.current_screen, Screen::Results(_)) {
                return;
            }
            thread::sleep(Duration::from_millis(10));
        }
        panic!("replacement did not complete");
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

    fn error_result(path: &str, line: usize, error: &str) -> SearchResultWithReplacement {
        SearchResultWithReplacement {
            search_result: SearchResult::new_line(
                Some(PathBuf::from(path)),
                line,
                "original".to_string(),
                LineEnding::Lf,
                true,
            ),
            replacement: "replacement".to_string(),
            replace_result: Some(ReplaceResult::Error(error.to_string())),
            preview_error: None,
        }
    }
}
