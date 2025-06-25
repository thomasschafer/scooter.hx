use ignore::WalkState;
use steel::rvals::Custom;
use steel::steel_vm::ffi::FFIValue;
use steel_derive::Steel;

use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;

use frep_core::{
    search::SearchResult,
    validation::{self, SearchConfiguration, ValidationResult},
};

use crate::validation::{
    ErrorHandler, error_response, success_response, validation_error_response,
};

#[derive(Clone, Debug, Eq, Steel, PartialEq)]
pub(crate) struct ReplacementStats {
    pub(crate) num_successes: usize,
    pub(crate) num_ignored: usize,
    pub(crate) errors: Vec<SearchResult>,
}

pub(crate) enum State {
    NotStarted,
    SearchInProgress {
        results: Vec<SearchResult>,
        cancelled: Arc<AtomicBool>,
    },
    SearchComplete(Vec<SearchResult>),
    PerformingReplacement {
        cancelled: Arc<AtomicBool>,
        num_replacements_completed: Arc<AtomicUsize>,
        num_ignored: usize,
    },
    ReplacementComplete(ReplacementStats),
}

impl State {
    fn name(&self) -> &'static str {
        match self {
            State::NotStarted => "NotStarted",
            State::SearchInProgress { .. } => "SearchInProgress",
            State::SearchComplete(_) => "SearchComplete",
            State::PerformingReplacement { .. } => "PerformingReplacement",
            State::ReplacementComplete(ReplacementStats { .. }) => "ReplacementComplete",
        }
    }
}

pub(crate) struct ScooterHx {
    pub(crate) state: Arc<Mutex<State>>,
}

impl Custom for ScooterHx {}

#[derive(Clone, Debug, Eq, Steel, PartialEq)]
pub(crate) struct WrappedSearchResult(SearchResult);

impl ScooterHx {
    pub(crate) fn new() -> Self {
        ScooterHx {
            state: Arc::new(Mutex::new(State::NotStarted)),
        }
    }

    pub(crate) fn cancel_search(&mut self) {
        let mut state = self.state.lock().unwrap();
        let State::SearchInProgress { cancelled, .. } = &mut *state else {
            return;
        };
        cancelled.store(true, Ordering::Relaxed);
        *self.state.lock().unwrap() = State::NotStarted;
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn start_search(
        &mut self,
        search_text: String,
        replacement_text: String,
        fixed_strings: bool,
        match_whole_word: bool,
        match_case: bool,
        include_globs: String,
        exclude_globs: String,
        directory: String,
    ) -> FFIValue {
        self.cancel_search();

        *self.state.lock().unwrap() = State::NotStarted;

        let search_config = SearchConfiguration {
            search_text: &search_text,
            replacement_text: &replacement_text,
            fixed_strings,
            advanced_regex: false,
            include_globs: Some(&include_globs),
            exclude_globs: Some(&exclude_globs),
            match_whole_word,
            match_case,
            include_hidden: false,
            directory: PathBuf::from(directory),
        };
        let mut error_handler = ErrorHandler::new();
        let result = validation::validate_search_configuration(search_config, &mut error_handler);
        let searcher = match result {
            Err(e) => {
                return error_response(
                    "configuration-error",
                    &format!("Failed to validate search configuration: {}", e),
                );
            }
            Ok(ValidationResult::Success(searcher)) => searcher,
            Ok(ValidationResult::ValidationErrors) => {
                return validation_error_response(&error_handler);
            }
        };

        let cancellation_token = Arc::new(AtomicBool::new(false));
        let state = self.state.clone();

        thread::spawn(move || {
            let (tx, rx) = crossbeam::channel::bounded(1000);

            *state.lock().unwrap() = State::SearchInProgress {
                results: Vec::new(),
                cancelled: cancellation_token.clone(),
            };

            searcher.walk_files(Some(&cancellation_token), || {
                let tx = tx.clone();
                Box::new(move |results| {
                    // Ignore error - likely state reset, thread about to be killed
                    let _ = tx.send(results);
                    WalkState::Continue
                })
            });

            while let Ok(additional_results) = rx.recv() {
                let mut state = state.lock().unwrap();
                match &mut *state {
                    State::SearchInProgress { results, .. } => {
                        results.extend(additional_results);
                    }
                    _ => break, // Search was cancelled
                }
            }

            let mut state = state.lock().unwrap();
            if let State::SearchInProgress { results, .. } =
                std::mem::replace(&mut *state, State::NotStarted)
            {
                *state = State::SearchComplete(results);
            }
        });

        success_response()
    }

    pub(crate) fn search_complete(&self) -> bool {
        matches!(&*self.state.lock().unwrap(), State::SearchComplete { .. })
    }

    pub(crate) fn search_results_window(
        &self,
        start: usize,
        end: usize,
    ) -> Vec<WrappedSearchResult> {
        let state = self.state.lock().unwrap();
        let results = match &*state {
            State::SearchInProgress { results, .. } | State::SearchComplete(results) => results,
            res => {
                panic!(
                    "Attempted to get search results window on {name}",
                    name = res.name()
                )
            }
        };

        results
            .get(start..end)
            .unwrap_or(&[])
            .iter()
            .map(|result| WrappedSearchResult(result.clone()))
            .collect()
    }

    pub(crate) fn toggle_inclusion(&mut self, idx: usize) {
        let mut state = self.state.lock().unwrap();
        let search_results = match &mut *state {
            State::SearchInProgress { results, .. } | State::SearchComplete(results) => results,
            res => {
                panic!("Attempted to toggle inclusion on {name}", name = res.name())
            }
        };

        match search_results.get_mut(idx) {
            Some(res) => {
                res.included = !res.included;
            }
            None => panic!(
                "No result at idx {idx}. Results have length {len}",
                len = search_results.len()
            ),
        }
    }

    pub(crate) fn start_replace(&mut self) {
        let cancelled = Arc::new(AtomicBool::new(false));
        let num_replacements_completed = Arc::new(AtomicUsize::new(0));
        let mut state = self.state.lock().unwrap();

        let (mut receiver, num_ignored) = match std::mem::replace(
            &mut *state,
            State::NotStarted, // temporary placeholder
        ) {
            State::SearchComplete(search_results) => {
                let cancelled_clone = cancelled.clone();
                let num_replacements_completed_clone = num_replacements_completed.clone();
                scooter_core::replace::spawn_replace_included(
                    search_results,
                    cancelled_clone,
                    num_replacements_completed_clone,
                )
            }
            res => {
                panic!(
                    "Called replace on {name}, expected SearchComplete",
                    name = res.name()
                )
            }
        };
        drop(state);

        let state_clone = self.state.clone();
        thread::spawn(async move || {
            let mut replacement_results = vec![];
            loop {
                tokio::select! {
                    res = receiver.recv() => match res {
                        Some(res) => replacement_results.push(res),
                        None => break,
                    }
                }
            }

            let stats = frep_core::replace::calculate_statistics(replacement_results);

            let mut state = state_clone.lock().unwrap();
            if let State::PerformingReplacement { .. } =
                std::mem::replace(&mut *state, State::NotStarted)
            {
                *state = State::ReplacementComplete(ReplacementStats {
                    num_successes: stats.num_successes,
                    num_ignored,
                    errors: stats.errors,
                });
            }
        });

        let mut state = self.state.lock().unwrap();
        *state = State::PerformingReplacement {
            cancelled,
            num_replacements_completed,
            num_ignored,
        };
    }

    pub(crate) fn replacement_complete(&self) -> bool {
        let state = self.state.lock().unwrap();
        matches!(*state, State::ReplacementComplete { .. })
    }

    pub(crate) fn replacement_stats(&self) -> ReplacementStats {
        let mut state = self.state.lock().unwrap();
        match &mut *state {
            State::ReplacementComplete(stats) => stats.clone(),
            res => panic!(
                "Called replace on {name}, expected ReplacementComplete",
                name = res.name()
            ),
        }
    }

    pub(crate) fn cancel_replacement(&mut self) {
        let mut state = self.state.lock().unwrap();
        let State::PerformingReplacement { cancelled, .. } = &mut *state else {
            return;
        };
        cancelled.store(true, Ordering::Relaxed);
        *self.state.lock().unwrap() = State::NotStarted;
    }
}

// TODO:
// - unit tests
// - end-to-end tests
