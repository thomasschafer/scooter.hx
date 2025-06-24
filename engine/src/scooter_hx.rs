use ignore::WalkState;
use steel::rvals::Custom;
use steel::steel_vm::ffi::FFIValue;
use steel_derive::Steel;

use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;

use frep_core::{
    search::SearchResult,
    validation::{self, SearchConfiguration, ValidationResult},
};

use crate::validation::{
    ErrorHandler, error_response, success_response, validation_error_response,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum SearchResults {
    NotStarted,
    InProgress(Vec<SearchResult>),
    Complete(Vec<SearchResult>),
}

pub(crate) struct ScooterHx {
    pub(crate) search_results: Arc<Mutex<SearchResults>>,
    pub(crate) search_cancelled: Option<Arc<AtomicBool>>,
}

impl Custom for ScooterHx {}

#[derive(Clone, Debug, Eq, Steel, PartialEq)]
pub(crate) struct WrappedSearchResult(SearchResult);

impl ScooterHx {
    pub(crate) fn new() -> Self {
        ScooterHx {
            search_results: Arc::new(Mutex::new(SearchResults::NotStarted)),
            search_cancelled: None,
        }
    }

    pub(crate) fn cancel_search(&mut self) {
        if let Some(token) = &self.search_cancelled {
            token.store(true, Ordering::Relaxed);
        }
        *self.search_results.lock().unwrap() = SearchResults::NotStarted;
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
        if let Some(token) = &self.search_cancelled {
            token.store(true, Ordering::Relaxed);
        }

        let cancellation_token = Arc::new(AtomicBool::new(false));
        self.search_cancelled = Some(cancellation_token.clone());

        *self.search_results.lock().unwrap() = SearchResults::NotStarted;

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

        let search_results = self.search_results.clone();

        thread::spawn(move || {
            let (tx, rx) = crossbeam::channel::bounded(100);

            *search_results.lock().unwrap() = SearchResults::InProgress(Vec::new());

            searcher.walk_files(Some(&cancellation_token), || {
                let tx = tx.clone();
                Box::new(move |results| {
                    // Ignore error - likely state reset, thread about to be killed
                    let _ = tx.send(results);
                    WalkState::Continue
                })
            });

            while let Ok(results) = rx.recv() {
                let mut search_results = search_results.lock().unwrap();
                match &mut *search_results {
                    SearchResults::InProgress(current_results) => {
                        current_results.extend(results);
                    }
                    _ => break, // Search was cancelled
                }
            }

            let mut search_results = search_results.lock().unwrap();
            if let SearchResults::InProgress(results) =
                std::mem::replace(&mut *search_results, SearchResults::NotStarted)
            {
                *search_results = SearchResults::Complete(results);
            }
        });

        success_response()
    }

    pub(crate) fn search_is_progressing(&self) -> bool {
        match &*self.search_results.lock().unwrap() {
            SearchResults::InProgress(_) => true,
            SearchResults::NotStarted | SearchResults::Complete(_) => false,
        }
    }

    pub(crate) fn search_results_window(
        &self,
        start: usize,
        end: usize,
    ) -> Vec<WrappedSearchResult> {
        let search_results = self.search_results.lock().unwrap();
        let results = match &*search_results {
            SearchResults::InProgress(results) | SearchResults::Complete(results) => results,
            SearchResults::NotStarted => {
                panic!("Attempted to get window on NotStarted search_results")
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
        let mut search_results_guard = self.search_results.lock().unwrap();
        let search_results = match &mut *search_results_guard {
            SearchResults::NotStarted => {
                panic!("Attempted to toggle inclusion on None search_results")
            }
            SearchResults::InProgress(search_results) | SearchResults::Complete(search_results) => {
                search_results
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

    pub(crate) fn replace(&mut self) {
        todo!() // TODO: kick off replacement
    }
}
