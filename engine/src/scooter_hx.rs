use ignore::WalkState;
use steel::rvals::Custom;
use steel_derive::Steel;

use std::path::PathBuf;

use frep_core::{
    search::SearchResult,
    validation::{self, SearchConfiguration, SimpleErrorHandler, ValidationResult},
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum SearchResults {
    NotStarted,
    InProgress(Vec<SearchResult>),
    Complete(Vec<SearchResult>),
}

pub(crate) struct ScooterHx {
    pub(crate) search_results: SearchResults,
}

impl Custom for ScooterHx {}

#[derive(Clone, Debug, Eq, Steel, PartialEq)]
pub(crate) struct WrappedSearchResult(SearchResult);

impl ScooterHx {
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn search(
        &mut self,
        search_text: String,
        replacement_text: String,
        fixed_strings: bool,
        match_whole_word: bool,
        match_case: bool,
        include_globs: String,
        exclude_globs: String,
        directory: String,
    ) {
        self.search_results = SearchResults::NotStarted;

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
        let mut error_handler = SimpleErrorHandler::new();
        let result = validation::validate_search_configuration(search_config, &mut error_handler);
        let searcher = match result {
            Err(e) => todo!(), // TODO: send back e
            Ok(ValidationResult::Success(searcher)) => searcher,
            Ok(ValidationResult::ValidationErrors) => {
                todo!()
                // TODO: send back error with the following string: error_handler.errors_str().unwrap()
            }
        };

        let (tx, rx) = crossbeam::channel::bounded(100);

        searcher.walk_files(None, || {
            let tx = tx.clone();
            Box::new(move |results| {
                // Ignore error - likely state reset, thread about to be killed
                let _ = tx.send(results);
                WalkState::Continue
            })
        });

        self.search_results = SearchResults::InProgress(Vec::new());

        while let Ok(results) = rx.recv() {
            match &mut self.search_results {
                SearchResults::InProgress(current_results) => {
                    current_results.extend(results);
                }
                _ => panic!("Expected SearchResults::InProgress"),
            }
        }

        match std::mem::replace(&mut self.search_results, SearchResults::NotStarted) {
            SearchResults::InProgress(results) => {
                self.search_results = SearchResults::Complete(results);
            }
            _ => panic!("Expected SearchResults::InProgress"),
        }
    }

    pub(crate) fn search_results_window(
        &self,
        start: usize,
        end: usize,
    ) -> Vec<WrappedSearchResult> {
        let results = match &self.search_results {
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
        let search_results = match &mut self.search_results {
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
