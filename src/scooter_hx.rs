use frep_core::replace::ReplaceResult;
use ignore::WalkState;
use steel::rvals::Custom;
use steel::steel_vm::ffi::FFIValue;
use steel_derive::Steel;

use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, mpsc};
use std::thread;

use frep_core::{
    search::SearchResult,
    validation::{self, SearchConfiguration, ValidationResult},
};
use scooter_core::{
    diff::{Diff, line_diff},
    utils::{read_lines_range, relative_path_from, split_indexed_lines, strip_control_chars},
};

use crate::logging;
use crate::validation::{
    ErrorHandler, error_response, success_response, validation_error_response,
};

#[derive(Clone, Debug, Eq, Steel, PartialEq)]
pub(crate) struct ReplacementStats {
    pub(crate) num_successes: usize,
    pub(crate) num_ignored: usize,
    pub(crate) errors: Vec<SteelSearchResult>,
}

impl ReplacementStats {
    pub(crate) fn num_successes(&self) -> usize {
        self.num_successes
    }

    pub(crate) fn num_ignored(&self) -> usize {
        self.num_ignored
    }

    pub(crate) fn num_errors(&self) -> usize {
        self.errors.len()
    }
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
    pub(crate) directory: PathBuf,
}

impl Custom for ScooterHx {}

#[derive(Clone, Debug, Eq, Steel, PartialEq)]
pub(crate) struct SteelSearchResult {
    pub(crate) display_path: String,
    pub(crate) full_path: String,
    pub(crate) line_num: usize,
    pub(crate) line: String,
    pub(crate) replacement: String,
    pub(crate) replace_result: Option<ReplaceResult>,
    pub(crate) included: bool,
}

impl SteelSearchResult {
    pub(crate) fn display_path(&self) -> String {
        self.display_path.clone()
    }

    pub(crate) fn full_path(&self) -> String {
        self.full_path.clone()
    }

    pub(crate) fn line_num(&self) -> usize {
        self.line_num
    }

    pub(crate) fn build_preview(&self, screen_height: usize) -> Vec<LineWithStyle> {
        match self.try_build_preview(screen_height) {
            Ok(preview) => preview,
            Err(error) => {
                // Return error message as red text
                vec![vec![vec![
                    format!("Failed to render diff: {}", error),
                    "red".to_string(),
                    "".to_string(),
                ]]]
            }
        }
    }

    pub(crate) fn included(&self) -> bool {
        self.included
    }

    pub(crate) fn display_error(&self) -> Vec<String> {
        let error = match &self.replace_result {
            Some(ReplaceResult::Error(error)) => error,
            None => panic!("Found error result with no error message"),
            Some(ReplaceResult::Success) => {
                panic!("Found successful result in errors: {self:?}")
            }
        };

        let path_display = format!("{}:{}", self.display_path, self.line_num);

        vec![path_display, error.to_string()]
    }

    fn try_build_preview(&self, screen_height: usize) -> Result<Vec<LineWithStyle>, String> {
        let line_idx = self.line_num.saturating_sub(1); // Convert to 0-based index
        let start = line_idx.saturating_sub(screen_height);
        let end = line_idx + screen_height;

        let file_path = Path::new(&self.full_path);
        let lines = read_lines_range(file_path, start, end)
            .map_err(|e| format!("file read error: {e}"))?
            .collect::<Vec<_>>();

        let (before, cur, after) = split_indexed_lines(lines, line_idx, screen_height - 1)
            .map_err(|e| format!("line split error: {e}"))?;
        if cur.1 != self.line {
            return Err("File content has changed".into());
        }

        let (before_segments, after_segments) = line_diff(&self.line, &self.replacement);

        let preview_lines = before
            .iter()
            .map(|(_, l)| str_to_vec(l))
            .chain(vec![
                diffs_to_vec(&before_segments),
                diffs_to_vec(&after_segments),
            ])
            .chain(after.iter().map(|(_, l)| str_to_vec(l)))
            .collect();

        Ok(preview_lines)
    }

    fn from_search_result(s: &SearchResult, directory: &Path) -> Self {
        Self {
            display_path: relative_path_from(directory, &s.path),
            full_path: s.path.to_string_lossy().to_string(),
            line_num: s.line_number,
            line: s.line.clone(),
            replacement: s.replacement.clone(),
            replace_result: s.replace_result.clone(),
            included: s.included,
        }
    }
}

/// Vector of triples [text, fg colour, bg colour]. We use this representation so that we
/// can easily pass to Steel.
type LineWithStyle = Vec<Vec<String>>;

fn str_to_vec(line: &str) -> LineWithStyle {
    vec![vec![
        format!("  {}", strip_control_chars(line)), // Add 2 spaces to align with diff prefixes
        "".to_string(),
        "".to_string(),
    ]]
}

fn diffs_to_vec(diffs: &[Diff]) -> LineWithStyle {
    diffs
        .iter()
        .map(|d| {
            vec![
                strip_control_chars(&d.text),
                d.fg_colour.to_str().to_owned(),
                d.bg_colour
                    .clone()
                    .map(|c| c.to_str().to_owned())
                    .unwrap_or_default(),
            ]
        })
        .collect()
}

impl ScooterHx {
    pub(crate) fn new(directory: String, logging_enabled: bool) -> Self {
        let log_level = if logging_enabled {
            log::LevelFilter::Error
        } else {
            log::LevelFilter::Off
        };
        logging::setup_logging(log_level).expect("Failed to initialize logging");
        ScooterHx {
            state: Arc::new(Mutex::new(State::NotStarted)),
            directory: directory.into(),
        }
    }

    pub(crate) fn reset(&mut self) {
        self.state = Arc::new(Mutex::new(State::NotStarted));
    }

    pub(crate) fn cancel_search(&mut self) {
        let mut state = self.state.lock().unwrap();
        if let State::SearchInProgress { cancelled, .. } = &*state {
            cancelled.store(true, Ordering::Relaxed);
            *state = State::NotStarted;
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn start_search(
        &mut self,
        search_text: &str,
        replacement_text: &str,
        fixed_strings: bool,
        match_whole_word: bool,
        match_case: bool,
        include_globs: &str,
        exclude_globs: &str,
    ) -> FFIValue {
        self.cancel_search();

        *self.state.lock().unwrap() = State::NotStarted;

        let search_config = SearchConfiguration {
            search_text,
            replacement_text,
            fixed_strings,
            advanced_regex: false,
            include_globs: Some(include_globs),
            exclude_globs: Some(exclude_globs),
            match_whole_word,
            match_case,
            include_hidden: false,
            directory: self.directory.clone(),
        };
        // TODO: handle errors in UI
        let mut error_handler = ErrorHandler::new();
        let result = validation::validate_search_configuration(search_config, &mut error_handler);
        let searcher = match result {
            Err(e) => {
                return error_response(
                    "configuration-error",
                    &format!("Failed to validate search configuration: {e}"),
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

            let state_clone = state.clone();
            let consumer_handle = thread::spawn(move || {
                while let Ok(additional_results) = rx.recv() {
                    let mut state = state_clone.lock().unwrap();
                    match &mut *state {
                        State::SearchInProgress { results, .. } => {
                            results.extend(additional_results);
                        }
                        _ => break, // Search was cancelled
                    }
                }
            });

            searcher.walk_files(Some(&cancellation_token), || {
                let tx = tx.clone();
                Box::new(move |results| {
                    // Ignore error - likely state reset, thread about to be killed
                    let _ = tx.send(results);
                    WalkState::Continue
                })
            });

            // Drop the original sender so the receiver loop can terminate
            drop(tx);

            consumer_handle.join().unwrap();

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
        matches!(&*self.state.lock().unwrap(), State::SearchComplete(_))
    }

    pub(crate) fn search_result_count(&self) -> usize {
        let state = self.state.lock().unwrap();
        match &*state {
            State::SearchInProgress { results, .. } | State::SearchComplete(results) => {
                results.len()
            }
            _ => 0,
        }
    }

    // Note that this is an inclusive window, i.e. `search_results_window(a, b)` maps to `[a..=b]`
    pub(crate) fn search_results_window(&self, start: usize, end: usize) -> Vec<SteelSearchResult> {
        let state = self.state.lock().unwrap();
        let (State::SearchInProgress { results, .. } | State::SearchComplete(results)) = &*state
        else {
            return vec![];
        };

        results
            .get(start..=end)
            .unwrap_or(&[])
            .iter()
            .map(|s| SteelSearchResult {
                display_path: relative_path_from(&self.directory, &s.path),
                full_path: s.path.to_string_lossy().to_string(),
                line_num: s.line_number,
                line: s.line.clone(),
                replacement: s.replacement.clone(),
                replace_result: s.replace_result.clone(),
                included: s.included,
            })
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

    pub(crate) fn toggle_all(&mut self) {
        let mut state = self.state.lock().unwrap();
        let (State::SearchInProgress { results, .. } | State::SearchComplete(results)) =
            &mut *state
        else {
            return;
        };
        let all_included = results.iter().all(|res| res.included);
        results
            .iter_mut()
            .for_each(|res| res.included = !all_included);
    }

    pub(crate) fn start_replace(&mut self) {
        let cancelled = Arc::new(AtomicBool::new(false));
        let num_replacements_completed = Arc::new(AtomicUsize::new(0));
        let mut state = self.state.lock().unwrap();

        let (tx, rx) = mpsc::channel();
        let num_ignored = match std::mem::replace(
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
                    move |result| {
                        let _ = tx.send(result); // Ignore error if receiver is dropped
                    },
                )
            }
            _ => return,
        };
        drop(state);

        let state_clone = self.state.clone();

        let directory = self.directory.clone();
        thread::spawn(move || {
            let mut replacement_results = vec![];
            while let Ok(res) = rx.recv() {
                replacement_results.push(res);
            }

            let stats = frep_core::replace::calculate_statistics(replacement_results);

            let mut state = state_clone.lock().unwrap();
            if let State::PerformingReplacement { .. } =
                std::mem::replace(&mut *state, State::NotStarted)
            {
                *state = State::ReplacementComplete(ReplacementStats {
                    num_successes: stats.num_successes,
                    num_ignored,
                    errors: stats
                        .errors
                        .iter()
                        .map(|sr| SteelSearchResult::from_search_result(sr, &directory))
                        .collect(),
                });
            }
        });

        let mut state = self.state.lock().unwrap();
        *state = State::PerformingReplacement {
            cancelled,
            num_replacements_completed,
        };
    }

    pub(crate) fn num_replacements_complete(&self) -> usize {
        let state = self.state.lock().unwrap();
        match &*state {
            State::PerformingReplacement {
                num_replacements_completed,
                ..
            } => num_replacements_completed.load(Ordering::Relaxed),
            State::ReplacementComplete(stats) => stats.num_successes,
            _ => 0,
        }
    }

    pub(crate) fn replacement_complete(&self) -> bool {
        let state = self.state.lock().unwrap();
        matches!(*state, State::ReplacementComplete(_))
    }

    pub(crate) fn replacement_stats(&self) -> ReplacementStats {
        let mut state = self.state.lock().unwrap();
        match &mut *state {
            State::ReplacementComplete(stats) => stats.clone(),
            res => panic!(
                "Called replacement_stats on {name}, expected ReplacementComplete",
                name = res.name()
            ),
        }
    }

    pub(crate) fn cancel_replacement(&mut self) {
        let mut state = self.state.lock().unwrap();
        if let State::PerformingReplacement { cancelled, .. } = &*state {
            cancelled.store(true, Ordering::Relaxed);
            *state = State::NotStarted;
        }
    }

    pub(crate) fn replacement_errors(&self) -> Vec<SteelSearchResult> {
        let state = self.state.lock().unwrap();
        match &*state {
            State::ReplacementComplete(stats) => stats.errors.clone(),
            _ => vec![],
        }
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use frep_core::line_reader::LineEnding;

    use super::*;
    use crate::test_utils::wait_until;

    #[allow(clippy::too_many_lines)]
    #[test]
    fn test_basic_search_and_replace() {
        let temp_dir = create_test_files!(
            "file1.txt" => text!(
                "This is a test file.",
                "It contains TEST_PATTERN that should be replaced.",
                "Multiple lines with TEST_PATTERN here.",
            ),
            "file2.txt" => text!(
                "Another file with TEST_PATTERN.",
                "Second line.",
            ),
            "subdir/file3.txt" => text!(
                "Nested file with TEST_PATTERN.",
                "Only one occurrence here.",
            ),
            "binary.bin" => &[10, 19, 3, 92],
        );
        let mut scooter = ScooterHx::new(temp_dir.path().to_string_lossy().into(), true);

        scooter.start_search("TEST_PATTERN", "REPLACEMENT", false, false, true, "", "");

        wait_until(|| scooter.search_complete(), Duration::from_millis(100));

        let mut search_results_clone = {
            let state = scooter.state.lock().unwrap();
            match &*state {
                State::SearchComplete(search_results) => search_results.clone(),
                other => panic!("Expected SearchComplete, found {}", other.name()),
            }
        };

        let expected = vec![
            SearchResult {
                path: temp_dir.path().to_path_buf().join("file1.txt"),
                line_number: 2,
                line: "It contains TEST_PATTERN that should be replaced.".to_owned(),
                line_ending: LineEnding::Lf,
                replacement: "It contains REPLACEMENT that should be replaced.".to_owned(),
                included: true,
                replace_result: None,
            },
            SearchResult {
                path: temp_dir.path().to_path_buf().join("file1.txt"),
                line_number: 3,
                line: "Multiple lines with TEST_PATTERN here.".to_owned(),
                line_ending: LineEnding::Lf,
                replacement: "Multiple lines with REPLACEMENT here.".to_owned(),
                included: true,
                replace_result: None,
            },
            SearchResult {
                path: temp_dir.path().to_path_buf().join("file2.txt"),
                line_number: 1,
                line: "Another file with TEST_PATTERN.".to_owned(),
                line_ending: LineEnding::Lf,
                replacement: "Another file with REPLACEMENT.".to_owned(),
                included: true,
                replace_result: None,
            },
            SearchResult {
                path: temp_dir
                    .path()
                    .to_path_buf()
                    .join("subdir")
                    .join("file3.txt"),
                line_number: 1,
                line: "Nested file with TEST_PATTERN.".to_owned(),
                line_ending: LineEnding::Lf,
                replacement: "Nested file with REPLACEMENT.".to_owned(),
                included: true,
                replace_result: None,
            },
        ];
        search_results_clone.sort_by_key(|s| (s.path.clone(), s.line_number));
        assert_eq!(search_results_clone, expected);

        // let mut window = scooter.search_results_window(0, 3).clone();
        // window.sort_by_key(|s| (s.0.path.clone(), s.0.line_number));
        // assert_eq!(
        //     window,
        //     expected
        //         .into_iter()
        //         .map(SteelSearchResult::from)
        //         .collect::<Vec<_>>()
        // );

        scooter.start_replace();

        wait_until(
            || scooter.replacement_complete(),
            Duration::from_millis(100),
        );

        let stats_clone = {
            let state = scooter.state.lock().unwrap();
            match &*state {
                State::ReplacementComplete(stats) => stats.clone(),
                other => panic!("Expected ReplacementComplete, found {}", other.name()),
            }
        };
        assert_eq!(stats_clone.num_successes, 4);
        assert_eq!(stats_clone.num_ignored, 0);
        assert_eq!(stats_clone.errors.len(), 0);

        assert_test_files!(
            &temp_dir,
            "file1.txt" => text!(
                "This is a test file.",
                "It contains REPLACEMENT that should be replaced.",
                "Multiple lines with REPLACEMENT here.",
            ),
            "file2.txt" => text!(
                "Another file with REPLACEMENT.",
                "Second line.",
            ),
            "subdir/file3.txt" => text!(
                "Nested file with REPLACEMENT.",
                "Only one occurrence here.",
            ),
            "binary.bin" => &[10, 19, 3, 92],
        );
    }
    // TODO: more detailed tests for individual functions e.g. `search_results_window`, `toggle_inclusion`
}
