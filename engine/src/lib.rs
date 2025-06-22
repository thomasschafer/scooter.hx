use frep_core::search::SearchResult;
use steel::{
    declare_module,
    rvals::Custom,
    steel_vm::ffi::{FFIModule, RegisterFFIFn},
};
use steel_derive::Steel;

declare_module!(create_module);

impl Custom for ScooterHx {}

struct ScooterHx {
    search_results: Vec<SearchResult>,
}

#[derive(Clone, Debug, Steel, PartialEq)]
struct WrappedSearchResult(SearchResult);

impl ScooterHx {
    fn search() {
        todo!()
    }

    fn search_results_window(start: usize, end: usize) -> Vec<WrappedSearchResult> {
        todo!()
    }

    fn toggle_inclusion(&mut self, idx: usize) {
        match self.search_results.get_mut(idx) {
            Some(res) => {
                res.included = !res.included;
            }
            None => panic!(
                "No result at idx {idx}. Results have length {len}",
                len = self.search_results.len()
            ),
        }
    }

    fn replace() {
        todo!()
    }
}

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("steel/sys-info");

    module
        .register_fn("Scooter-search", ScooterHx::search)
        .register_fn(
            "Scooter-search-results-window",
            ScooterHx::search_results_window,
        )
        .register_fn("Scooter-toggle_inclusion", ScooterHx::toggle_inclusion)
        .register_fn("Scooter-replace", ScooterHx::replace);

    module
}
