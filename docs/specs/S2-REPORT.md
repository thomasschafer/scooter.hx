# S2 implementation report

## Delivered

- Replaced the S1 placeholder with a session-owned Tokio runtime (two workers) and embedded `scooter_core::app::App`.
- Added the complete S2 FFI surface: engine creation from Helix's working directory, key dispatch, non-blocking event pumping, busy state, frame/cursor rendering, reset, and quit. Every entry point remains panic-contained.
- Added the isolated key decoder and unit coverage for named keys, modifiers, character canonicalization, and invalid codes.
- Added the minimal native fields/results view: seven boxed search fields, collapse to the first two while results are focussed, result banner/status/time, selectable relative-path result rows, display-width-aware clipping, and text cursor placement.
- Restored persistent Steel session semantics for `:scooter` / `:scooter-new`, including hiding, quit teardown, cursor forwarding, and a 50ms background poll loop.
- Updated the smoke assertion to the stable `Search text` marker and added `scripts/e2e-live-search.sh` covering live debounced counts (5 then 2), hide/resume, quit, and fresh-session reset.
- Added a headless integration-style engine test using a temporary fixture and real debounced searches.

## Deviations

None beyond S2's intentional minimal view: popup and toast state is handled by core and escape closes it, but popup/toast content is not rendered until E5. Preview and footer rendering remain out of scope.

## Upstream needs for U1

None discovered in S2. The existing `EventChannels::try_recv` and currently public App/state accessors were sufficient.

## Validation

Run from the repository root:

```sh
scripts/check.sh
bash -n scripts/e2e-env.sh scripts/e2e-smoke.sh scripts/e2e-live-search.sh
git diff --check
scripts/e2e-smoke.sh && scripts/e2e-smoke.sh
scripts/e2e-live-search.sh && scripts/e2e-live-search.sh
```

All commands passed. The repeated e2e runs each rebuilt and installed the dylib only into the isolated scratch `STEEL_HOME`, then launched isolated Helix instances in tmux.

No commit was created and `../scooter` was not modified.
