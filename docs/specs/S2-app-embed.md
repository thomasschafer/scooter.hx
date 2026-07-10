# S2: embed scooter-core App — live search end to end

Read `docs/REWRITE-PLAN.md` for context, and `docs/specs/S1-toolchain-spike.md` for the established FFI/key contracts. S1 is done: the dylib loads, themed runs blit into a popup, keys round-trip. S2 replaces the static demo with the real application state machine and proves live search works in a real Helix session. This chunk's view is deliberately minimal (no preview pane, no popups/toasts rendering); it becomes the foundation that later chunks extend.

## Environment facts

- `scooter-core` lives at `../scooter/scooter-core` (path dependency). The `../scooter` checkout is on branch `new-plugin-changes`, which already contains `EventChannels::try_recv`. Treat `../scooter` as read-only: study it as much as you like (especially `scooter-core/src/app.rs` and the TUI's `scooter/src/ui/view.rs` + `scooter/src/app_runner.rs` for reference behaviour), but do not modify it. If you believe an upstream change is required, stop and write the need into your report instead of working around it.
- Validation commands: `scripts/check.sh` (build + clippy + test on a pinned toolchain; do not run bare `cargo clippy`, it fails on this machine for toolchain reasons), `scripts/e2e-smoke.sh`, and the new e2e script you will write.
- First build after adding scooter-core will be slow (tokio, rayon, two-face/onig C code). That is expected.

## Architecture requirements

Engine struct (replaces the S1 placeholder):

- Owns a tokio multi-thread `Runtime` (2 worker threads) and a `scooter_core::app::App`.
- `App::new(InputSource::Directory(dir), &SearchFieldValues::default(), AppRunConfig::default(), Config::default())`.
- CRITICAL: every call into `App` that can spawn tasks or timers (`handle_key_event`, `handle_internal_event`, `perform_search_background`, `reset`, and `App::new` itself) must run inside the runtime context: hold `let _guard = self.runtime.enter();` first. Without it you get a "there is no reactor running" panic at the first debounce.
- All FFI entry points stay panic-guarded (S1 `ffi_guard` pattern). The engine is only ever touched from Helix's main thread; no additional locking needed.

## FFI surface (replacing/extending S1)

- `Scooter-engine-new (directory)` -> engine. Directory comes from Steel (`get-helix-cwd`, as the old plugin did — see `scooter.scm` on `main`).
- `Scooter-handle-key (engine code-string modifiers-int)` -> status string:
  - Translate to `scooter_core::keyboard::{KeyCode, KeyModifiers}` using the S1 key-name table (chars pass through as `KeyCode::Char`; shift=1, ctrl=2, alt=4). Call `key_event.canonicalize()` before dispatch. Keep this translation in its own module with unit tests — E2 extends it.
  - Escape special-casing BEFORE forwarding to App: if `app.show_popup()` is false AND the current screen is `Screen::SearchFields` with `focussed_section == FocussedSection::SearchFields`, return `"hide"` without forwarding (the window hides; session stays alive). In every other case forward esc to App (it closes popups / goes back to fields). Multiselect refinement comes later (E2).
  - Forward everything else to `app.handle_key_event`. Map the result: `Exit(..)` -> cancel in-progress tasks (`app.cancel_in_progress_tasks()`) then `"quit"`; anything else -> `"rerender"`.
  - After any forwarded key, drain ready internal events (same as pump below) before returning, so the next render reflects them.
- `Scooter-pump (engine)` -> status string `"rerender"` or `"idle"`: non-blocking drain loop (cap ~1000 iterations): `app.event_channels.try_recv()` -> on `Event::Rerender` note rerender; on `Event::Internal(e)` call `app.handle_internal_event(e)` (inside runtime guard) and note rerender; on `Event::LaunchEditor(..)` log and ignore (H3 wires this up); `Event::ExitAndReplace` is unreachable for directory input — log if seen. Also drain `app.background_processing_reciever()` the same way via its `try_recv`, feeding `app.handle_background_processing_event`. Return `"rerender"` if anything was processed.
- `Scooter-busy? (engine)` -> bool: true when more background events are expected, i.e. any of: current screen is `Screen::PerformingReplacement`; screen is `SearchFields` and (`search_debounce_timer.is_some()` or `preview_update_state.is_some()` or (`search_state` present and its `phase` is not complete... note: `SearchPhase::Invalid` expects no further events, treat as not busy; `Pending`/`Running` are busy)); or `app.toast_message().is_some()`. All these fields/methods are public on the App/state types — check `app.rs` for exact shapes.
- `Scooter-render (engine width height)` -> runs, same wire format as S1.
- `Scooter-cursor (engine width height)` -> `(x y)` list or `#f` (FFIValue boolean false): position of the text cursor within the drawn area when the fields section is focussed on a text field and no popup is open; otherwise false.
- `Scooter-reset (engine)`: calls `app.reset()` (inside runtime guard). Used by `:scooter-new`.
- `Scooter-quit (engine)`: calls `app.cancel_in_progress_tasks()`. Used when destroying the session.

## Minimal native view (new `src/view.rs`)

Produce `Vec<Run>` + cursor from `&mut App` and (width, height). Mirror the TUI's structure (`../scooter/scooter/src/ui/view.rs::render` is the reference) but keep S2 scope small:

- Search fields stack, centred vertically in the top section: one 3-row box per field (7 fields), built from unicode border chars (`┌─┐│└┘`) as runs. Field title in the top border, value inside (text fields: the text; checkboxes: `[X]`/`[ ]` plus title). Highlighted field gets tag `"active"` on its border+title; others `"text"`. When `focussed_section == SearchResults`, render only the first 2 fields (search + replace), exactly like the TUI collapses.
- Results banner (one line under the fields): `Results: <n> [<status>]` where status is one of `Search is empty` (no search state and empty search text, tag "error"), `Invalid search` (phase Invalid, tag "error"), `Still searching...` (tag "info"), `Search complete` (tag "diff-added"). Include the elapsed time as ` [Time taken: <s>.<ms>s]` when phase reports it.
- Results list below the banner filling the remaining height: one row per result: `[x] `/`[ ] ` then `path:line`, relative path from the search directory (scooter-core `utils::relative_path`). Selected row: prefix `> ` and tag `"selection"`; others `"text"`. Update `search_state.num_displayed` and `view_offset` before slicing, replicating the TUI's clamp logic in `render_search_results` (keep the selected row visible with a 1-row margin top / 2-row margin bottom).
- No preview pane, no popups, no toasts, no footer hints in S2. (If a popup IS open, App state still changes; do not render it — esc still closes it via App. This is acceptable interim behaviour, noted in the report.)
- Truncate every run so nothing exceeds the width (unicode-width aware truncation; scooter-core's `utils` has helpers, or use the `unicode-width` crate which is already in scooter-core's tree).
- Cursor: for the highlighted text field, x = text box content start + `field.cursor_pos()` (visual position), y = the field's text row.

Unit-test the view against a hand-built App where practical, but the main coverage is the integration test below.

## Steel side

- `scooter.scm`: restore session semantics from the old plugin (see `scooter.scm` on `main` branch): a global `*scooter-session*` holding the engine; `(scooter)` resumes or creates; `(scooter-new)` calls `Scooter-quit` on any existing engine, then creates fresh. Provide both as typed commands. `"quit"` status from handle-key destroys the session and closes; `"hide"` just closes the component.
- Component (`ui/spike.scm` -> rename to `ui/window.scm`): as S1, plus:
  - `"cursor"` handler in the component hash returning a `position` built from `Scooter-cursor` (see old plugin's cursor handler shape on `main`).
  - Poll loop: when the component is pushed, start a self-rescheduling callback via `enqueue-thread-local-callback-with-delay` (~50ms): if the session is alive and the window is visible, call `Scooter-pump`, and keep rescheduling while `Scooter-busy?` is true; stop when the window closes (track visibility with a box owned by the window state; set it on close paths). Also run one `Scooter-pump` when resuming a hidden session so results that arrived while hidden appear immediately.
  - After `Scooter-handle-key` returns, the poll loop must be (re)started if `Scooter-busy?` became true (e.g. the keystroke scheduled a debounced search).

## e2e script `scripts/e2e-live-search.sh`

Create a dedicated fixture dir `$REPO/.dev/fixtures/search` (in `e2e-env.sh` or the script) with deterministic content where you KNOW the match counts, e.g. three files containing the token `alpha` on 5 lines total, and the token `alphabet` on 2 of those lines (so `alpha` -> 5, `alphabet` -> 2 — pick your own scheme but assert exact counts you control).

Flow (poll-based assertions like the smoke test, generous timeouts around the 300ms debounce):

1. Launch hx in the fixture dir, `:scooter`, assert the fields render (`Search text` visible).
2. Type the broad token; assert the banner reaches `Results: 5` and `[Search complete]`.
3. Continue typing to the narrow token (just send the remaining chars); assert `Results: 2` and complete.
4. Send `esc`; assert the window content disappears (editor visible again).
5. `:scooter` again; assert `Results: 2` still shown without retyping (session resumed).
6. Send `C-c`; assert closed. `:scooter` again; assert the search field is empty (fresh session).

Keep it reliable twice in a row; reuse the S1 harness helpers (extract shared wait functions into `e2e-env.sh` or a lib file if that keeps things DRY).

## Rust integration test

Add an integration test (in `tests/` or `src/` as appropriate) that exercises the engine without Helix: create engine on a tempdir fixture (recreate the old plugin's tempfile test pattern — see `src/scooter_hx.rs` + `src/test_utils.rs` on `main` branch for `create_test_files!`/`wait_until` ideas), feed key events char by char through the public FFI-facing functions, then poll pump/busy until the search completes (bounded wait), and assert: result count, that render output contains expected path rows, and that toggling a key like tab moves the highlighted field in the render output. `tempfile` as a dev-dependency is fine.

## Acceptance criteria

- `scripts/check.sh` passes.
- `scripts/e2e-smoke.sh` updated if needed (the popup content changed from the S1 demo — keep a stable marker like the `Search text` field title) and passes twice.
- `scripts/e2e-live-search.sh` passes twice consecutively.
- Rust integration test covers the search flow headlessly.
- No modifications under `../scooter`. No writes outside this repo and `/tmp`.
- Do not commit. Write `docs/specs/S2-REPORT.md`: what you did, deviations, upstream needs discovered (for U1), exact validation commands run.

## Gotchas

- Runtime guard (see above) — first debounced search will panic without it.
- `Config::default()` pulls in syntect theme loading; it is unused by our view but harmless. Do not enable scooter-core's `steel` feature.
- Key chars: `key-event-char` returns a char for printable keys including space; the translation must preserve case and pass shift through canonicalize (which strips SHIFT for chars).
- tokio `UnboundedReceiver::try_recv` returns `Err(Empty)` when nothing is queued — treat both Empty and Disconnected as end-of-drain.
- The debounce means a keystroke returns `"rerender"` with `busy?` true but no results yet; results arrive via pump ~300ms later. The e2e assertions must poll.
- tmux `send-keys` types literally by default when given a quoted string; use `C-c` key name syntax for ctrl-c and `Escape` for esc.
