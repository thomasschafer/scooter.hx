# E5: footer hints, popups, toasts, replacement + results screens

Read `docs/REWRITE-PLAN.md` for context. S1/S2/E3/E4 are committed: fields and results/preview render with TUI fidelity. E5 completes the screen set so the entire replace flow is visible: footer key hints, the three popups, toasts, the performing-replacement screen, and the final results screen. References in `../scooter/scooter/src/ui/view.rs`: `render` (footer/header structure), `render_key_hints`, `render_error_popup`, `render_help_popup`, `render_text_popup`, `render_paragraph_popup`, `render_table_popup`, `popup_width`, `get_popup_area`, `render_toast`, `render_performing_replacement_view`, `render_results_view` + `render_results_success` / `render_results_errors` / `render_results_tallies` / `error_result`. `../scooter` is read-only.

## Overlay mechanism

Runs are blitted in order and later runs overwrite earlier cells, so overlays are: first space-filled background rows (a run of spaces per row) with a new tag `"popup"` (add to the Steel style table -> `ui.popup`), then the overlay content on top. Nothing else changes in the pipeline.

## Deliverables

1. Footer key hints (all screens): reserve the last row of the frame; render `app.keymaps_compact()` joined as `<key> action / <key> action ...`, centred, truncated to fit, tag `"info"` (the TUI renders hints blue). The rest of the layout must account for the reserved row (results/list/preview heights shrink by one).

2. Popups, rendered over whatever screen is active when `app.popup()` is `Some`:
   - Geometry: width = 85% of frame width capped at 125; centred both axes (mirror `popup_width`/`get_popup_area`). Height fits content, capped at 80% of frame height.
   - Error popup (`Popup::Error`): title `Errors`; for each `AppError` in `app.errors()`: name line tag `"active"`, detail lines tag `"error"`, blank line between errors (mirror `render_error_popup`).
   - Help popup (`Popup::Help`): two-column table from `app.keymaps_all()`: right-aligned key column tag `"info"`, action column tag `"text"`, title `Help` (mirror `render_help_popup`).
   - Text popup (`Popup::Text { title, body }`): title + body lines, tag `"text"`.
   - All popups get a border (box chars, tag `"popup"`) and the space-filled background. Long content truncates; do not implement scrolling.

3. Toast: when `app.toast_message()` is `Some`, a bordered 3-row box, bottom-centred just above the footer, sized to the message (+2 padding each side), border tag `"diff-added"` (TUI uses green), message tag `"text"`, with space-filled background (mirror `render_toast`). Toasts already appear/dismiss via App + the poll loop.

4. Performing-replacement screen (`Screen::PerformingReplacement(state)`): centred vertically: `Performing replacement...` (tag `"text"`), blank line, then `Completed: <n>/<total> (<pct>%)` and `Time: <s>.<ms>s` centred (tag `"info"`), using `state.num_replacements_completed` (AtomicUsize), `state.total_replacements`, `state.replacement_started` (mirror `render_performing_replacement_view`).

5. Results screen (`Screen::Results(replace_state)`):
   - Tallies: three 3-row bordered boxes stacked (border+title tag `"text"`): `Successful replacements (lines):`, `Ignored (lines):`, `Errors:` with their numbers inside.
   - No errors: `Success!` (tag `"diff-added"`) centred above the tallies (mirror `render_results_success` layout).
   - With errors: tallies at top, then `Errors:` heading and a scrolling list of errors — each error: blank line, `path:line` (tag `"text"`), error message (tag `"error"`) — starting from `replace_state.replacement_errors_pos` and filling the remaining height (mirror `render_results_errors` + `error_result`; `display_error()` on the result gives `(path_display, error)`).
   - Keys already work through App (scroll j/k, enter/q to quit -> engine returns "quit").

6. Tests:
   - Extend the render-grid property test to cover: an open help popup, a toast, the performing-replacement screen, and both results-screen variants.
   - Headless integration: `C-h` opens help popup (runs contain key/action pairs) and esc closes it (popup dismissed, window NOT hidden — verify the engine returns "rerender" not "hide" when a popup is open); `A-m` shows a `Multiline: ON` toast and it dismisses via pump within ~2s (poll); full replace flow: search on a disposable tempdir fixture -> `enter` (focus results) -> `enter` (replace) -> pump until `Screen::Results` -> render shows the tallies with the right numbers -> files on disk actually changed; results screen with a forced error state renders the error list (construct a `ReplaceState` with errors directly if easier than forcing a real failure — a unit-style view test is fine for this variant).
   - e2e `scripts/e2e-replace.sh`: IMPORTANT: replacement mutates files, so the script must create its own throwaway fixture copy per run (never reuse `$SEARCH_FIXTURE_DIR` in place). Flow: open, search a known token, type a replacement in the replace field, `enter` to results, `enter` to replace, wait for the results screen (`Successful replacements`), assert the on-disk file content changed (grep the fixture copy), press `enter` and assert the window closed, then `:scooter` shows a fresh session (the quit path destroyed the old one). Also cover: `C-h` help popup appears and esc dismisses it while the window stays open; `A-m` toast text appears and then disappears.

## Acceptance criteria

- `scripts/check.sh` passes; all e2e scripts (`smoke`, `live-search`, `sizes`, `preview`, new `replace`) pass twice consecutively.
- Include captures of the help popup and the results screen in `docs/specs/E5-REPORT.md`.
- No modifications to `../scooter`. Do not commit.

## Notes

- Footer reservation changes existing layout maths — update E3/E4 tests that assume the old heights rather than weakening assertions.
- The buffer in the editor behind the window will NOT reflect replaced file contents in e2e (document reload is H3); assert against the filesystem, not the visible buffer.
- Popup content lines come from multi-line strings (`AppError.long` can contain newlines) — split on `\n` like the TUI does.
- `Screen::Results` keymaps include `enter`/`q` -> `CommandResults::Quit` which App maps to an Exit result; the engine already turns that into "quit". Esc with a popup open must keep being routed to App (existing `should_hide_for_escape` only hides when no popup — verify it also considers non-SearchFields screens correctly: on Results/PerformingReplacement esc is unmapped, App returns None, engine returns "rerender"; the window should NOT hide on esc there — that matches the TUI where esc does nothing on those screens. Add a test pinning this).
