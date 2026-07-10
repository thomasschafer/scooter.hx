# E6: pump/session lifecycle hardening

Read `docs/REWRITE-PLAN.md` for context. S1-E5 and F1 are committed. E6 hardens the engine lifecycle: the action queue that H3 will consume, hide/resume correctness, reset semantics, and clean teardown. Reference behaviour: `../scooter/scooter/src/app_runner.rs` (read-only).

## Deliverables

1. Action queue: replace the log-and-drop handling of `Event::LaunchEditor` with a real queue. `Scooter-pump` changes its return value from a status string to a list: first element the status string (`"rerender"`/`"idle"`), followed by zero or more actions, each a list: `("open-file" <path-string> <line-int>)`. `Scooter-handle-key`'s post-key drain must also surface actions — change its return to the same list shape (status first: `"rerender"`/`"hide"`/`"quit"`, then actions). Update `ui/window.scm`/`scooter.scm` to consume the new shapes; Steel logs and ignores `open-file` actions for now (H3 wires them to helix.open), but the plumbing and tests land here.

2. Clean teardown: `Scooter-quit` (and engine drop) must never block Helix's UI thread. Tokio's `Runtime` drop waits for blocking tasks; a large in-flight search could stall the editor. After signalling cancellation, shut the runtime down with `shutdown_background()` (this requires taking the runtime out of the struct — restructure as `Option<Runtime>` or equivalent). Add a regression test: quit immediately after starting a search over a large generated fixture (thousands of files; generate under a tempdir in the test) and assert quit returns quickly (< ~100ms).

3. Hide/resume correctness (integration tests; the mechanisms mostly exist):
   - Search continues while hidden: start a search over a fixture large enough to still be running, stop pumping (simulates hidden window), wait, then pump once and assert the phase reaches complete with results intact.
   - Toast scheduled before hiding dismisses correctly on resume (pump after the toast duration passes -> toast gone).
   - Actions queued while hidden are delivered on the first pump after resume.

4. Reset semantics: `Scooter-reset` (used by `:scooter-new` and `C-r` through App) mid-search must not leak stale events into the fresh state: reset while a search is mid-flight, then pump repeatedly and assert no results appear and the state stays pristine (empty search field, no search state). If stale `BackgroundProcessingEvent`s from the old receiver can bleed through, drain/drop them on reset (check what `App::reset` already guarantees — the receivers are replaced with the new App, so the old channel should die with it; write the test to prove it).

5. `busy?` audit with tests: exactly true when (a) debounce timer pending, (b) search phase Pending/Running, (c) preview updates in flight, (d) PerformingReplacement screen, (e) toast visible. Exactly false on: fields idle with complete/invalid search, Results screen with no toast. One test per row.

6. Panic-path degradation: after a caught FFI panic (force one in a test via a small test-only hook or by constructing a poisoned state), subsequent calls must still return safe values and not panic again. At minimum add a test that `render`/`pump`/`busy?` on a fresh engine never panic across the size grid with popups/toasts active (extend the existing property test if not already covered).

7. e2e `scripts/e2e-lifecycle.sh`: generate a throwaway fixture with enough files to make search take a couple of seconds (e.g. 2000 small files under `.dev/fixtures/lifecycle`, created by the script); flow: `:scooter`, type a query, immediately esc (hide) while searching, wait ~3s, `:scooter` (resume) and assert the completed result count appears without retyping; `C-r` and assert fields are reset to empty; type a query again, `C-c` mid-search and assert the window closes promptly (session destroyed); `:scooter` fresh and empty. Pass twice consecutively.

## Acceptance criteria

- `scripts/check.sh` passes; all e2e scripts (smoke, live-search, sizes, preview, replace, new lifecycle) pass twice consecutively.
- No modifications to `../scooter` (if you conclude an upstream change is genuinely required for stale-event isolation, write it up in the report instead of implementing).
- Do not commit. Write `docs/specs/E6-REPORT.md` with deviations, upstream needs, and validation commands.

## Notes

- The pump/handle-key return-shape change touches the Steel FFI contract — keep the encoding simple (lists of strings/ints) and update every Steel call site; the e2e suites are the safety net.
- `shutdown_background` abandons blocking tasks; the search walker polls the cancellation flag, so tasks die soon after anyway. Signal cancellation BEFORE shutdown.
- Keep DRAIN_LIMIT behaviour; actions must not be lost when the limit is hit mid-drain (they are just queued for the next pump).

## Riders (small fixes bundled into this chunk)

8. Popup borders: the TUI's `create_popup_block` uses a green border (`border_style(Color::Green)`) on all popups. Introduce a `"popup-border"` tag for popup border runs (title keeps its current tag), mapped in the Steel style table to the same green-ish scope used for `diff-added`, with the explicit fg/bg discipline from F1.

9. Harden the popup border uniformity e2e: set `theme = "catppuccin_mocha"` in the harness helix config (`e2e-env.sh`) so popup/field/background colours genuinely differ (text-based assertions are unaffected since plain captures strip colours), and tighten `e2e_assert_popup_border_has_uniform_style` to inspect the span from the popup's own top-left corner to the title (not the whole row), so coincidental theme uniformity can't mask regressions.
