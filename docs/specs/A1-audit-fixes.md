# A1: colour architecture audit + code review fixes

Read `docs/REWRITE-PLAN.md` for context. Everything through E7 is committed. A1 is a reviewed audit backlog: work through the items IN ORDER, validating as you go (see the working loop at the bottom). You have latitude to fix additional issues of the same kind you discover along the way — document every extra fix in the report. Behaviour and TUI parity must not regress; the E7 snapshot suite is your tripwire.

## Part 1: colour architecture (highest priority — user-visible bug)

Reported bug: with themes where `ui.popup`'s background differs from the editor background, the error popup's interior background and its border background differ (interior took one colour, border the other). Root cause in `ui/window.scm`: `popup-border`/`toast-border` build their explicit background from `background-colour` (the editor background) while the `popup` fill keeps `ui.popup`'s own bg when the theme sets one.

1. Introduce a single resolved "surface" concept in `style-table`: `surface-bg` = `ui.popup`'s bg if set, else the editor background. EVERY overlay-related style — `popup` fill, `popup-border`, `toast-border`, and anything else painted over arbitrary content — takes bg = `surface-bg`. The main window's block style and the ` Scooter ` title should also use the same resolved popup style so window and popups agree.
2. Write the styling invariant down as a comment table in `window.scm`: each tag is either (a) "overlay: explicit fg+bg from surface" or (b) "content: fg-patch, inherits the fill beneath". Audit every tag in the table against this rule and fix inconsistencies. Content tags rendered inside popups sit on the popup fill, so they stay rule (b).
3. Verify and simplify the accessor chains: `(style->fg (theme->fg *helix.cx*))` — check what `theme->fg`/`theme->bg` actually return in this helix build (see `components.rs` in the helix checkout / the generated cogs docs) and make the fallback chain type-correct with `safe-theme-scope`-style guards rather than accidental success.
4. `selection-excluded` uses the error scope's fg as a background with the theme fg on top — check readability under catppuccin_mocha and the default theme in real captures; if unreadable, use the surface bg colour as the row fg (the TUI uses near-white on red).
5. Harden the themed e2e: extend `e2e_assert_popup_border_has_uniform_style` (or add a sibling assertion) to also assert the popup INTERIOR background equals the border background, by parsing bg SGR codes from `capture-pane -e` for a row inside the popup vs the border row. Must fail if item 1's bug regresses.

## Part 2: code review fixes (in priority order)

6. Logging: replace the `eprintln!` calls (`src/lib.rs` panic guard, `src/engine.rs` ExitAndReplace arm) with the `log` crate wired to a file logger (the OLD plugin's `src/logging.rs` on the `main` branch is the pattern: simple-log into a cache-dir file). stderr writes from a dylib inside a fullscreen TUI can corrupt the terminal. Log level: default warn; keep it cheap.
7. Unknown-tag safety in Steel: `blit-run!` uses `hash-ref` which errors on an unknown tag, killing the render callback if Rust ever emits a tag Steel doesn't know. Fall back to the `text` style (and log once via `log::warn!`) instead.
8. Make style tags a Rust enum (`StyleTag` with `as_str()`), replacing the stringly-typed `tag: String`/string literals throughout `view.rs`/`engine.rs`. Wire format over FFI is unchanged (strings). This is typo-proofing; zero snapshot changes expected.
9. Split `src/view.rs` (2,200+ lines) into focused modules (suggested: `view/mod.rs` with `layout`, `fields`, `results`, `preview`, `overlays`, `banner`, and a `canvas` module for Run/add_run/add_segment/truncate/width helpers). PURE code motion — the E7 snapshots must be byte-identical afterwards (that is the acceptance test for this item).
10. Engine teardown ergonomics: `self.runtime.as_ref().expect("active runtime")` appears several times and any FFI call after `quit` panics into the guard. Add a single helper returning `Option`, and make post-quit calls clean no-ops returning safe defaults (`"idle"`/empty frames/false). Add a test: handle-key/pump/render/busy? after quit neither panic nor log panics.
11. Wrap parity: the TUI's `wrap_lines` continuation lines have NO prefix marker; ours adds `↪ `. Remove the marker to match the TUI (check `wrap_lines` in the TUI source to confirm, and mirror). Update affected snapshots/tests.
12. Steel naming: `scooter-response-status` is a side-effecting consumer named like a getter — rename to something honest (e.g. `consume-scooter-response!`) and update call sites.
13. Small sweeps: dead code paths in `view.rs` (e.g. narrow-box special cases — delete if unreachable, keep if reachable at tiny sizes per the property test); make sure every e2e script uses the shared cleanup trap; confirm the snapshot canvas's tag legend is emitted consistently (the report for E7 claims a per-row legend; some snapshots appear canvas-only — make it consistent either way and regenerate).

## Working loop (you are expected to run long and self-validate; no human in the loop)

- Work items in order. After each item: `scripts/check.sh`; plus the e2e scripts most relevant to the item (item 1/5: `e2e-replace.sh`; item 9/11: review `cargo insta` diffs carefully).
- Snapshot changes: item 9 must produce none; items 11/13 may — review each diff and justify it in the report.
- When all items are done: run the FULL validation twice consecutively — `scripts/check.sh` and every `scripts/e2e-*.sh`.
- Then do a fresh-eyes visual pass yourself: tmux captures (plain and `-e`) of fields, results+preview, error popup, help popup, toast, results screen, under BOTH the default theme and catppuccin_mocha, at 120x40 and 220x55. Look for any remaining colour inconsistency of the same family as item 1 — fix and document anything you find.
- Write `docs/specs/A1-REPORT.md`: per-item outcome, extra fixes, snapshot-diff justifications, capture excerpts for the popup fix (before/after if practical), and full validation transcript summary.

Do not commit. Do not modify `../scooter`.
