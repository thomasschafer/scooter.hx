# E7: headless frame snapshot suite

Read `docs/REWRITE-PLAN.md` for context. S1-E6 and F1 are committed. E7 consolidates rendering coverage into an insta snapshot suite over the engine's frame output. Do NOT duplicate scooter-core's own behaviour tests (`../scooter/scooter-core/tests/app.rs` covers App semantics like popup precedence, unlock flows, toggles); the value here is snapshotting the PLUGIN's rendering of those states, so view regressions show up as reviewable text diffs.

## Infrastructure

1. Add `insta` as a dev-dependency (match the version scooter uses).
2. A snapshot renderer: turn a `Frame` into deterministic text — the character canvas (runs painted in order onto a width x height grid), followed by a compact per-row tag legend (e.g. `row 3: 0-1 text | 2-14 active | ...`). Cursor position appended when present.
3. Determinism rules (critical):
   - insta filters to normalise `[Time taken: <...>]` and absolute tempdir paths.
   - Multi-file searches complete in nondeterministic ORDER (parallel walker). Frame snapshots must therefore use single-file fixtures (or fixtures where all matches are in one file); use plain assertions (not snapshots) for anything multi-file.
   - No snapshots of mid-search states (racy); always pump to completion first.

## Scenarios (one snapshot each unless noted, at 120x40 plus a second size where layout differs)

- Fields screen, fresh engine (all seven fields, checkbox states, cursor position).
- Fields with text in every field and a checkbox toggled.
- Field error (invalid regex) + the error popup open (two snapshots: popup closed via esc shows the field error remains).
- Live search complete, fields focussed vs results focussed (collapse), wide and narrow layouts.
- Preview: single-line diff with word emphasis; multiline diff (multiline mode via A-m, pattern spanning lines); wrap on vs off (long line fixture); preview error (file deleted after search); file-changed guard (file modified after search).
- Multiselect: range of 3 with primary at the bottom; flip direction; an excluded row inside the range.
- Help popup on fields focus and on results focus (keymap content differs).
- Toast visible (multiline ON).
- Performing-replacement screen (normalise the progress numbers via filters).
- Results screen: success variant; errors variant (construct `ReplaceState` directly with 2+ errors, scrolled and unscrolled).
- Empty-search and pre-debounce banner states.

## Acceptance criteria

- `scripts/check.sh` passes (snapshots committed under `src/snapshots/` or `tests/snapshots/` per insta convention — these SHOULD be left in the working tree for review, still do not git-commit).
- Existing unit tests that duplicate what a snapshot now covers may be simplified/removed where clearly redundant — note removals in the report.
- All e2e scripts still pass once each (no rendering changes expected; if a snapshot reveals a rendering bug, FIX it and call it out prominently in the report).
- Write `docs/specs/E7-REPORT.md`.
- Do not commit. Do not modify `../scooter`.
