# E3: search fields renderer — TUI fidelity, errors, responsiveness

Read `docs/REWRITE-PLAN.md` for context. S1/S2 are committed; the engine embeds `scooter_core::app::App` and `src/view.rs` renders a minimal fields+results view. E3 brings the fields section to full fidelity with the scooter TUI and makes the frame model robust at any terminal size. The reference implementation is `../scooter/scooter/src/ui/view.rs` (`render_search_fields`, `render_search_field`, `create_title_spans`, `render_num_results`) — study it and mirror its behaviour, not its ratatui mechanics. `../scooter` is read-only.

Product feedback shaping this chunk (from the reviewer of the S2 milestone): checkboxes must match the TUI (small box + label, not a full-width titled box), and the UI must degrade gracefully across terminal sizes.

## Deliverables

1. Checkbox fields, TUI style: a 5-cell-wide, 3-row bordered box containing ` X ` (checked) or blank, with the field title rendered as plain text to the right of the box, vertically on the middle row, one cell gap. No title in the border. Title text and box border use tag `"active"` when the field is highlighted, else `"text"`.

2. Text fields (mostly as now) plus error display: when `field.error()` is `Some`, append ` (Error: <short>)` to the title using tag `"error"` for that segment (the title itself keeps its normal tag). This means a run row may need multiple segments — extend the title rendering to emit adjacent runs with different tags rather than one string. Border tag stays `"active"`/`"text"` (errors do not change border colour in the TUI; verify against `create_title_spans`/`render_search_field` and copy what the TUI actually does).

3. Banner fidelity (`render_num_results` is the reference):
   - Left: `Results: <n> [<status>]` as now.
   - Right-aligned at the right edge of the layout width: `[Time taken: <s>.<ms>s]` when available (tag: `"diff-added"` when complete, `"info"` otherwise — mirror the TUI's green/blue split).
   - Middle: `[Updating preview: <a>/<b> (<pct>%)]` (tag `"info"`) when `search_fields_state.replacements_in_progress()` returns Some and total >= 10000 (mirror `preview_update_status`).
   - Empty-state parity: `Results: 0 [Search is empty]` only when the search text is empty; when text is non-empty but `search_state` is `None` (pre-debounce window), render nothing in the results area, exactly like the TUI.

4. Cursor: clamp the cursor x so it never leaves the field's inner box (right edge = box interior end), in both `render` and `cursor`. Keep everything else as is.

5. Responsiveness hardening:
   - A property-style Rust test that renders frames for every (width, height) in a grid covering 0..=3 exhaustively plus a spread of realistic sizes (e.g. widths {10, 24, 60, 79, 80, 81, 110, 111, 160, 250}, heights {4, 10, 23, 24, 40, 55, 80}), against an App with an active search and results, asserting: no panics, no run exceeds the frame bounds (x + display-width(text) <= width, y < height), and no overlapping runs on the same row with the same y that would garble output (adjacent segments from deliverable 2 are allowed to touch, not overlap).
   - Degradation rules: if the full field stack does not fit the height, render as many whole fields as fit (as now); the banner and results only render when there is room. No panics or subtraction overflows at 1x1.

6. e2e: extend the harness with a size-matrix smoke run — `scripts/e2e-sizes.sh` launching hx at 80x24, 120x40, and 220x55, opening `:scooter`, typing a search, and asserting the banner text and at least one field title appear at every size (at 80x24 assert on a field title only if the banner cannot fit — determine what should be visible and assert accordingly; the point is catching layout crashes and glaring clipping, not pixel equality).

7. Update the headless integration test to cover: checkbox toggle via space reflected as ` X ` in the checkbox box, error display after typing an invalid regex (`(` as search text with fixed strings off → the search field shows an error title segment and the banner shows `Invalid search`), and the collapse-to-2-fields behaviour when focus moves to results (send `enter` with a completed search).

## Acceptance criteria

- `scripts/check.sh` passes; `scripts/e2e-smoke.sh`, `scripts/e2e-live-search.sh`, and the new `scripts/e2e-sizes.sh` each pass twice consecutively.
- Screenshot-check your own work: capture tmux panes at the three sizes and compare the fields section against the TUI screenshots' structure (checkbox boxes, title-in-border text fields). Include the 120x40 capture (fields + banner area, as text) in your report.
- No modifications to `../scooter`. Do not commit. Write `docs/specs/E3-REPORT.md` with deviations, upstream needs (for U1), and validation commands.

## Notes

- Multi-segment rows: keep the `Run` model unchanged (adjacent runs with different tags); the Steel blitting already handles arbitrary runs.
- `SearchField::set_by_cli` is always false in the plugin (no CLI); ignore the TUI's blue set-by-cli styling.
- Look at how the TUI centres the checkbox title vertically (`checkbox_text` two-line construction) — the title sits on the box's middle row.
- Beware display-width vs char-count in the bounds test (unicode titles/errors).
