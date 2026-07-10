# E4: results list + preview pane

Read `docs/REWRITE-PLAN.md` for context. S1/S2/E3 are committed: the engine embeds `App`, and the fields screen renders with TUI fidelity. E4 delivers the results area: the list/preview split, faithful result rows, and a native preview pane with diff highlighting. Reference: `../scooter/scooter/src/ui/view.rs` (`render_search_results`, `build_search_results`, `search_result`, `file_path_line`, `line_list`, `build_preview_from_file`, `compute_detailed_diff`, `simple_diff`, `build_multiline_diff`, `wrap_lines`, `diffs_to_lines`) — mirror behaviour and layout, not ratatui mechanics. `../scooter` is read-only; if an upstream change is needed, record it in your report and work around it only if trivial.

Per the project plan there is NO syntax highlighting in the preview (that is future work); preview text is plain with diff colouring. The TUI's async highlight tasks and LRU caches are deliberately not ported. Read files synchronously per render; only the visible window of lines is read, so this is cheap.

## Layout

Inside the existing layout (fields, one-row gap, banner at `banner_y`, then the results area below the banner's 2 rows):

- Wide layout (results-area width > 110): list on the left (2/5 of the width), one-column gap, preview on the right (remainder). Both full height of the results area.
- Narrow layout (<= 110): list on top (5 rows), one-row gap, preview below filling the rest. (Mirror `render_search_results`'s `small_screen` behaviour, including `num_to_render = 5`.)
- `num_displayed`/`view_offset` clamping as now, driven by the actual list height.

## Result rows (mirror `search_result` in the TUI)

- `[x] `/`[ ] ` inclusion marker, then `path:line`, truncated with a leading ellipsis (keep the END of the path visible — see `last_n_chars` usage / the old plugin's `truncate-str-with-ellipsis`) so long paths stay useful.
- Selection: the TUI highlights selected rows with the selection style and marks included rows bold. Map: primary-selected row -> tag `"selection"`; rows inside a multiselect range but not primary -> new tag `"selection-secondary"`; others `"text"`. Add `"selection-secondary"` to the Steel style table in `ui/window.scm` (map to the `ui.selection` scope plus `style-with-dim`, so ranges are visible but the primary stands out). Line-number portion uses tag `"info"` on unselected rows (the TUI styles line numbers distinctly).
- The multiselect commands already work through App (`v`, `esc`, `alt-;` etc.); this chunk only renders the state.

## Preview pane

For the primary selected result:

1. If `preview_error` is set on the result: render `Error generating preview: <error>` with tag `"error"` and nothing else.
2. Single-line results (`MatchContent::Line`): read a window of file lines centred on the match line, sized to the preview height (`scooter_core::utils::read_lines_range` + `largest_range_centered_on` / `split_indexed_lines` — check what fits best; the old plugin's `try_build_preview` on `main` is also a reference). Context lines: `(<line-number>) <text>` with the number tag `"dim"` and text tag `"text"` (mirror the TUI's `line_list` numbering style). At the match position render two lines: the old line prefixed `- ` (tag `"diff-removed"`) and the replacement prefixed `+ ` (tag `"diff-added"`), with word-level emphasis from `scooter_core::diff::line_diff`: segments whose `Diff` has a background colour set are emphasised — render those with new tags `"diff-removed-emph"`/`"diff-added-emph"` (add to the Steel style table: base diff scope with `style-with-reversed`). Strip control chars (`strip_control_chars`).
3. Multiline results (`MatchContent::ByteRange`): mirror the TUI's `simple_diff`-equivalent as the baseline: old content lines each prefixed `- `, new content lines each prefixed `+ `, with surrounding context lines from the file. Then, matching `compute_detailed_diff` behaviour synchronously with a size cap: if `old.len() + new.len() <= 20_000` bytes, compute the word-level multiline diff (mirror `build_multiline_diff`'s output structure); otherwise fall back to the simple form. No async, no cache — note this simplification in your report.
4. If the file content changed since the search (context read shows a different line at the match position than the recorded one), show the same error the TUI/old plugin shows (`File content has changed` style message) rather than a bogus diff — check how `expected_first_line_content` is used in the TUI and mirror the guard.
5. Wrapping: when `app.config.preview.wrap_text` is true (toggleable at runtime via the existing `C-l` keymap through App), wrap preview lines to the pane width, mirroring `wrap_lines` (prefix continuation lines correctly; keep it simpler than the TUI if needed but document any divergence). When false, truncate at the pane width.

## Tests

- Extend the render-grid property test to cover frames where the results+preview render (search complete with results), asserting bounds/no-overlap as before across the size grid.
- Headless integration coverage: preview shows context + `- `/`+ ` lines for a known fixture; `j`/`k` moves selection and the preview follows; `space` toggles the marker of the selected row; `a` toggles all; multiselect `v` + `j` renders a `selection-secondary` row; wrap toggle (`C-l` key event, ctrl modifier bit) changes long-line rendering.
- e2e `scripts/e2e-preview.sh`: fixture with a file whose matched line has recognisable context above/below; open, search, assert context text, `- `, and `+ ` lines appear in the pane; press `j` and assert the preview changed to the second result's content; press `space` and assert the selected row shows `[ ]`. Run at both a wide (160x45) and a narrow (100x30) size to exercise both layouts.

## Acceptance criteria

- `scripts/check.sh` passes; all e2e scripts (`smoke`, `live-search`, `sizes`, new `preview`) pass twice consecutively.
- Include a 160x45 capture of the results+preview area in `docs/specs/E4-REPORT.md`, and compare it structurally against the TUI screenshots referenced in the plan (list left, preview right with `(n)` numbering and coloured diff lines).
- No modifications to `../scooter`. Do not commit.

## Notes

- The preview reads the file fresh each render; if the file vanished, surface the read error via the preview error path, never panic.
- Keep diff-segment runs adjacent-but-not-overlapping (the property test enforces this).
- `line_diff` returns `(Vec<Diff>, Vec<Diff>)` for (old, new); each `Diff` has `text`, `fg_colour`, `bg_colour: Option<..>`. Treat `bg_colour.is_some()` as "emphasised segment".
- Check `MatchContent`'s shape in `scooter-core/src/search.rs` (`Line { content, .. }` vs `ByteRange { content, byte_start, byte_end, .. }`) and `start_line_number`/`end_line_number` helpers.
