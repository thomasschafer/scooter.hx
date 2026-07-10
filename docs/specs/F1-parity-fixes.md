# F1: TUI parity fixes + popup colour-bleed bug

Read `docs/REWRITE-PLAN.md` for context. S1-E5 are committed. F1 is a bundle of fixes from the product owner's milestone review, all verified against the TUI source (`../scooter/scooter/src/ui/view.rs`, read-only). Two of them are bugs, the rest are parity corrections.

## 1. Popup colour-bleed bug (priority)

Observed: the Errors popup border is not a uniform colour — where it overlaps underlying content (field borders, checkboxes), the underlying colours show through (screenshot showed purple segments in an otherwise white border).

Cause: Helix's `frame-set-string!` patches styles — attributes the style leaves unset keep the previous cell's value. Our overlay tags resolve via `theme-scope` (e.g. `ui.popup`), and many themes leave fg or bg unset on such scopes, so border/background runs inherit fg/bg from whatever was painted underneath.

Fix (Steel side, `ui/window.scm`): every overlay-related style — the `popup` tag used for backgrounds/borders, and by extension any style used to paint over existing content — must carry an EXPLICIT fg and bg. Build them by starting from a base with fg = the theme's foreground (`theme->fg`, falling back to the `ui.text` scope's fg) and bg = the theme's background (`theme->bg` / `ui.background`), then overlaying the scope's own fg/bg where set. Check what style accessors exist in the components API (`style-fg`/`style-bg` setters exist; look at generated docs in `.dev/steel-home/cogs/helix/components.scm` for getters) — if fg/bg can't be read from a Style object, construct explicitly: popup style = `(style-bg (style-fg (style) <fg-colour>) <bg-colour>)` using `theme->fg`/`theme->bg`, and accept that themes with a distinct ui.popup bg lose that nuance for now (document it if so).

Also audit the toast border/background and popup content rows for the same issue (content rows sit on the space-filled background, so they are OK as long as the fill itself sets bg explicitly — verify).

e2e verification: capture with `tmux capture-pane -e` (includes SGR escape codes) and assert the popup's top-border row has uniform styling across its full width (parse the escape sequences in the captured line; a simple check that the border row contains only ONE distinct SGR-prefix before/within the border span is enough). Trigger the error popup deterministically (type `(` as search text, then press enter to trigger the foreground-search error popup).

## 2. Preview: no line numbers

The preview pane must NOT show line numbers (the `(n)` values in the TUI screenshots are result indices in the LIST, not preview line numbers — see item 3). Context lines get a two-space prefix (the TUI's `PREVIEW_LINE_PREFIX = "  "`, view.rs:530) so they align with the `- `/`+ ` diff prefixes. Remove `context_preview_line`'s numbering. Keep the `(<stripped control chars>)` handling.

## 3. Result list rows: right-aligned indices + accessory styling

Mirror `file_path_line` (view.rs ~1600):

- Row content: `[x] ` or `[ ] ` marker, path (end-truncated with leading `…` as now), `:<line>`, spacer fill, right-aligned ` (<idx+1>)` where idx is the absolute result index.
- Unselected rows: marker tag `"info"`, path tag `"text"`, `:<line>` tag `"info"`, index tag `"info"` (the TUI colours these accessories blue). No spacer run needed.
- Selected rows (see item 4 for which tag): the ENTIRE row — marker, path, line, spacer, index — uses the selection tag uniformly, and the spacer must fill the full row width so the background is continuous (the TUI paints the whole line's bg).

## 4. Selection variants and focus gating

Mirror the TUI's selection colours (`file_path_line`): selection styling applies ONLY when the results section is focussed (`focussed_section == SearchResults`); when fields are focussed, all rows render as unselected. Variants:

- primary + included -> `"selection"` (existing)
- range (non-primary) + included -> `"selection-secondary"` (existing)
- primary + excluded -> new tag `"selection-excluded"`
- range + excluded -> new tag `"selection-secondary-excluded"`

The TUI uses blue bg / darker blue / red bg / darker red respectively. Steel mapping (add to the style table in `ui/window.scm`, with the explicit fg/bg discipline from item 1): excluded variants derive from the `error` scope's colour used as background (reversed), with `style-with-dim` for the secondary variant. Keep it theme-driven; no hardcoded colours.

## 5. Window border title

Render ` Scooter ` over the popup window's top border, left-offset by 2 columns, in the popup border style (the old plugin's `draw-title` on the `main` branch is the reference). Steel side, after `block/render`.

## 6. Small alignments

- Results-screen tally box titles (`Successful replacements (lines):` etc.) must be left-aligned in the top border (at x+1), not centred — matching ratatui's default `Block::title` used by the TUI (`render_results_tallies`).
- Check `create_popup_block` in the TUI for popup title alignment and padding, and match ours to it (title alignment, and the TUI uses horizontal padding of 1-2 inside popups — mirror what you find).

## Tests / acceptance

- Update all affected unit/integration tests and e2e assertions (several assert on the old `(n)` preview numbering and row format).
- Add coverage: list row shows right-aligned index; selection uniformity across the full row width when results focussed; no selection tags when fields focussed; excluded+selected rows use the excluded tags; popup border uniformity e2e (item 1).
- `scripts/check.sh` passes; ALL e2e scripts (smoke, live-search, sizes, preview, replace) pass twice consecutively.
- Include a capture of the results view (wide) and the error popup in `docs/specs/F1-REPORT.md`.
- Do not commit. Do not modify `../scooter`.
