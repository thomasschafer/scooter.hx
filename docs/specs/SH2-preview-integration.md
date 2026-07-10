# SH2: syntax highlighting in the preview + editor background

Read `docs/REWRITE-PLAN.md` (phase 2), `docs/specs/SH1-REPORT.md` (the engine you are integrating), and the styling invariant table in `ui/window.scm`. SH2 makes highlighting visible in the preview pane and applies a deliberate, product-requested deviation from the TUI: the preview pane's background becomes the EDITOR background rather than the popup surface, so code reads the way it does in the editor.

## Deliverables

1. Highlighted context lines: preview context lines (the lines around the diff, currently plain `text`) get syntax colouring from the SH1 engine (`s:<scope>` segments). Diff lines (`- `/`+ `) keep their diff styling untouched (TUI parity). The `  ` context prefix and diff prefixes stay; span offsets must account for them.
   - The engine needs full-file content for correct parsing. Refactor the preview read path: for files within the engine's size cap, read the full content ONCE per render (the engine's LRU already dedupes the expensive highlight; make sure the read itself isn't repeated wastefully — reuse the content for both window extraction and highlighting). Oversized files keep the current windowed read and render plain.
   - Multiline (ByteRange) previews: context lines highlighted the same way; the old/new diff blocks stay diff-styled.
   - Wrapping must carry scope segments through (the wrapper already handles styled segments).
2. Config: new option `'syntax-highlighting` (default on) in the options table (`scooter-set! 'syntax-highlighting #f` disables, rendering plain). Mirrors the TUI's `preview.syntax_highlighting` config.
3. Preview pane background = editor background:
   - New fill: the preview pane's rectangle is filled each render with a tag whose style is EXPLICIT fg+bg where bg = the resolved editor background (`ui.background`/`theme->bg` — NOT the popup surface). Add the tag to the invariant table as an overlay-class style. Content on top (context, highlighted spans, diff lines) stays fg-patch, so it inherits this editor background.
   - Scope: the preview pane only. The results list and everything else stay on the popup surface.
   - Document the deviation in the README's "differences from the TUI" section (one line).
4. Snapshots: add snapshot coverage with a `.rs` fixture file showing `s:` scope tags in the legend (both narrow and wide layouts, and one with wrapping). Existing snapshots use `.txt` fixtures (no language), so they should remain byte-identical apart from the new preview background fill runs — review and justify every changed snapshot in the report.
5. Themed e2e (extend `e2e-preview.sh` or a sibling): under catppuccin_mocha via `capture-pane -e`:
   - a context line in a Rust file's preview contains a foreground SGR different from the plain text foreground (proves highlighting is active);
   - the preview pane's background SGR equals the editor background and differs from the popup surface background (proves item 3);
   - with `'syntax-highlighting #f` in a scratch config, the same context line has NO extra foreground SGR (proves the toggle).
6. Perf sanity: a test asserting that rendering the preview twice for the same selected result performs at most one engine highlight (cache hit — observable via the spans Arc identity or a counter), and an e2e-level smoke that navigating j/k rapidly through results in a large Rust file stays responsive (no strict timing assert; just ensure no timeout at the usual waits).

## Acceptance criteria

- `HELIX_RUNTIME=~/Development/helix/runtime scripts/check.sh` passes; ALL e2e scripts pass twice consecutively (including your extended preview assertions).
- Include in `docs/specs/SH2-REPORT.md`: a 160x45 plain capture of a highlighted Rust preview, the SGR evidence for background and highlighting, snapshot-diff justifications, and any engine API adjustments you needed.
- Do not commit. Do not modify `../scooter` or the helix checkout.
