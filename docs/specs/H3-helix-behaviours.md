# H3: Helix behaviours — open in editor, buffer reload

Read `docs/REWRITE-PLAN.md` for context. Everything through E7/C1 is committed. H3 wires the engine's action queue into Helix: opening results in the editor and reloading buffers after replacements. References: the old plugin's `ui/window.scm` on the `main` branch (`open-selected-search-result`, `reload-non-dirty-documents`) for the Helix API usage (`helix.open`, `helix.goto`, `align_view_center`, `editor-all-documents`, `editor-document-dirty?`, `editor-document-reload`) — verify these still exist in the generated cogs under `.dev/steel-home/cogs/helix/`.

## Deliverables

1. Foreground open (`e`, or whatever the user bound `open_in_editor` to): the engine already emits `("open-file" path line)` actions. Steel consumes them: hide the window (session kept, exactly like esc-hide), then `helix.open` the path, `helix.goto` the line, and centre the view. The user returns with `:scooter` and everything is as they left it.

2. Background open (plugin extra, `A-e` hardcoded... NO — respect remapping): when the results section is focussed and the engine receives alt + the FIRST configured `open_in_editor` binding's base key (e.g. `A-e` for the default `e`; compute alt+<char> only when the binding is a plain char key, otherwise skip the feature for that config), the engine forwards the CONFIGURED binding to App (so App emits its normal LaunchEditor) and tags the resulting drained action as `("open-file-bg" path line)`. Steel handles it like open-file but WITHOUT hiding the window: open + goto + centre happen behind the popup. Add a note to the README config section documenting this.

3. Buffer reload after replacement: when the engine processes `ReplacementCompleted` (the transition to `Screen::Results`), queue a `("reload-docs")` action. Steel consumes it by reloading every non-dirty document (old plugin's `reload-non-dirty-documents` pattern). This must also work when the replacement finishes while the window is hidden (action delivered on resume pump — E6 guarantees queue persistence).

4. `editor_open` config from scooter's TOML is intentionally not supported (Helix always opens in itself); no code needed, just a README note alongside item 2's.

## Tests

- Rust: `open-file-bg` tagging (alt+configured-key with results focussed -> action tagged bg; fields focussed -> alt-e types nothing/ignored as today), `reload-docs` action on replacement completion (also when pumped later).
- e2e `scripts/e2e-open.sh`:
  - Foreground: search, focus results, press `e`; assert the scooter window closed and hx now shows the result's file and line (statusline shows the filename; assert the line content is visible); `:scooter` resumes with results intact.
  - Background: `A-e` on a different result; assert the window is still open; then esc-hide and assert the background-opened file is the active buffer.
  - Reload: run a replacement over a file that is OPEN in hx (open it first, keep it non-dirty), complete the replacement, quit the results screen, and assert the visible buffer shows the replaced content without any manual `:reload`.
- All existing e2e scripts still pass.

## Acceptance criteria

- `scripts/check.sh` + all e2e scripts (including the new open one) pass twice consecutively.
- Write `docs/specs/H3-REPORT.md`. Do not commit. Do not modify `../scooter`.

## Notes

- `helix.goto` in the old plugin took a string line number (`int->string`) — verify the current signature in the cogs.
- Steel consuming actions runs inside component event/poll callbacks; opening documents from there worked in the old plugin via `enqueue-thread-local-callback` — check whether direct calls work in the current event system or whether the enqueue indirection is still needed (old `scooter.scm` used it for destroy-session).
- The window-hide on foreground open must go through the same path as esc-hide so the poll-loop visibility flag stays consistent.
