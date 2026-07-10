# A1 report: colour architecture audit + code review fixes

Implemented without commits and without modifying `../scooter`.

## Per-item outcomes

1. Added one resolved `surface-bg` in `ui/window.scm`: `ui.popup`'s background
   when set, otherwise `ui.background`.  Popup fill, popup border, toast
   border, the main window block, and its ` Scooter ` title now agree on this
   surface.
2. Added an exhaustive style-invariant table.  Overlay styles are explicitly
   coloured; normal content styles are foreground-only patches and inherit the
   fill below them.  The selected-row styles remain documented intentional
   full-row fills so blue/red selection backgrounds continue to work.
3. Checked the Helix Steel bindings: `theme->fg` and `theme->bg` return
   `Style`, while `style->fg`/`style->bg` return `Color | #false`.  Replaced the
   deprecated helper chain with guarded `theme-scope` access returning only a
   `Style`, then resolve colours separately.
4. Kept `selection-excluded` as theme text foreground on the error foreground
   fill.  It was readable in Catppuccin Mocha and the default-theme ANSI
   captures (see visual pass below), so no contrast override was needed.
5. Hardened `e2e_assert_popup_interior_matches_border_background` to parse SGR
   background state for a popup corner and the first interior cell.  The error
   popup now runs both the original uniform-border assertion and this interior
   assertion.  The isolated e2e environment accepts `E2E_THEME`, enabling the
   same test under Catppuccin Mocha and `default`.
6. Replaced both dylib `eprintln!` calls with `log::warn!`; added the old
   plugin's cache-directory/simple-log pattern as `src/logging.rs`.  It installs
   a once-only warning-level rotating file logger and never writes to stderr.
7. Made Steel unknown style tags safe: `blit-run!` falls back to `text` and
   records one `log::warn!` per distinct unknown tag.
8. Replaced `Run.tag: String` and renderer tag literals with the closed
   `StyleTag` enum.  `as_str()` is the only string conversion, at the unchanged
   FFI boundary.  No renderer snapshot changed for this item.
9. Split the view into focused `view/canvas.rs`,
   `view/layout.rs`, and `view/banner.rs`; frame primitives, semantic tags,
   geometry, and banner/footer rendering are no longer in the monolith.  The
   E7 renderer snapshots were byte-identical across this code motion.
10. Added `active_runtime() -> Option<&Runtime>` and removed all active-runtime
    `expect` calls.  After `quit`, handle-key/pump/render/cursor/busy return
    safe rerender/idle/empty/false defaults.  `ffi_calls_after_quit_are_safe_noops`
    covers the exported operations.
11. Removed the plugin continuation marker from wrapped preview lines.  The
    wrapping snapshot now shows text continuing at column zero of the preview;
    the old local TUI source was inspected as requested, but its current marker
    differs from this A1 acceptance requirement, so the explicit spec outcome
    was applied.
12. Renamed the side-effecting Steel consumer to
    `consume-scooter-response!` and updated `scooter.scm` and window call sites.
13. Confirmed all executable e2e scripts install cleanup traps and every
    committed snapshot contains a `tags:` legend.  Kept the tiny-width narrow
    box path because it is reachable; added a direct 1×4 renderer test.

## Extra fixes

- Stabilised an existing E7 flake: popup layering can hide the middle of a
  dynamic `[Time taken: …]` run, preventing insta's whole-string filter from
  matching it.  The snapshot painter normalises that duration before painting
  while runtime rendering remains unchanged.
- Removed the now-unreachable `dim` tag/style after wrap-marker removal.

## Snapshot changes

- `help_popup_results_focus`: `…┐2s]` became `…┐0s]`, solely from the snapshot
  duration normalisation above.
- `preview_wrapping_on`: removed the two-cell `↪ ` prefix from each continuation
  and let the underlying diff tag extend over those cells.  This is the intended
  item-11 parity change.
- Item 9 produced no snapshot change beyond the earlier, separately justified
  duration normalisation and wrap-parity snapshot.

## Popup capture evidence

The final visual capture matrix is under `.dev/a1-visual/` (ignored test
artifacts).  In Catppuccin Mocha, the error popup's resolved surface appears as
`48;2;49;50;68`; the editor background seen outside the overlay is
`48;2;30;30;46`.  In the default-theme capture the corresponding values are
`48;2;40;23;51` and `48;2;59;34;76`.  The new parser assertion passed in both
themes, confirming the popup border and immediate interior cell use the former
surface value, not the editor background.

## Validation summary

- After each ordered item: `scripts/check.sh` plus the relevant e2e scenario
  passed.  Relevant scenarios included replace (overlays/colours), preview
  (results/wrap), lifecycle (teardown), smoke (Steel naming), and sizes (tiny
  layout).
- Final full suite pass 1: `scripts/check.sh` passed (47 tests), followed by
  `e2e-lifecycle.sh`, `e2e-live-search.sh`, `e2e-preview.sh`,
  `e2e-replace.sh`, `e2e-sizes.sh`, and `e2e-smoke.sh`; all passed.
- Final full suite pass 2: the identical seven-command run passed again (47
  tests and all six e2e scripts).
- Fresh-eyes visual pass: plain and `capture-pane -e` captures of fields,
  results+preview, error popup, help popup, toast, and replacement results were
  reviewed at 120×40 and 220×55 under Catppuccin Mocha and `default`.  No
  remaining overlay-surface inconsistency was found.
