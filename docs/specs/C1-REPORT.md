# C1 report: Steel config surface + full key decode

## Delivered

- Added `(Scooter-engine-new directory options)`. Its narrow pair-list wire
  format accepts the specified booleans, `window.size` float, and key-string
  lists; unset values retain scooter-core defaults.
- Added all documented `keys.*` paths, using core `KeyEvent::FromStr` and
  `Keys::new`. Engine creation now reports core key conflicts as a normal
  error string for Steel to display instead of throwing over FFI.
- Added `Scooter-window-size`, with a default ratio of `0.9`; the window uses
  it for both rendering and cursor geometry.
- Added documented public Steel APIs: `scooter-set!` and `scooter-keys!`.
  They retain module-level settings and apply only to a newly created session.
  Creation errors call Helix's `set-error!` and do not open a window.
- Completed key decoding for F1–F24 and every named `KeyCode` exposed by the
  current generated Helix components API. Keys with no exposed predicate
  remain ignored.
- Added `scripts/e2e-config.sh`, covering configured multiline matching,
  rebinding `move_down` to `n` while removing `j`, conflict reporting, and
  restoration of the standard scratch init file.
- Drafted the README configuration section.

## Rider

Restored the TUI continuation prefix exactly as `"  ↪ "`, with the new `dim`
semantic tag mapped to `ui.text.inactive` as a foreground-only content style.
The wrapper follows the TUI's small-width behaviour (no wrapped preview output
when the prefix cannot fit), and `preview_wrapping_on` was updated to show the
prefix and its dim tag.

## Deviations and upstream needs

None. The existing scooter-core path dependency already provided public
configuration fields, `Keys::new`, key parsing, conflict reporting, and the
non-blocking event receive API required for this chunk. No files in
`../scooter` were modified.

## Validation

- Focused checks: `scripts/check.sh` passed with 57 tests; the config e2e
  passed independently while the surface was being developed.
- Full pass 1: `scripts/check.sh`, then `e2e-config.sh`, `e2e-lifecycle.sh`,
  `e2e-live-search.sh`, `e2e-preview.sh`, `e2e-replace.sh`, `e2e-sizes.sh`,
  and `e2e-smoke.sh` all passed.
- Full pass 2: the identical command set passed again (57 tests and all seven
  e2e scripts).

No commit was created.
