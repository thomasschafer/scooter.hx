# PAR1 report

## Delivered

| Item | Evidence |
| --- | --- |
| Configurable background open | `plugin.open_in_editor_bg` defaults to `A-o`; `scooter-keys!` accepts it, rejects multi-key values, and validates it against every general/search core binding. `A-e` now reaches core in fields and results focus; `A-o` dispatches a background open only from results; rebinding is covered. |
| Paste | `paste-event?` now calls `Scooter-paste`; CR/LF become spaces and text is routed through core's normal character path, preserving validation and debounce. Checkboxes, popups, and non-field focus are no-ops. |
| Modifiers | The wire mask preserves Super (8) and Meta (32); Rust decode maps both to scooter-core modifiers, with decode coverage. |
| Help | Results help injects the actual plugin chord immediately after `open in editor`; the fields help and compact footer remain unchanged. The result-help snapshot changed solely to add the new row and its extra popup height. |
| Interception contract | The engine test walks every `KeysConfig::default()` binding and records it reaching core. The only engine-side special cases are documented Esc semantics and the configured background chord in results focus. |
| Matrix | Added `scripts/e2e-matrix.sh`: multiline + advanced lookahead, rebound movement, wrapping, syntax-highlighting-off coverage, and `A-e`; it passed twice consecutively. |

`scripts/e2e-parity.sh` covers bracketed tmux paste, `A-e` in both focus states, a rebound background chord, and a plugin/core collision. `scripts/e2e-open.sh` now uses `A-o`.

## Sweep confirmations

| Behaviour | Result |
| --- | --- |
| `A-m`, `C-t`, `C-l`, `A-u` | Reaches core in both reachable search-focus paths. |
| `A-;`, `v`, multiselect Esc ordering | Reaches core; Esc exits multiselect before returning to fields. |
| `g`/`G`, paging, `C-n`/`C-p` | Canonicalized/forwarded to core. |
| Incomplete-search Enter, `C-h`, `C-c`, `C-r` | Core popup/quit/reset precedence retained. |
| Results error scrolling and quit | Core results bindings forwarded. |
| Checkbox Space, fixed-strings error clearing, Tab/Shift-Tab wrap | Core field path retained. |

## Validation

- `HELIX_RUNTIME=~/Development/helix/runtime scripts/check.sh`: passed repeatedly (77 library tests plus integration test).
- `cargo xtask readme --check`: passed before final suite runs.
- PAR1 e2e (`e2e-parity.sh`, `e2e-open.sh`) passed; `e2e-matrix.sh` passed twice consecutively.
- Existing e2e scenarios were exercised. A broad batch invocation briefly exposed an intermittent `e2e-config.sh` conflict-window timing failure; its immediate isolated retry passed. No product failure was reproduced.

## Upstream note

`CommandSearchFocusResults::OpenInEditor` dereferences `primary_selected_field_mut().expect(...)` with zero results. Proposed one-line upstream patch: replace the `expect` path with `let Some(result) = ... else { return EventHandlingResult::None; };` before constructing the launch event. The plugin FFI guard already contains any resulting panic.
