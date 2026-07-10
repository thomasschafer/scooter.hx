# C1: Steel config surface + full key decode

Read `docs/REWRITE-PLAN.md` for context (this chunk merges the plan's E1 + H2 + the remainder of E2). The plugin is configured from Steel (init.scm), NOT from scooter's config.toml. Everything funnels into core `Config`/`AppRunConfig` at engine creation, reusing core's key parsing (`KeyEvent::FromStr`, same "C-o"/"A-m"/"S-tab" syntax as scooter's docs) and conflict detection.

## Rust side

1. `Scooter-engine-new (directory options)` — second parameter: a list of `(key value)` pairs (strings/bools/floats/lists of strings). Supported keys:
   - `"keys.<section>.<binding>"` -> list of key strings, covering every binding in core's `KeysConfig` (general/search/search.fields/search.results/results). Parse each with `KeyEvent::FromStr`; construct `KeysConfig` programmatically (all fields pub; `Keys::new`). Unknown key paths or unparseable key strings -> error (see 3).
   - `"search.multiline"`, `"search.hidden"`, `"search.advanced-regex"`, `"search.include-git-folders"`, `"search.escape-sequences"` -> bools into `AppRunConfig` (multiline, include_hidden, advanced_regex, include_git_folders, interpret_escape_sequences).
   - `"preview.wrap-text"` -> bool into `Config.preview.wrap_text`.
   - `"window.size"` -> float 0.5-1.0, plugin-level (returned to Steel via a new `Scooter-window-size (engine)` getter used by the window sizing code; default 0.9).
2. Defaults: unset keys keep core defaults (scooter's default keymap etc.).
3. Errors: invalid option keys/values or key-binding conflicts (`App::new` returns Err on conflicts) must make `Scooter-engine-new` return an error the Steel side can distinguish (e.g. return a string instead of the engine handle, or a tagged list — pick something clean given FFI constraints) with the full human-readable message from core's conflict reporting.
4. Complete the key decode table (the E2 remainder): support every `KeyCode` the Steel components API can report — check the generated `components.scm` docs in `.dev/steel-home/cogs/helix/` for the full set of `key-event-*?` predicates (F-keys via `key-event-F?`/similar, insert, etc.) and extend both `event-code` (Steel) and `key::decode` (Rust) accordingly. Unmappable keys stay ignored.

## Steel side

5. Public config API in `scooter.scm` (provided, documented with ;;@doc):
   - `(scooter-set! 'multiline #t)` — accepted symbols: multiline, hidden, advanced-regex, include-git-folders, escape-sequences, wrap-text, window-size.
   - `(scooter-keys! "search.results.move_down" '("j" "down"))` — value may be a single string or list.
   - Settings accumulate in module-level state and apply at engine creation (document: takes effect for new sessions, i.e. next `:scooter-new` or first `:scooter`).
6. On engine-new error: surface the message to the user via helix (check what's available — the old plugin era used `helix.misc` echo-style functions; find the current equivalent in the generated cogs) and do not open the window.
7. Window sizing uses the configured ratio.

## Tests

- Rust: option parsing (each key, invalid values, conflict error propagation), full key decode table.
- e2e `scripts/e2e-config.sh`: writes a scratch init.scm variant that sets `(scooter-set! 'multiline #t)` and rebinds move_down to `"n"`; assert: the multiline toast/behaviour is on by default (a `\n`-spanning pattern matches), `n` navigates results down, `j` no longer does; then a second config with a deliberate key conflict asserts the error message appears and the window does not open. Restore the standard harness config afterwards (the other e2e scripts must still pass).

## Acceptance criteria

- `scripts/check.sh` + all e2e scripts (including the new config one) pass twice consecutively.
- README gets a new drafted section documenting the config API (keep the rest of README untouched; final README rewrite happens in P).
- Write `docs/specs/C1-REPORT.md` with deviations and upstream needs. Do not commit. Do not modify `../scooter`.

## Rider

8. Restore the preview wrap continuation prefix removed in A1 item 11: the A1 spec was wrong — the TUI DOES prefix wrapped continuation lines, with `"  ↪ "` styled dim (`WRAPPED_LINE_PREFIX`, TUI view.rs:531). Match it exactly: same string, dim styling (reintroduce the dim tag mapped to `ui.text.inactive` as a content/fg-patch tag per the window.scm invariant table). Update the wrapping snapshot and note the change.
