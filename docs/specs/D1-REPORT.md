# D1 report: README docs automation

## Implementation

`src/options.rs` now has one declarative `OPTION_SPECS` table. Each entry
owns the `scooter-set!` symbol, Rust wire path, value type, default, concise
documentation, and its parser setter. `EngineOptions` applies that table for
defaults and for every non-key option. The Steel shim now resolves setting
symbols through the Rust table over the FFI, so Scheme no longer carries a
second hand-maintained symbol-to-path list.

The engine exposes only the option documentation surface through
`scooter_hx::docs`; it is built as an `rlib` as well as the plugin `cdylib`.
The new `xtask` depends on that small public surface instead of reparsing
`src/options.rs`. This was the least fragile choice: the runtime parser, the
Steel API, and README generation all use the same compiled declarations.

`cargo xtask readme` is configured through `.cargo/config.toml`. The command
uses `cargo metadata` to find the resolved git checkout of `scooter-core`,
syn-parses `src/config/keys.rs` for `KeysConfig` field documentation, and uses
`KeysConfig::default()` for the corresponding default bindings. It therefore
does not require a sibling scooter repository, a Helix checkout, or `.dev/`.

## Generated README sections

- `<!-- TOC START/END -->`: links for the README's `##` headings.
- `<!-- CONFIG START/END -->`: the `scooter-set!` options table plus a
  `scooter-keys!` usage block.
- `<!-- KEYS START/END -->`: default binding path, key strings, and
  scooter-core doc-comment description table.

All surrounding prose, examples, installation guidance, behavioural notes,
and e2e instructions remain hand-written. The pre-existing preview-background
deviation note was retained.

## CI and validation

`scripts/check.sh` and `.github/workflows/test.yml` now use workspace build,
Clippy, and test commands, so xtask is linted and tested with the engine. CI
also runs `cargo xtask readme --check` after the Rust tests. A deliberately
changed option doc string made `--check` fail; the string was restored and the
regenerated README checks cleanly.

Validation completed:

- `HELIX_RUNTIME=$HOME/Development/helix/runtime scripts/check.sh` passed
  (66 library tests and the SH0 integration test).
- `cargo xtask readme --check` passed on the final tree.
- The documented e2e scripts were run once: smoke, live search, sizes,
  preview, replace, lifecycle, config, and open.

The explicit `HELIX_RUNTIME` on the local check is the existing runtime setup
used by the e2e harness; without it, this machine's symlinked runtime discovery
does not load the Rust grammar in the highlighted-preview snapshot.

No commit was created, and neither `../scooter` nor the Helix checkout was
modified.
