# D1: README docs automation (scooter-style xtask)

Read `docs/REWRITE-PLAN.md` (phase 2). The scooter TUI generates its README config/keys documentation with an `xtask readme` command (see `../scooter/xtask/src/build_readme.rs`, read-only): syn-parses config structs' doc comments, rewrites marker-delimited README sections, and offers `--check` for CI. D1 brings the same discipline here, with a refactor making a single source of truth for plugin options.

## Deliverables

1. Options-table refactor: consolidate the C1 option parsing (`src/options.rs`) into ONE declarative table — each entry: option key/symbol, wire path (e.g. `search.multiline`), type, default, and a doc string. The parser, the `scooter-set!` symbol list, and the docs generator all derive from this table. Ensure every option added since C1 (`runtime-dir`, `syntax-highlighting`, `window.size`, ...) is present with an accurate doc string.

2. `xtask/` crate (workspace member, mirroring scooter's structure) with a `readme` command:
   - `--check` mode exits non-zero when the README is stale (for CI).
   - Regenerates marker-delimited sections in `README.md`:
     - `<!-- TOC START/END -->`: table of contents from `##` headings (reuse scooter's logic shape).
     - `<!-- CONFIG START/END -->`: options table generated from the item-1 table — for each option: the `scooter-set!` symbol, type, default, doc. Include a generated `scooter-keys!` usage block.
     - `<!-- KEYS START/END -->`: the default keymap table generated from scooter-core's `KeysConfig` doc comments + defaults. Locate the scooter-core source via `cargo metadata` (works for the git dependency checkout under `~/.cargo/git/`), then syn-parse `src/config/keys.rs` like scooter's own generator does. Render: binding path, default key(s), description.
   - How the generator obtains the item-1 table: prefer having the main crate expose it (e.g. a small `pub` docs module or a feature-gated export) over xtask re-parsing our source with syn — pick the least fragile mechanism and justify it in the report.
3. README: insert the markers, replace the hand-written config/keys sections with generated content, and regenerate. The hand-written prose around the markers stays. Also add the one-line preview-background deviation note if SH2 has not already (check).
4. CI: add an `xtask readme --check` step to `test.yml` (same job is fine). It must work on CI: no helix checkout, no `.dev/` — the generator can only depend on the repo + cargo deps.
5. A `scripts/` or README-documented invocation (`cargo xtask readme`) with the cargo alias configured (`.cargo/config.toml` alias like scooter's, if that is how scooter wires it — check and mirror).

## Acceptance criteria

- `cargo xtask readme --check` passes on a clean tree; deliberately editing a doc string then running `--check` fails until regenerated.
- `scripts/check.sh` passes (xtask included in workspace lints/tests as appropriate); all e2e scripts pass once each (no runtime behaviour change expected).
- README renders correctly (eyeball the generated markdown tables).
- Report `docs/specs/D1-REPORT.md`: mechanism choices, exactly which README sections are generated vs hand-written, CI wiring.
- Do not commit. Do not modify `../scooter` or the helix checkout.
