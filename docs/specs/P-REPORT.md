# P report: polish and release readiness

## [confirm] Product-owner decisions

1. The release version has been prepared as `0.2.0` in both `Cargo.toml` and
   `cog.scm`. Confirm that this is the intended first rewrite release version.
2. The existing `media/preview.gif` and `media/preview.png` predate the
   rewrite. They are explicitly marked stale and no longer embedded in the
   README. Should Tom regenerate them, or should the files be removed before
   release?
3. `scooter-core` is pinned to the current `new-plugin-changes` revision
   `9387c36c6a6b8004d091b8f32e107b91ff530b0b`. A prominent Cargo TODO records
   the planned switch to `scooter-core` 0.4 on crates.io. Confirm that the git
   pin is the desired release form until that publication.
4. All chunk specs and reports, including this one, remain in `docs/specs/` on
   the branch. Confirm whether they should be pruned immediately before the
   final PR.

## Delivered

- Rewrote the README around the native plugin architecture: embedded
  `scooter-core`, Helix-native rendering, compatible Helix/Steel requirements,
  Forge and source installation, commands/session semantics, full core-default
  keymap, configuration tables, intentional TUI differences, and local
  validation instructions.
- The default-keymap table was checked against
  `scooter_core::config::KeysConfig::default()` at the pinned revision. It
  matches the Scooter TUI defaults.
- Documented `e` foreground opening, `A-e` background opening, non-dirty
  buffer reload after replacement, Esc hiding semantics, lack of syntax
  highlighting, and the inapplicability of Scooter TOML `editor_open`.
- Bumped the prepared release version to `0.2.0` in Cargo and cog metadata.
  The existing release workflow's version assertion now succeeds.
- Simplified `test.yml` to one stable-toolchain job that runs exactly
  `cargo clippy --all-targets -- -D warnings` and `cargo test`. There is no
  rustfmt configuration, so no formatting check was added. The workflow and
  README both state why the tmux/Helix e2e suite remains local-only.
- Kept `release.yml` structurally unchanged. The README and report flag that
  Linux cross builds should be watched on the first CI release: the
  `scooter-core` `two-face`/Oniguruma C dependency is the relevant risk.
- Replaced the local `../scooter/scooter-core` dependency with the pinned git
  dependency. `Cargo.lock` records that revision, and the manifest has a TODO
  for the future crates.io 0.4 form.
- Completed the cleanup sweep: every direct dependency is used, no
  `test_utils`-era dependency remains, stale README media embedding was
  removed, `.dev/` is ignored, and `docs/specs/` now has a report for every
  retained chunk spec.

## Validation

- `scripts/check.sh` passed twice: build, Clippy with `-D warnings`, and all
  59 tests including snapshots, on Rust 1.89.0.
- The complete local e2e suite passed twice in its isolated environment:
  `e2e-smoke`, `e2e-live-search`, `e2e-sizes`, `e2e-preview`, `e2e-replace`,
  `e2e-lifecycle`, `e2e-config`, and `e2e-open`.
- A temporary clean git clone with the working diff applied built with an
  isolated `CARGO_TARGET_DIR` and no sibling `../scooter` checkout, proving
  the remote core dependency is self-contained.
- Release builds succeeded for both configured macOS targets:
  `aarch64-apple-darwin` (arm64 dylib) and `x86_64-apple-darwin` (x86_64
  dylib), each via `cargo build --release --target` using a temporary Rust
  toolchain and target directory.
- A manual `scripts/try.sh` tmux smoke session covered live search, previews,
  all four field toggles, multiselect, foreground/background opening,
  hide/resume, reset, help, replacement, and quit. A fresh `scripts/try.sh`
  session then reloaded a temporary config override and demonstrated that
  remapping `search.results.move_down` to plain `n` takes effect for a new
  session.

No commit was created and `../scooter` was not modified.
