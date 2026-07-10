# P: polish and release readiness

Read `docs/REWRITE-PLAN.md` for context. All feature chunks (through H3) are committed. P makes the branch PR-ready. Several items need the product owner's sign-off — they are marked [confirm]; implement the rest, and for [confirm] items prepare the change but list the open question prominently in the report.

## Deliverables

1. README rewrite:
   - What the plugin is (mirror of the scooter TUI, embedded scooter-core engine, native Helix rendering).
   - Requirements: Helix built from the `steel-event-system` branch, steel toolchain (`cargo xtask steel`), the pinned steel-core rev caveat.
   - Installation: forge install and build-from-source (`cargo steel-lib`), init.scm require line.
   - Usage: `:scooter` / `:scooter-new`, session semantics (hide/resume, quit), default keymap table (generate from core's `KeysConfig::default()` — same defaults as the scooter TUI), open-in-editor `e` / background `A-e`, buffer reload behaviour.
   - Configuration: polish the C1-drafted section (`scooter-set!` options table, `scooter-keys!` with scooter's key syntax, conflict errors, when settings apply).
   - Note what intentionally differs from the TUI (no syntax highlighting yet -> link future work; `editor_open` config not applicable; esc hides the window).
   - Media: mark the existing gif/png as stale [confirm: Tom regenerates or we drop them from the README until then].
2. `cog.scm`: bump version (suggest `0.2.0` [confirm]) and keep it matching Cargo.toml (release workflow asserts this).
3. CI `test.yml`: run fmt-check if rustfmt config exists (it does not — skip), `cargo clippy --all-targets -- -D warnings`, `cargo test` (includes snapshots) on a single stable toolchain. The tmux e2e suite is local-only: add a brief comment in the workflow and a README-adjacent note (developer docs section in README or CONTRIBUTING snippet) explaining `scripts/check.sh` and the e2e harness.
4. `release.yml`: unchanged in structure; verify locally that `cargo build --release` succeeds for the host targets available (aarch64/x86_64 macOS via `--target`), and flag in the report that the Linux cross builds (onig C dependency via scooter-core's two-face) need watching on the first CI run.
5. Dependencies: swap the `scooter-core` path dependency for a git dependency pinned to the current `new-plugin-changes` rev of `https://github.com/thomasschafer/scooter` [confirm: final form may be a crates.io version once scooter-core 0.4 is published — leave a clearly marked TODO comment either way]. Ensure `cargo build` works from a clean clone without `../scooter` present (use a scratch `CARGO_TARGET_DIR` and a temporary checkout elsewhere to prove it).
6. Cleanup sweep: remove anything dead (`media/` references decided by item 1, stale comments, unused deps in Cargo.toml — check `test_utils`-era leftovers), confirm `.gitignore` covers `.dev/` artifacts, and verify `docs/specs/` contents are consistent (specs+reports stay on the branch; pruning before PR is [confirm]).
7. Final validation: `scripts/check.sh` and the full e2e suite twice; plus one manual `scripts/try.sh` session smoke-tested by you covering: live search, preview, toggles, multiselect, replace flow, open-in-editor fg/bg, hide/resume, reset, quit, help popup, config override.

## Acceptance criteria

- Everything above green; README accurate against the actual behaviour (verify claims against the code/e2e, not memory).
- `docs/specs/P-REPORT.md` with the [confirm] questions collected at the top for the product owner.
- Do not commit. Do not modify `../scooter`.
