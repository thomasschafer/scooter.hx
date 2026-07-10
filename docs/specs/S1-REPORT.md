# S1 implementation report

## Delivered

- Replaced the legacy engine and Scheme UI with the `steel/scooter` S1 spike.
- Pinned `steel-core` and `steel-derive` to Helix's `dec633b908afeafeaf62bab457a92e2bf873745a` revision.
- Added a panic-contained opaque engine, typed frame runs, all eight semantic style tags, and the specified key-status bridge.
- Added a centered, themed Steel popup and the isolated tmux smoke harness in `scripts/`.
- Marked S1 complete in the rewrite plan. No commit was created.

## Validation

Run from the repository root:

```sh
cargo build
cargo test
RUSTC="$HOME/.rustup/toolchains/1.89.0-aarch64-apple-darwin/bin/rustc" \
  CARGO_TARGET_DIR=/tmp/scooter-hx-s1-clippy-1.89-clean \
  rustup run 1.89.0 cargo clippy --quiet --all-targets -- -D warnings
bash -n scripts/e2e-env.sh scripts/e2e-smoke.sh
git diff --check
scripts/e2e-smoke.sh
scripts/e2e-smoke.sh
```

All commands passed. Each smoke run built and installed the dylib in the scratch `STEEL_HOME`, launched isolated Helix in tmux, asserted the static popup content, then closed it with `esc` and asserted that the content disappeared.

## Notes

There are no functional deviations from S1. The harness opens `alpha.txt` from the fixture workspace rather than `hx .`, because the latter opens Helix's directory picker and prevents the command keystrokes from reaching normal mode.

The explicit 1.89 Clippy command is required only in this shell: its default `rustc` is 1.94 while `cargo-clippy` on `PATH` is 1.89. The pinned toolchain and temporary target make the validation reproducible without modifying global tooling.
