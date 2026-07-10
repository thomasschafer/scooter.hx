# S1: toolchain spike — dylib skeleton, themed blit, e2e harness

Read `docs/REWRITE-PLAN.md` first for full context. This repo is being rewritten from scratch; you are implementing the first chunk. This chunk proves the toolchain end to end: a fresh Steel dylib that loads in the new Helix branch, renders static themed content in a popup, responds to keys, and an e2e harness that verifies this in a real `hx` session under tmux.

## Environment facts (do not rediscover these; they are verified)

- Helix checkout: `~/Development/helix`, branch `steel-event-system`. Test binary: `~/Development/helix/target/release/hx` (already built with `--features steel,git`; do NOT rebuild it or run `cargo install` in that repo).
- Helix pins steel-core via git. The plugin MUST use the same rev, from helix's Cargo.lock: `steel-core 0.8.2, git rev dec633b908afeafeaf62bab457a92e2bf873745a` (repo `https://github.com/mattwparas/steel.git`). Pin `steel-core` (features: `dylibs`, `sync`) and `steel-derive` to this rev. Dylib loading is version-checked by abi_stable; a mismatched rev fails to load at runtime with a library error.
- `cargo-steel-lib` is installed and respects `STEEL_HOME`: `STEEL_HOME=<dir> cargo steel-lib` builds the cdylib and copies it to `<dir>/native/`.
- Helix's Steel modules (helix/components.scm etc.) have been pre-generated into this repo's scratch steel home: `.dev/steel-home/cogs/helix/`. If missing, fail with a clear message telling the operator to run `STEEL_HOME=$PWD/.dev/steel-home cargo xtask code-gen` in the helix checkout (do not run it yourself).
- The old plugin implementation (see git history / `main` branch) targeted steel-core 0.7.0 and an old scooter-core; it is being deleted, not migrated. Use it only as reference for FFI registration patterns and the Steel component API. Reference files from the old implementation you may find useful: `scooter.scm`, `ui/window.scm` (component lifecycle, `push-component!`, `new-component!`, event handler shape, `theme-scope` usage via `ui/styles.scm`).
- Never touch `~/.steel`, `~/.config/helix`, or anything outside this repo and `/tmp`. All hx invocations must use the isolated env (see harness section).

## Deliverables

1. Delete the old implementation: `src/*` (all Rust), `ui/*.scm`, and replace `Cargo.toml` deps. Keep `scooter.scm` as the entry point file (rewritten), keep `cog.scm`, README, workflows, media untouched for now.

2. New `Cargo.toml`:
   - `crate-type = ["cdylib"]`, lib name `scooter_hx` (the dylib must be named `libscooter_hx` — release workflow and cog.scm depend on it).
   - Deps: `steel-core` + `steel-derive` pinned to the git rev above, `abi_stable`. Nothing else yet (no scooter-core in this chunk).
   - Keep the existing clippy pedantic lint table from git history.

3. `src/lib.rs` (+ modules as you see fit): a minimal FFI module named `steel/scooter` registering:
   - `Scooter-engine-new` -> opaque engine handle (a `Custom` struct; for now it just holds a demo counter or nothing).
   - `Scooter-render (engine width height)` -> the frame model: a list of runs, each run a list `(x y text style-tag)` with x/y as usize (0-based, relative to the drawn area), text a string, style-tag a string from this fixed set: `"text"`, `"dim"`, `"selection"`, `"active"`, `"error"`, `"info"`, `"diff-added"`, `"diff-removed"`. Render a static demo frame that exercises every tag (e.g. a title line, a fake field box border, a fake selected result line, a fake diff pair) and fills the full width/height edges so clipping bugs are visible.
   - `Scooter-handle-key (engine code-string modifiers-int)` -> returns a status string: `"consumed"` for most keys, `"hide"` when code-string is `"esc"`, `"quit"` when it's char `c` with the ctrl modifier bit set. Key encoding contract (this will grow in E2, keep it in one module): code-string is either the literal char (single character string), or one of `"esc" "enter" "tab" "backspace" "left" "right" "up" "down" "home" "end" "pageup" "pagedown" "delete"`; modifiers-int is a bitflag int matching Helix conventions: shift=1, ctrl=2, alt=4.
   - Wrap every FFI entry point so a Rust panic cannot cross the FFI boundary (catch_unwind; on panic, log and return a safe value). This pattern is permanent — build it properly.

4. Rewritten `scooter.scm` + a new `ui/` scheme file if needed:
   - Provides `scooter` (typed command). Invoking `:scooter` pushes a component drawing a centered window (90% x 90% of the screen) with a `block/render` border, and inside it blits the runs from `Scooter-render`, mapping style tags to theme styles: text->`ui.text`, dim->`ui.text.inactive`, selection->`ui.selection`, active->`ui.text.focus` (fall back to `hint` if that scope errors), error->`error`, info->`info`, diff-added->`diff.plus`, diff-removed->`diff.minus`. Build the tag->style table once per render call.
   - Event handler decodes Helix key events to the (code-string, modifiers-int) contract and calls `Scooter-handle-key`; on `"hide"` or `"quit"` result, close the component (`event-result/close`), otherwise consume.
   - Use the old `ui/window.scm` from git history as reference for the component API (`new-component!`, `push-component!`, `key-event-char`, `key-event-modifier`, area/frame functions), but write fresh minimal code.

5. e2e harness `scripts/e2e-env.sh` (shared env setup, sourced by other scripts) and `scripts/e2e-smoke.sh`:
   - Env setup: `REPO=$(git rev-parse --show-toplevel)`; `STEEL_HOME=$REPO/.dev/steel-home`; scratch config `$REPO/.dev/config/helix/init.scm` that requires the repo's `scooter.scm` by absolute path; `XDG_CONFIG_HOME=$REPO/.dev/config`; `XDG_CACHE_HOME=$REPO/.dev/cache`; `HELIX_RUNTIME=$HOME/Development/helix/runtime`; fixture workspace dir `$REPO/.dev/fixtures/basic` containing a few small text files (create deterministically in the script).
   - Smoke test flow: build+install dylib (`STEEL_HOME=... cargo steel-lib`), start tmux session (unique socket name, 120x40) running `hx` with the env above in the fixture dir, wait for startup, send `:scooter<enter>`, capture pane (`tmux capture-pane -p`), assert expected demo strings are present, send `esc`, capture, assert the popup content is gone, kill the session (always, on trap). Exit non-zero with the captured pane printed on any failure. Poll-with-timeout for assertions rather than fixed sleeps (helix startup time varies); keep total runtime under ~30s.
   - The harness must be reliable when run twice in a row (clean up tmux sessions and stale state).

6. `.gitignore`: add `.dev/`.

## Acceptance criteria

- `cargo build` and `cargo clippy --all-targets -- -D warnings` pass.
- `scripts/e2e-smoke.sh` passes, run from a clean checkout state (given `.dev/steel-home/cogs/helix` exists and the hx binary exists).
- Run the smoke test twice consecutively; both pass.
- No writes outside the repo and `/tmp`. No global installs. Do not commit anything; leave all changes in the working tree.
- Write a short summary of what you did and any deviations from this spec into `docs/specs/S1-REPORT.md` (this file is read by the reviewer; include exact commands to reproduce your validation).

## Notes and gotchas

- The dylib name lookup happens via `#%require-dylib "libscooter_hx" ...` in scheme.
- If the dylib fails to load with an abi_stable version error, your steel-core rev doesn't match the helix binary — fix the pin rather than working around it.
- `theme-scope` may error on unknown scopes depending on theme; guard with a fallback style rather than crashing the render callback.
- tmux: use `tmux -L <socket> new-session -d -x 120 -y 40 'env ... hx .'` style invocation; `send-keys` needs a short delay after startup; prefer polling `capture-pane` output for a marker string.
- hx logs go to `$XDG_CACHE_HOME/helix/helix.log` with the env above — cat it on failure to aid debugging.
