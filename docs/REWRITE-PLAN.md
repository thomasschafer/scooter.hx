# scooter.hx rewrite plan

Source of truth for the greenfield rewrite of this plugin. Kept up to date as work progresses; the status table below tracks each chunk.

## Goal

Rewrite scooter.hx so that it mirrors the behaviour of the current scooter TUI (`../scooter`), as a native Helix plugin. The previous plugin was built against scooter-core 0.1.6, before the application state machine moved into the core crate; this rewrite embeds the modern `scooter_core::app::App` directly, so search/replace behaviour is identical to the TUI by construction. Rendering is native to Helix: layout mirrors the TUI's structure, but all styling comes from the user's Helix theme.

## Architecture

Three layers, from bottom to top:

1. `scooter_core::app::App` (unmodified, from `../scooter` via path dependency): the full state machine — debounced live search, search phases, replacement previews, multiselect, configurable keymaps, popups, toasts, replacement execution and results.
2. Rust view + engine layer (this repo, compiled to the Steel dylib): owns a tokio runtime and the `App`. Translates key events into `App` commands, drains `App`'s internal event channels (`pump`), and renders `App` state into a frame model: a list of runs `(x, y, text, style-tag)` where style tags are semantic (`text`, `dim`, `selection`, `diff-added`, `diff-removed`, `error`, ...). No colours are hardcoded in Rust.
3. Steel layer (thin, target ~200-300 lines): component lifecycle, window sizing, forwarding key events over FFI, resolving style tags to Helix theme styles via `theme-scope`, blitting runs with `frame-set-string!`, cursor handler, and a poll loop (`enqueue-thread-local-callback-with-delay`) that runs while the engine reports background work pending.

Event flow:

- Key pressed in the component -> Steel decodes to (code, modifiers) -> FFI `handle-key` -> `App::handle_key_event` -> synchronous drain of ready internal events -> returns action (`rerender`, `hide`, `open-file`, `exit`, ...).
- Background work (debounce timers, search batches, preview updates, replacement progress, toast dismissal) lands in `App`'s channels from tokio tasks; the Steel poll loop calls FFI `pump` (non-blocking drain) on a ~50ms tick while `busy?` is true, triggering rerenders.
- Rendering: component render callback passes (width, height) -> FFI `render` returns runs + cursor position -> Steel blits.

## Key decisions

- Native look: no ratatui/TUI pass-through. All styles resolved from the Helix theme; layout structure mirrors the TUI.
- No syntax highlighting in the preview pane for v1: preview shows plain context lines plus diff colouring from theme scopes (like the old plugin). Syntax highlighting is wanted eventually — see future work.
- Config via Steel, not scooter's config.toml. Keymaps use the same string syntax as scooter ("C-o", "A-m", parsed by core's `KeyEvent::FromStr`) and core's conflict detection. Behaviour defaults (multiline, hidden files, advanced regex, include git folders, escape sequences) and plugin options (window size) are also Steel-configurable. scooter's default keymap remains the default.
- Session semantics preserved from the old plugin: `:scooter` resumes an existing session (window can be hidden and re-shown; background searches keep running while hidden), `:scooter-new` cancels all in-flight work and starts fresh.
- Escape handling: esc is forwarded to `App` whenever it would do something (close popup, exit multiselect, back-to-fields from results). If it would fall through unhandled (fields focus, nothing to dismiss), the window hides instead. The quit command (default `C-c`) destroys the session and closes the window.
- Open in editor: intercepted from `App` (never shells out to $EDITOR). `e` opens the result in Helix, hides the window (session kept); `alt-e` opens it in the background without hiding. After a replacement completes, non-dirty Helix documents are reloaded.
- One PR in `scooter` and one PR in `scooter.hx` at the end; no intermediate PRs. Development happens on the `rewrite` branch here and a companion branch in `../scooter`, with scooter-core consumed as a path dependency until release.
- The plugin depends only on `scooter-core` (not the `scooter` TUI crate).

## Target environment

- Helix branch `steel-event-system` (`~/Development/helix`), which pins steel-core 0.8.2 (git rev `dec633b...` in its Cargo.lock). The plugin must pin steel-core/steel-derive to the same rev; dylib loading is version-checked via abi_stable, so the old plugin's 0.7.0 pin will not load.
- `hx` for testing is built from that branch to `~/Development/helix/target/release/hx` (not `cargo install`, to avoid clobbering the daily-driver install).
- All e2e testing uses an isolated environment: scratch `STEEL_HOME` (with helix cogs generated into it and the plugin dylib installed into `<steel-home>/native`), scratch `XDG_CONFIG_HOME` with a minimal helix config + init.scm requiring the plugin, and `HELIX_RUNTIME` pointed at the helix checkout's runtime dir. Nothing touches `~/.steel` or `~/.config/helix`.

## Upstream changes needed in ../scooter

Accumulated on the `new-plugin-changes` branch in `../scooter`, one PR at the end:

- `EventChannels::try_recv` so the plugin can pump events without an async executor on the caller side. (Done, commit 9387c36.)
- Introspection needed for esc handling: either make `KeyMap::lookup` pub, or add a small `App` helper (e.g. `would_handle_key`) plus pub access to popup/multiselect state where not already available.
- Whatever small pub accessors emerge during implementation. Keep this list updated.

## Work chunks and status

Statuses: todo / in progress / in review / done. Each chunk has a spec in `docs/specs/` that is handed to Codex, plus a report written back by Codex; both are committed on this branch for resilience and pruned before the final PR.

| Chunk | Description | Status |
|-------|-------------|--------|
| S1 | Toolchain spike: dylib skeleton on steel-core 0.8.2 rev, static styled runs blitted in a popup with theme-scope styles, key round-trip, e2e tmux harness script | done |
| S2 | App embedded: tokio runtime, handle-key/pump/busy?, minimal fields view, live search visible in hx (milestone: Tom tries it) | done |
| U1 | Upstream scooter branch: try_recv, esc introspection, misc pub accessors | todo |
| E3 | Renderer: search fields screen (fields, errors, cursor, collapse behaviour, TUI-style checkboxes, size responsiveness) | done |
| E4 | Renderer: results list + preview (context lines, single/multiline diffs, wrapping, preview errors) | done |
| E5 | Renderer: footer hints, popups (error/help/text), toasts, performing-replacement + results screens | done |
| F1 | TUI parity fixes from milestone review: no preview line numbers (2-space context prefix), right-aligned (n) result indices in list rows, red/blue selection split for excluded/included rows, selection only when results focussed, " Scooter " window border title, plus popup colour-bleed fix (explicit fg/bg on overlay styles) | done |
| E6 | Pump/session lifecycle: busy?, action queue, reset/cancel-all, hide/resume correctness, green popup borders, themed border e2e | done |
| E7 | Headless frame snapshot suite (insta) over engine render output | done |
| A1 | Audit fixes: overlay surface-bg unification (popup border/interior mismatch), styling invariant table, file logging instead of stderr, tag enum, view.rs module split, teardown ergonomics, wrap parity, themed bg e2e | done |
| C1 | Steel config surface (`scooter-set!`, `scooter-keys!`) + full key decode table (absorbs old E1/E2/H2; H1 was absorbed by S1-F1) | done |
| H3 | Helix behaviours: open in editor (fg/bg), reload non-dirty docs after replace | done |
| P | Polish: README, cog.scm, CI (test + release workflows), delete dead code, dependency strategy | done |

Phase 2 (post-rewrite, agreed 2026-07-10): syntax highlighting + docs automation before the release gif.

| Chunk | Description | Status |
|-------|-------------|--------|
| SH0 | Spike: load a helix runtime grammar via tree-house, run highlights.scm, print theme-scope spans (proves ABI/loader) | done |
| SH1 | Highlight engine: language detection (languages.toml + fallback map), grammar/query loading with inherits + caching, spans API, scope:<name> run tags resolved in Steel | done |
| SH2 | Preview integration: highlighted context lines, preview pane background = editor background (deliberate Helix-native deviation from TUI), snapshots + themed SGR e2e, perf guard | done |
| D1 | Docs automation like scooter's xtask: declarative options table as single source of truth, xtask readme generator (TOC/CONFIG/KEYS markers, core keys.rs doc parsing via cargo metadata), CI --check job | done |
| A2 | Deep review sweep: languages.toml shadowing fix, preview I/O caching, pump-delivered hide handling, non-UTF8 fallback, span scan perf, quality sweep | done |
| END | Endgame: merge scooter PR, publish scooter-core, repoint dep, Tom regenerates gif, prune docs/specs, final PRs | todo |

Sequencing (updated): SH0 -> SH1 -> SH2 -> D1 -> END. Original phase-1 sequencing was E7 -> C1 -> H3 -> P. The original E1/E2/H1/H2 rows were absorbed: H1 (Steel shim) landed incrementally across S1-F1; E1+E2+H2 merged into C1. U1 so far needs only try_recv (already on the scooter branch).

## Validation

Three layers:

1. Codex self-validation (required before any chunk is handed back): renders must be checked at multiple terminal sizes (small/medium/large — `scripts/e2e-sizes.sh` once it lands in E3); `scripts/check.sh` clean (build + clippy + tests on a single pinned toolchain; a bare `cargo clippy` fails on this machine due to a Nix/rustup toolchain mix — see the script header); insta snapshots for renderer changes; and for anything user-visible, the e2e harness — build the dylib, install into the scratch STEEL_HOME, launch `hx` in tmux against a fixture workspace, send keys, capture panes, and assert on expected content. The harness (built in S1) lives in `scripts/` so both Codex and Claude run exactly the same checks.
2. Claude review: diff review against the chunk spec, full test suite, independent e2e runs in tmux, and behaviour comparison against the real scooter TUI running on the same fixture (same core, so it's a strong oracle for behaviour even though rendering differs).
3. Tom at milestones: tests the locally built plugin + hx via `scripts/try.sh [directory]`, which launches the isolated dev build in any directory without touching the real helix/steel setup. First milestone after S2, second after H3, final before PRs.

## Future work (post-v1)

- Syntax highlighting in the preview pane, done natively: load Helix's runtime tree-sitter grammars + highlight queries directly in the dylib, map capture names through the user's Helix theme (`theme-scope`). This keeps highlighting consistent with the editor. The TUI's syntect-based approach is deliberately not used.
- Possible upstreaming of the preview view-model (diff segment computation) into scooter-core so the TUI and plugin share it.
- Buffer diffing / partial repaints over FFI if profiling shows blitting cost matters.
