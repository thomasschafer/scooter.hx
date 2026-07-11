# A2: deep review sweep — correctness, performance, reliability

Read `docs/REWRITE-PLAN.md` for context. This is a reviewed audit backlog over the entire rewrite (phases 1+2), like A1: work the items IN ORDER, self-validate as you go, and use the working loop at the bottom. You have latitude to fix additional defects of the same families you find on the way — document every extra fix. TUI parity and the E7+ snapshot suite are your tripwires. `../scooter` and the helix checkout stay read-only.

## Correctness and reliability

1. (HIGH) User `languages.toml` shadowing in `src/highlight/mod.rs`: `LanguageRegistry::from_runtime` reads `<runtime>/../languages.toml`. For the common installed layout `~/.config/helix/runtime`, the parent file is the USER'S override manifest, which typically defines only the few languages the user customises — and because it parses non-empty, the registry then knows ONLY those languages, silently losing highlighting for everything else. Fix: merge — parsed definitions take precedence per language name, the built-in fallback set fills every gap (order matters for `language_for_path` first-match: parsed entries first, then fallback entries whose names are not already present). Test: a two-language user manifest still resolves `.rs`, `.py`, and an injection alias from the fallback set, while the user's entries win for their names/file-types.

2. (HIGH) Per-render preview I/O in `src/view.rs::read_preview_window`: every render with a visible preview does `fs::metadata` + full-file `fs::read_to_string` (up to 512 KiB) + `indexed_full_lines` (allocates an owned String for EVERY line of the whole file, then keeps ~2×height of them) + a full content hash in the engine's cache key. The poll loop renders ~20×/s while busy. Fix:
   - Add a small content cache: `(path) -> (mtime, len, Arc<str>)`, validated per render by a cheap `fs::metadata` stat — reuse the Arc when mtime+len match, re-read otherwise. This PRESERVES the freshness the "File content has changed" guard depends on; do not cache without the stat validation. An LRU of ~8 entries is plenty; it can live beside the engine's spans cache or in the view layer — pick the cleaner seam.
   - Make the line-window extraction allocate only the needed window: iterate `split_inclusive` with running byte offsets and collect only lines in `[start..=end]` (full content still goes to the highlighter).
   - Avoid re-hashing unchanged content: when the content Arc is reused from the cache, the engine can key its span lookup by pointer identity or a stored hash rather than re-hashing 512 KiB (extend `HighlightEngine::highlight` to accept a precomputed key or an `Arc<str>` — your choice, keep the API tidy).
   - Test: extend the existing computation-counter pattern with a read counter (cfg(test)) proving a second render of the same selection performs zero file reads and zero hashes; plus a test that touching the file (different content, later mtime) IS picked up.

3. (MED) "hide" outcomes are dropped when actions arrive via pump instead of a key press: `start-scooter-poll-loop!` and `resume-session!` (ui/window.scm, scooter.scm) call `consume-scooter-response!` and discard the returned status. A foreground `open-file` delivered on a poll tick (or on the resume pump) opens the document but leaves the Scooter window up. Fix: when the poll/resume path gets `"hide"`, close the window properly — set the visible box to `#f` and pop the component by name (`pop-last-component-by-name!` is available; verify the exact call in the generated cogs). Cover with a test or e2e where a foreground open is queued while hidden, then `:scooter` resume delivers it: the window must close and the file open.

4. (MED) Non-UTF8 files with syntax highlighting enabled: `read_preview_window` hard-errors (`fs::read_to_string` fails), showing "Error generating preview: stream did not contain valid UTF-8" — while the windowed plain path (and the TUI) tolerate such files. Fix: on read failure, fall back to the windowed `read_lines_range` path (plain, no highlighting) instead of erroring. First verify what core's `surrounding_line_window` actually does with invalid UTF-8 lines and mirror the TUI's observable behaviour; add a test with a file containing invalid UTF-8 bytes.

5. (LOW) Defensive span slicing in `context_preview_line`: byte offsets index `source.text[..]` directly; a span boundary that is not a char boundary (malformed grammar output) would panic into the FFI guard. Use checked slicing (`str::get`) and render that line plain on failure.

6. (LOW) e2e flakiness: `scripts/e2e-replace.sh` uses a fixed `sleep 1` to wait for preview updates before triggering replacement. Replace with a polled condition (e.g. retry the enter keypress until the replacement screen appears, or poll for the absence of the preview-update banner segment). Audit the other scripts' raw `sleep`s (excluding poll intervals) the same way.

## Performance

7. (MED) `context_preview_line` scans the ENTIRE span list for every visible line (spans can be tens of thousands for a large file; ~40 visible lines each render). Spans are sorted by byte range: `partition_point` to the window start once, then advance a cursor across lines. Add a micro-test pinning the behaviour at line boundaries (span exactly ending at line start, spanning multiple lines, zero-length guard).

8. (LOW) Registry lookups: `language_for_path` runs on every `highlight()` call (before the cache check) and scans all ~200 languages; build `HashMap` indexes (extension -> language, exact filename -> language) at registry construction, keeping the linear scan only for globs. Also swap `intern_scope`'s linear scan for a map. Keep `language_for_path`'s first-match semantics stable (item 1's ordering).

## Code quality

9. Stale `#[allow(dead_code)]` on `StyleTag::Scope` in `src/view/canvas.rs` — it has been used since SH2; remove.

10. `src/logging.rs`: `simple_log::file(path, "warn", 100, 10)` allows ~1 GB of logs. Reduce to something proportionate (a few MB, 2 files).

11. `src/highlight/mod.rs::discover_runtime` hardcodes `~/Development/helix/runtime` as a final fallback — a machine-specific dev convenience in production code. Keep the behaviour but isolate and comment it explicitly as a development fallback, and make sure the README's runtime-dir documentation tells users the supported mechanisms (option, HELIX_RUNTIME, config dir).

12. `ui/window.scm` formatting nits: the misaligned `#%require-dylib` import list (line ~11) and the indentation inside `style-for-run`'s unknown-tag branch. Optionally collapse the 24-arm F-key cond into a loop if it reads better in Steel — only if genuinely clearer.

13. Small-file sweep with the same axes (correctness/perf/quality): `src/view/banner.rs`, `src/view/layout.rs`, `src/key.rs`, `xtask/src/build_readme.rs`, `scripts/e2e-env.sh`. Fix what you find of the families above; list findings (or "clean") per file in the report.

## Working loop

- Items in order. After each: `HELIX_RUNTIME=~/Development/helix/runtime scripts/check.sh` plus the most relevant e2e script(s). Snapshot diffs must be justified per item (items 1-8 should produce none unless noted).
- When all items are done: full validation twice consecutively — check.sh and every `scripts/e2e-*.sh`.
- Fresh-eyes pass: tmux captures (plain + `-e`) of the highlighted preview over a large real file (e.g. this repo's `src/view.rs`), rapid navigation responsiveness, and the popup/toast surfaces, under default + catppuccin_mocha. Fix and document anything of the audited families you spot.
- Write `docs/specs/A2-REPORT.md`: per-item outcome with evidence (for item 2: before/after counter numbers), extra fixes, per-file sweep results, validation transcript summary.

Do not commit. Do not modify `../scooter` or the helix checkout.
