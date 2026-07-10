# SH1: native syntax highlight engine

Read `docs/REWRITE-PLAN.md` (phase 2) and `docs/specs/SH0-REPORT.md` — the spike proved the path and documents the exact API recipe (Grammar::new, LanguageConfig with all three queries, `tree_house::read_query` for inherits, Syntax::new + Highlighter walk, versions 0.4.0/0.3.2 matching helix's lock). SH1 turns that into the production engine. Preview integration is SH2 — this chunk builds and tests the engine plus the wire-format extension, without changing what the preview renders.

## Deliverables

1. Promote `tree-house`/`tree-house-bindings` (helix's exact versions, default features off) + `ropey` to real dependencies. The spike test may be folded into the new engine's tests or deleted.

2. `src/highlight/` module:
   - Runtime discovery: candidates in order — a plugin config override (`scooter-set! 'runtime-dir`, add to C1's option table), `$HELIX_RUNTIME`, `~/.config/helix/runtime`, `~/Development/helix/runtime` last-ditch for dev. First candidate containing `grammars/` wins. Also check whether the generated helix cogs expose a runtime-path accessor Steel could pass in at engine creation (look in `.dev/steel-home/cogs/helix/`); if one exists, prefer passing it from Steel over guessing.
   - Language registry: parse `<runtime>/../languages.toml` when present (source checkouts) for language name, `file-types` (extensions AND exact filenames AND glob suffixes — check the real file's shapes), `grammar` override field, and `injection-regex`. Fall back to a curated built-in map (~40 common languages: rust, python, js/ts/tsx, go, c, cpp, java, ruby, sh/bash, toml, yaml, json, md, html, css, scss, lua, zig, hs, ml, nix, scheme, etc.) when no languages.toml is found. The registry answers: language-for-path, and language-for-injection-marker (name/alias match plus injection-regex).
   - Loader: implements tree-house's `LanguageLoader`; lazily loads + caches per-language `LanguageConfig` (grammar dylib with platform extension fallback, `read_query` for highlights/injections/locals). Failures (missing grammar, bad query, unsupported predicate) log once per language and mark it unavailable — NEVER panic or error the caller.
   - Public engine API: `fn highlight(&self, path: &Path, content: &str) -> Option<Arc<[HighlightSpan]>>` where `HighlightSpan { byte_range: Range<usize>, scope: Arc<str> }`. `None` = no highlighting available (unknown language, too big, timed out, load failure) — caller renders plain.
   - Guards: skip content > 512 KiB; pass a parse timeout (~100ms) to `Syntax::new`; on timeout return None and log once per path.
   - Caching: LRU (~16 entries) keyed by (path, content-hash) holding the spans Arc. Content hash because the preview re-reads files that may change. Reuse the `lru` crate.

3. Wire-format extension: `Run` tags may now be a dynamic scope, encoded over FFI as `s:<scope>` (e.g. `s:keyword.function`). Rust side: extend `StyleTag` with a `Scope(Arc<str>)` variant (keep `as_str()` producing the prefixed form; adjust the enum/encoding pragmatically). Steel side (`ui/window.scm`): tags starting with `s:` resolve via `theme-scope` on the scope name with a per-render lazy cache (a mutable hash built during the render call), falling back to the `text` style for unknown scopes. These are content styles per the invariant table: fg-patch only. Document in the invariant table.

4. Tests (respect the spike's skip-when-no-runtime pattern so CI without helix passes):
   - language registry: extension/filename detection from a synthetic languages.toml + fallback map behaviour.
   - engine: rust spans (assert real scopes as in the spike), injections (markdown fence), cache hit (same path+content returns the same Arc), size cap, unknown language -> None.
   - wire: a `Scope` tag round-trips through the FFI encoding; Steel-side unknown-scope fallback covered by an e2e or unit approach as practical.
   - No visible rendering change yet: E7 snapshots must be untouched.

## Acceptance criteria

- `scripts/check.sh` passes; all e2e scripts pass once each (no behaviour change expected).
- Engine tests pass with `HELIX_RUNTIME=~/Development/helix/runtime`.
- Report `docs/specs/SH1-REPORT.md`: decisions (runtime discovery outcome — especially whether Steel can supply the path), curated-map contents, any upstream/tree-house pitfalls beyond the spike's findings.
- Do not commit. Do not modify `../scooter` or the helix checkout.
