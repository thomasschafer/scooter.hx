# SH0: tree-sitter highlighting spike

Read `docs/REWRITE-PLAN.md` for context (phase 2). This spike proves the native syntax-highlighting path before SH1 builds the real engine. Keep it minimal and throwaway-quality where noted; the deliverable is EVIDENCE, written up in the report.

## Background (verified facts)

- Helix performs its tree-sitter work via the `tree-house` crate (crates.io, 0.4.0 in helix's lock; `tree-house-bindings` 0.3.2). Match those versions.
- Grammars: `$HELIX_RUNTIME/grammars/<lang>.{so,dylib}` (267 present in `~/Development/helix/runtime/grammars`). Helix loads them via a C ABI entry point (see how `tree-house`/helix-loader resolve the symbol and platform extension).
- Queries: `$HELIX_RUNTIME/queries/<lang>/highlights.scm`, first line may be `; inherits: a,b` (e.g. cpp inherits c).
- Helix theme scopes ARE the capture names used by these queries (`keyword`, `function`, `string.special`, ...).

## Deliverables

1. A dev-only test or example binary (`examples/` or `#[cfg(test)]` integration test, NOT part of the dylib's public surface) that:
   - Adds `tree-house` (and bindings) as a dev-dependency at helix's versions.
   - Loads the `rust` grammar from `$HELIX_RUNTIME/grammars/` (env var, default to `~/Development/helix/runtime`).
   - Loads and compiles `queries/rust/highlights.scm` (no inherits needed for rust; ALSO do one inherits case, e.g. `cpp` -> prepend `c`, with a minimal first-line `; inherits:` expansion).
   - Highlights a small Rust source string and a small C++ string, producing `(byte_range, capture/scope name)` spans via tree-house's highlighter API (study how helix-core drives it — `helix-core/src/syntax.rs` in the helix checkout is the reference for API usage; our usage can be much simpler, no incremental editing needed).
   - Asserts a handful of expected spans (e.g. `fn` -> `keyword.function` or whatever the query actually captures — assert what is TRUE, discovering it is part of the spike).
2. Answer these questions explicitly in `docs/specs/SH0-REPORT.md`:
   - Does grammar loading work with helix's shipped dylibs on macOS (which extension, which symbol)? Any ABI/version pitfalls with tree-house 0.4?
   - What does the highlighter API need from us (loader trait? injections callback?) and what is the minimal correct usage for highlighting a STRING (not a document with edits)?
   - Do injections (e.g. markdown code fences) work out of the box with a loader that can serve multiple languages, or is that meaningful extra work? (Try markdown briefly; if it is a rabbit hole, say so and stop.)
   - Rough timing: highlight a ~2000-line file's full text — is synchronous per-render highlighting plausible, or does SH1 need caching from day one?
   - Anything about query features (precedence, locals, custom predicates) that the simple path gets wrong?
3. Keep ALL of this out of the production build (dev-deps / examples only). No changes to src/ beyond what is strictly needed (ideally none).

## Acceptance criteria

- `scripts/check.sh` passes (including your new test); existing e2e suite untouched and still passing (spot-run `e2e-smoke.sh`).
- The report answers every question above with evidence (actual spans, actual timings).
- Do not commit. Do not modify `../scooter` or `~/Development/helix`.
