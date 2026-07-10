# SH0 report: tree-sitter highlighting spike

## Result

The native path works. `tests/sh0_treesitter_spike.rs` is a dev-only integration
test; the production library under `src/` is unchanged. It uses `tree-house`
0.4.0 (with default features disabled, as in Helix), `tree-house-bindings` 0.3.2,
and `ropey` only in `[dev-dependencies]`.

Run it against a particular runtime with:

```sh
HELIX_RUNTIME=/path/to/helix/runtime cargo test --test sh0_treesitter_spike -- --nocapture
```

Without `HELIX_RUNTIME`, it defaults to `~/Development/helix/runtime`; if neither
runtime exists, the test prints a skip message so ordinary CI does not need a
Helix checkout.

## Grammar loading and ABI

Yes, Helix's shipped macOS grammar dylibs load successfully. The test selected
`$HELIX_RUNTIME/grammars/rust.dylib` (then c, cpp, and markdown) and loaded each
with `tree_house_bindings::Grammar::new`. Its output reported tree-sitter ABI 15
for the grammars. `file runtime/grammars/rust.dylib` identified it as an arm64
Mach-O dynamic library, and `nm -gU` showed the exported C symbol
`_tree_sitter_rust`; `Grammar::new("rust", ...)` looks up `tree_sitter_rust`
(the leading underscore is Mach-O symbol-table spelling). On Linux the same test
will choose `.so`; the code checks `.dylib` first and then `.so`.

`tree-house-bindings` 0.3.2 accepts grammar ABI 13 through 15. The important
pitfall is not to use a different tree-sitter binding/version: its grammar loader
rejects an out-of-range ABI before parsing, and Helix's release runtime must be
paired with the `tree-house`/bindings version Helix uses. This spike matches the
versions in the Helix lockfile (0.4.0 / 0.3.2), and loading four actual shipped
grammars is the compatibility evidence.

## Minimal string highlighter API

The required pieces are:

1. Read `highlights.scm`, `injections.scm`, and `locals.scm`; expand the first
   file's `; inherits:` directive with `tree_house::read_query`.
2. Load a `Grammar` and construct `LanguageConfig::new(grammar, highlights,
   injections, locals)`. Call `configure` to map each capture name to the
   caller's opaque `Highlight` index. This spike keeps the reverse index so its
   output can be `(byte range, scope name)`.
3. Implement `LanguageLoader`: `get_config(Language)` returns that config and
   `language_for_marker(InjectionLanguageMarker)` resolves injected language
   names.
4. For an immutable string, make a `Rope`, call
   `Syntax::new(source.slice(..), language, timeout, &loader)`, then create
   `Highlighter::new(&syntax, source.slice(..), &loader, 0..)`. Iterate its byte
   boundaries with `next_event_offset` and `advance`, taking the active scope for
   each non-empty range. No document model or incremental edit API is required.

Actual spans from the test include:

```text
Rust: "fn" -> keyword.function
Rust: "greeting" -> function
Rust: "\"hi {name}\"" -> string
C++:  "int" -> type.builtin
C++:  "return" -> keyword.control.return
```

The C++ test uses `tree_house::read_query("cpp", ...)`; it asserts that the
expanded query contains C's `"return" @keyword.control.return` rule, proving
the `; inherits: c` first line was expanded rather than merely ignored.

## Injections

They work through the same API once the loader can serve every referenced
language. A Markdown source containing a ` ```rust ` fence created nested
`markdown -> rust` syntax layers, and the source text `fn` inside the fence
received `keyword.function`. This used Markdown's shipped `injections.scm` and
the normal `InjectionLanguageMarker::Match` callback; no special injection
callback or separate parse loop was needed.

That is meaningful loader work for SH1: it must load/cache configurations for
all possible injected languages and resolve aliases/fences (Helix's loader also
does filename, shebang, and regex matching). The spike intentionally supports
only the exact `rust` fence it exercises, and returns `None` for unknown
languages; it demonstrates the mechanism, not the required language registry.

## Timing and caching conclusion

The test generated 2,000 lines of Rust:

```rust
pub fn line_N() { let text = "value"; }
```

It parsed and highlighted the complete 2,000-line string into 24,000 spans in
24.39 ms in an optimized (`cargo test --release`) local run on this machine.
An unoptimized test run was 161.01 ms. The measurement includes creating the
`Rope`, building `Syntax`, and walking all highlighter events; it excludes
grammar/query loading because the loader is constructed first.

Synchronous full-file highlighting on every render is therefore not plausible:
even the optimized figure consumes more than a 60 Hz frame budget, and preview
rerenders can be frequent. SH1 needs syntax/config caching and SH2 should cache
spans (or at least invalidate/recompute only when preview text/language changes)
from day one.

## Query semantics and limits of the simple path

The path is not a raw tree-sitter-query shortcut: passing all three shipped
queries to `LanguageConfig` is necessary. `tree-house` applies built-in text
predicates (`#eq?`, `#match?`, `#any-of?` and their negated/any variants), which
are used by the tested Rust/C/C++ runtime queries. It also combines `locals.scm`
with highlights to resolve `@local.reference`, and its highlighter applies the
same-node precedence rule that the last matching capture wins (documented in
tree-house as matching Neovim, Zed, and tree-sitter-cli). Its injection handling
also computes nested/overlapping injection precedence.

The simple spike would get semantics wrong if SH1 omitted `locals.scm`, omitted
injection queries, skipped inherited query expansion, or mapped all unknown
scopes to no highlight. It also deliberately rejects unknown custom predicates:
`LanguageConfig` only accepts its supported `local.*` and `injection.*` property
predicates in addition to the built-in text predicates. If a future Helix query
uses a different custom predicate/property, SH1 must add explicit support (or
report and disable that query) rather than silently treating it as correct.

## Validation

- `cargo test --test sh0_treesitter_spike -- --nocapture` — passed; debug timing
  161.01 ms.
- `cargo test --release --test sh0_treesitter_spike -- --nocapture` — passed;
  release timing 24.39 ms.
- `scripts/check.sh` — passed.
- `scripts/e2e-smoke.sh` — passed.
