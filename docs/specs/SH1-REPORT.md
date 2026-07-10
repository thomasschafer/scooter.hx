# SH1 report: native syntax highlight engine

## Result

SH1 is complete. The production `src/highlight/` engine discovers a Helix
runtime, detects languages from its adjacent `languages.toml` (or a curated
fallback), lazily loads Helix grammars and expanded Tree-house queries, and
returns cached UTF-8 byte spans carrying Tree-sitter scope names. It is not
yet wired into preview rendering; that remains SH2.

`tree-house` 0.4.0, `tree-house-bindings` 0.3.2, and `ropey` are now normal
dependencies, with `lru`, `regex`, and `toml` supporting the engine.

## Interrupted work audit

Kept from the interrupted implementation:

- the runtime discovery order and Steel `runtime-dir` option;
- the lazy `LanguageConfig` cache, grammar loader, query expansion through
  `tree_house::read_query`, scope interning, failure/timeout logging, and
  16-entry `(path, content-hash)` LRU;
- the 512 KiB and 100 ms guards;
- the `StyleTag::Scope(Arc<str>)` wire encoding (`s:<scope>`), plus the
  render-local Steel scope-style cache and foreground-only style invariant;
- the fallback registry and the Rust/Markdown-injection/cache tests.

Redone or corrected:

- Fixed the registry parser to read Helix's TOML *document root table* rather
  than a standalone `toml::Value`; before this fix every real
  `languages.toml` silently fell back to the built-in map.
- Made `highlights.scm` required for an available language. Missing grammars,
  primary highlight queries, bad queries, and parse failures now log once and
  produce `None`, so rendering always falls back to plain text.
- Completed the synthetic registry coverage for extension, exact filename,
  glob suffix, grammar override, and injection-regex matching; adjusted view
  ownership for the now-dynamic `StyleTag` and cleared strict clippy.

## Runtime discovery and registry

Discovery checks, in order: the configured `runtime-dir`, `HELIX_RUNTIME`,
`~/.config/helix/runtime`, then `~/Development/helix/runtime`. The first path
with `grammars/` wins. The generated Helix cogs in `.dev/steel-home/cogs/helix`
have no runtime-path accessor, so Steel cannot supply a more authoritative
path in this Helix build.

For source checkouts the engine parses `<runtime>/../languages.toml`, including
`name`, `grammar`, `file-types` strings and `{ glob = ... }` entries, and
`injection-regex`. It resolves file extensions, exact names, and suffix/glob
forms, and resolves injected names using language/grammar names and anchored
injection regexes. Packaged runtimes without that manifest use 40 common
entries: Rust, Python, JavaScript/TypeScript/TSX, Go, C/C++, Java, Ruby,
Bash, TOML, YAML, JSON, Markdown, HTML/CSS/SCSS, Lua, Zig, Haskell, OCaml,
Nix, Scheme, Clojure, Elixir, Erlang, PHP, Swift, Kotlin, Scala, C#, Dart,
SQL, Dockerfile, Make, XML, Vue, Svelte, and Protobuf.

## Tree-house notes

The SH0 ABI finding remains: Helix grammar dylibs use ABI 15 and must stay
paired with Tree-house/bindings 0.4.0/0.3.2. `LanguageConfig` receives all
three queries; `read_query` expands Helix `inherits` directives. The root
highlight query is required, while locals and injections are optional because
some shipped languages omit them. Tree-house's unsupported predicates surface
as a query compilation failure, which disables that language rather than
affecting the caller.

## Validation

- `HELIX_RUNTIME=~/Development/helix/runtime scripts/check.sh` passed: build,
  `clippy -D warnings`, 64 unit tests, and the retained SH0 runtime spike.
- Engine coverage with that runtime passed for Rust scopes, Markdown Rust-fence
  injection, cache Arc identity, unknown languages, and the size cap.
- Ran each isolated e2e script once: `e2e-config`, `e2e-lifecycle`,
  `e2e-live-search`, `e2e-open`, `e2e-preview`, `e2e-replace`, `e2e-sizes`,
  and `e2e-smoke`.

No commit was created. No files in `../scooter` or the Helix checkout were
modified.
