//! Runtime-backed, fault-tolerant Tree-sitter highlighting for preview text.
//!
//! This module deliberately has no view dependency. SH2 will decide when and
//! how to turn its byte spans into [`crate::view::StyleTag::Scope`] runs.

use std::{
    collections::{HashMap, HashSet},
    env, fs,
    hash::{Hash, Hasher},
    num::NonZeroUsize,
    ops::Range,
    path::{Path, PathBuf},
    sync::{Arc, Mutex, OnceLock},
    time::Duration,
};

use lru::LruCache;
use regex::Regex;
use ropey::Rope;
use tree_house::{
    Error as TreeHouseError, InjectionLanguageMarker, Language, LanguageConfig, LanguageLoader,
    Syntax,
    highlighter::{Highlight, Highlighter},
    read_query,
};
use tree_house_bindings::Grammar;

/// The largest file preview rendering reads in full for syntax highlighting.
/// Larger files retain the inexpensive windowed preview path.
pub(crate) const MAX_CONTENT_BYTES: usize = 512 * 1024;
const PARSE_TIMEOUT: Duration = Duration::from_millis(100);
const CACHE_CAPACITY: usize = 16;

/// A semantic Tree-sitter scope over UTF-8 byte offsets in the input text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HighlightSpan {
    pub byte_range: Range<usize>,
    pub scope: Arc<str>,
}

/// Highlight text using grammars and queries from a Helix runtime.
///
/// All external/runtime failures become `None`: preview rendering must always
/// be able to fall back to plain text.
pub struct HighlightEngine {
    loader: Option<RuntimeLoader>,
    cache: Mutex<LruCache<CacheKey, Arc<[HighlightSpan]>>>,
    timeout_paths: Mutex<HashSet<PathBuf>>,
    #[cfg(test)]
    highlight_computations: std::sync::atomic::AtomicUsize,
}

impl HighlightEngine {
    /// Discover a runtime, using `runtime_override` ahead of the normal Helix
    /// locations. Construction never fails because highlighting is optional.
    pub fn new(runtime_override: Option<PathBuf>) -> Self {
        let runtime = discover_runtime(runtime_override);
        let loader = runtime.map(RuntimeLoader::new);
        Self {
            loader,
            cache: Mutex::new(LruCache::new(
                NonZeroUsize::new(CACHE_CAPACITY).expect("non-zero cache capacity"),
            )),
            timeout_paths: Mutex::new(HashSet::new()),
            #[cfg(test)]
            highlight_computations: std::sync::atomic::AtomicUsize::new(0),
        }
    }

    /// Return semantic scopes for `content`, or `None` when the runtime,
    /// language, grammar, query, size, or parse deadline cannot support it.
    pub fn highlight(&self, path: &Path, content: &str) -> Option<Arc<[HighlightSpan]>> {
        if content.len() > MAX_CONTENT_BYTES {
            return None;
        }

        let loader = self.loader.as_ref()?;
        let language = loader.registry.language_for_path(path)?;
        let key = CacheKey::new(path, content);
        if let Ok(mut cache) = self.cache.lock()
            && let Some(spans) = cache.get(&key)
        {
            return Some(Arc::clone(spans));
        }

        let source = Rope::from_str(content);
        #[cfg(test)]
        self.highlight_computations
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let syntax = match Syntax::new(source.slice(..), language, PARSE_TIMEOUT, loader) {
            Ok(syntax) => syntax,
            Err(TreeHouseError::Timeout) => {
                self.log_timeout_once(path);
                return None;
            }
            Err(error) => {
                loader.log_language_failure_once(language, &format!("parse failed: {error}"));
                return None;
            }
        };
        let mut highlighter = Highlighter::new(&syntax, source.slice(..), loader, 0..);
        let mut spans = Vec::new();
        let source_len = u32::try_from(source.len_bytes()).ok()?;
        let mut position = highlighter.next_event_offset();
        while position != u32::MAX {
            let _ = highlighter.advance();
            let next = highlighter.next_event_offset().min(source_len);
            if position < next
                && let Some(highlight) = highlighter.active_highlights().next_back()
                && let Some(scope) = loader.scope(highlight)
            {
                spans.push(HighlightSpan {
                    byte_range: usize::try_from(position).ok()?..usize::try_from(next).ok()?,
                    scope,
                });
            }
            position = highlighter.next_event_offset();
        }

        let spans: Arc<[HighlightSpan]> = spans.into();
        if let Ok(mut cache) = self.cache.lock() {
            cache.put(key, Arc::clone(&spans));
        }
        Some(spans)
    }

    #[cfg(test)]
    fn with_runtime(runtime: PathBuf) -> Self {
        Self::new(Some(runtime))
    }

    #[cfg(test)]
    pub(crate) fn highlight_computations(&self) -> usize {
        self.highlight_computations
            .load(std::sync::atomic::Ordering::Relaxed)
    }

    fn log_timeout_once(&self, path: &Path) {
        let path = path.to_path_buf();
        if let Ok(mut paths) = self.timeout_paths.lock()
            && paths.insert(path.clone())
        {
            log::warn!(
                "scooter-hx: syntax highlighting timed out for {}; using plain preview text",
                path.display()
            );
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct CacheKey {
    path: PathBuf,
    content_hash: u64,
}

impl CacheKey {
    fn new(path: &Path, content: &str) -> Self {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        content.hash(&mut hasher);
        Self {
            path: path.to_path_buf(),
            content_hash: hasher.finish(),
        }
    }
}

fn discover_runtime(runtime_override: Option<PathBuf>) -> Option<PathBuf> {
    let mut candidates = Vec::new();
    if let Some(path) = runtime_override {
        candidates.push(path);
    }
    if let Some(path) = env::var_os("HELIX_RUNTIME") {
        candidates.push(PathBuf::from(path));
    }
    if let Some(home) = env::var_os("HOME") {
        let home = PathBuf::from(home);
        candidates.push(home.join(".config/helix/runtime"));
        candidates.push(home.join("Development/helix/runtime"));
    }
    candidates
        .into_iter()
        .find(|path| path.join("grammars").is_dir())
}

#[derive(Debug)]
struct RuntimeLoader {
    runtime: PathBuf,
    registry: LanguageRegistry,
    configs: Vec<OnceLock<Option<LanguageConfig>>>,
    scopes: Mutex<Vec<Arc<str>>>,
    failures: Mutex<HashSet<usize>>,
}

impl RuntimeLoader {
    fn new(runtime: PathBuf) -> Self {
        let registry = LanguageRegistry::from_runtime(&runtime);
        let configs = (0..registry.languages.len())
            .map(|_| OnceLock::new())
            .collect();
        Self {
            runtime,
            registry,
            configs,
            scopes: Mutex::new(Vec::new()),
            failures: Mutex::new(HashSet::new()),
        }
    }

    fn scope(&self, highlight: Highlight) -> Option<Arc<str>> {
        self.scopes.lock().ok()?.get(highlight.idx()).cloned()
    }

    fn config_for(&self, language: Language) -> Option<&LanguageConfig> {
        let index = language.idx();
        let slot = self.configs.get(index)?;
        slot.get_or_init(|| self.load_config(index)).as_ref()
    }

    fn load_config(&self, index: usize) -> Option<LanguageConfig> {
        let language = self.registry.languages.get(index)?;
        let language_id = Language::new(u32::try_from(index).ok()?);
        let Some(grammar_path) = grammar_path(&self.runtime, &language.grammar) else {
            self.log_language_failure_once(
                language_id,
                &format!("missing grammar dylib for {}", language.grammar),
            );
            return None;
        };
        // SAFETY: grammar paths are selected exclusively from Helix's runtime
        // grammar directory, and Grammar owns the opened dynamic library.
        let grammar = match unsafe { Grammar::new(&language.grammar, &grammar_path) } {
            Ok(grammar) => grammar,
            Err(error) => {
                self.log_language_failure_once(
                    language_id,
                    &format!("failed to load {}: {error}", grammar_path.display()),
                );
                return None;
            }
        };
        // A language without a highlights query is not useful to this engine.
        // The other two queries are optional in Helix runtimes, but must still
        // be supplied when present: `locals.scm` affects local references and
        // `injections.scm` is what enables fenced/embedded languages.
        let Some(highlights) = runtime_query(&self.runtime, &language.name, "highlights.scm")
        else {
            self.log_language_failure_once(language_id, "missing highlights.scm");
            return None;
        };
        let injections =
            runtime_query(&self.runtime, &language.name, "injections.scm").unwrap_or_default();
        let locals = runtime_query(&self.runtime, &language.name, "locals.scm").unwrap_or_default();
        let config = match LanguageConfig::new(grammar, &highlights, &injections, &locals) {
            Ok(config) => config,
            Err(error) => {
                self.log_language_failure_once(
                    language_id,
                    &format!("failed to compile runtime queries: {error}"),
                );
                return None;
            }
        };
        config.configure(|scope| self.intern_scope(scope));
        Some(config)
    }

    fn intern_scope(&self, scope: &str) -> Option<Highlight> {
        let mut scopes = self.scopes.lock().ok()?;
        let index = scopes
            .iter()
            .position(|known| known.as_ref() == scope)
            .unwrap_or_else(|| {
                scopes.push(Arc::from(scope));
                scopes.len() - 1
            });
        u32::try_from(index).ok().map(Highlight::new)
    }

    fn log_language_failure_once(&self, language: Language, message: &str) {
        let index = language.idx();
        if let Ok(mut failures) = self.failures.lock()
            && failures.insert(index)
        {
            let name = self
                .registry
                .languages
                .get(index)
                .map_or("unknown", |language| language.name.as_str());
            log::warn!("scooter-hx: syntax highlighting unavailable for {name}: {message}");
        }
    }
}

impl LanguageLoader for RuntimeLoader {
    fn language_for_marker(&self, marker: InjectionLanguageMarker<'_>) -> Option<Language> {
        match marker {
            InjectionLanguageMarker::Name(name) => self.registry.language_for_injection(name),
            InjectionLanguageMarker::Match(value) | InjectionLanguageMarker::Shebang(value) => {
                self.registry.language_for_injection(&value.to_string())
            }
            InjectionLanguageMarker::Filename(value) => self
                .registry
                .language_for_path(Path::new(&value.to_string())),
        }
    }

    fn get_config(&self, language: Language) -> Option<&LanguageConfig> {
        self.config_for(language)
    }
}

fn grammar_path(runtime: &Path, grammar: &str) -> Option<PathBuf> {
    ["dylib", "so", "dll"]
        .into_iter()
        .map(|extension| {
            runtime
                .join("grammars")
                .join(format!("{grammar}.{extension}"))
        })
        .find(|path| path.is_file())
}

fn runtime_query(runtime: &Path, language: &str, query: &str) -> Option<String> {
    let path = runtime.join("queries").join(language).join(query);
    path.is_file().then(|| {
        read_query(language, |name| {
            fs::read_to_string(runtime.join("queries").join(name).join(query)).unwrap_or_default()
        })
    })
}

#[derive(Debug)]
struct LanguageRegistry {
    languages: Vec<LanguageDefinition>,
    names: HashMap<String, Language>,
}

#[derive(Debug)]
struct LanguageDefinition {
    name: String,
    grammar: String,
    file_types: Vec<FileType>,
    injection_regex: Option<Regex>,
}

#[derive(Debug)]
enum FileType {
    NameOrExtension(String),
    Glob(String),
}

impl LanguageRegistry {
    fn from_runtime(runtime: &Path) -> Self {
        let manifest = runtime.parent().map(|parent| parent.join("languages.toml"));
        manifest
            .as_deref()
            .and_then(parse_languages_toml)
            .filter(|languages| !languages.is_empty())
            .map_or_else(Self::fallback, Self::from_definitions)
    }

    fn from_definitions(languages: Vec<LanguageDefinition>) -> Self {
        let mut names = HashMap::new();
        for (index, language) in languages.iter().enumerate() {
            let language_id = Language::new(u32::try_from(index).expect("language index fits u32"));
            // A language's own name is authoritative. Several Helix entries
            // deliberately share a grammar (for example markdown-rustdoc),
            // and those aliases must not replace the ordinary language.
            names
                .entry(language.name.to_ascii_lowercase())
                .or_insert(language_id);
            names
                .entry(language.grammar.to_ascii_lowercase())
                .or_insert(language_id);
        }
        Self { languages, names }
    }

    fn language_for_path(&self, path: &Path) -> Option<Language> {
        let file_name = path.file_name()?.to_string_lossy();
        let extension = path
            .extension()
            .map(|extension| extension.to_string_lossy());
        self.languages
            .iter()
            .enumerate()
            .find_map(|(index, language)| {
                language
                    .file_types
                    .iter()
                    .any(|file_type| match file_type {
                        FileType::NameOrExtension(value) => {
                            value == &file_name || extension.as_deref() == Some(value)
                        }
                        FileType::Glob(pattern) => glob_matches(pattern, &file_name, path),
                    })
                    .then(|| Language::new(u32::try_from(index).expect("language index fits u32")))
            })
    }

    fn language_for_injection(&self, marker: &str) -> Option<Language> {
        let normalized = marker.trim().to_ascii_lowercase();
        self.names.get(&normalized).copied().or_else(|| {
            self.languages
                .iter()
                .enumerate()
                .find_map(|(index, language)| {
                    language.injection_regex.as_ref().and_then(|regex| {
                        regex.is_match(&normalized).then(|| {
                            Language::new(u32::try_from(index).expect("language index fits u32"))
                        })
                    })
                })
        })
    }

    fn fallback() -> Self {
        Self::from_definitions(fallback_languages())
    }
}

fn parse_languages_toml(path: &Path) -> Option<Vec<LanguageDefinition>> {
    // `toml::Value` parses a single TOML value with the 1.x crate.  Helix's
    // `languages.toml` is a document, so parse its root table directly.
    let document = fs::read_to_string(path).ok()?.parse::<toml::Table>().ok()?;
    let entries = document.get("language")?.as_array()?;
    Some(entries.iter().filter_map(parse_language).collect())
}

fn parse_language(value: &toml::Value) -> Option<LanguageDefinition> {
    let table = value.as_table()?;
    let name = table.get("name")?.as_str()?.to_owned();
    let grammar = table
        .get("grammar")
        .and_then(toml::Value::as_str)
        .unwrap_or(&name)
        .to_owned();
    let injection_regex = table
        .get("injection-regex")
        .and_then(toml::Value::as_str)
        .and_then(injection_regex);
    let file_types = table
        .get("file-types")
        .and_then(toml::Value::as_array)
        .map_or_else(Vec::new, |types| {
            types
                .iter()
                .filter_map(|value| {
                    value
                        .as_str()
                        .map(|value| FileType::NameOrExtension(value.to_owned()))
                        .or_else(|| {
                            value
                                .get("glob")
                                .and_then(toml::Value::as_str)
                                .map(|glob| FileType::Glob(glob.to_owned()))
                        })
                })
                .collect()
        });
    Some(LanguageDefinition {
        name,
        grammar,
        file_types,
        injection_regex,
    })
}

fn injection_regex(pattern: &str) -> Option<Regex> {
    Regex::new(&format!("^(?:{pattern})$")).ok()
}

fn glob_matches(pattern: &str, file_name: &str, path: &Path) -> bool {
    let path = path.to_string_lossy();
    if !pattern.contains('*') && !pattern.contains('{') {
        return file_name.ends_with(pattern) || path.ends_with(pattern);
    }
    brace_expansions(pattern)
        .into_iter()
        .any(|pattern| wildcard_matches(&pattern, file_name) || wildcard_matches(&pattern, &path))
}

fn brace_expansions(pattern: &str) -> Vec<String> {
    let Some(open) = pattern.find('{') else {
        return vec![pattern.to_owned()];
    };
    let Some(close) = pattern[open..].find('}').map(|offset| open + offset) else {
        return vec![pattern.to_owned()];
    };
    pattern[open + 1..close]
        .split(',')
        .map(|choice| format!("{}{}{}", &pattern[..open], choice, &pattern[close + 1..]))
        .collect()
}

fn wildcard_matches(pattern: &str, text: &str) -> bool {
    let (mut pattern_index, mut text_index, mut star, mut retry) = (0, 0, None, 0);
    let pattern = pattern.as_bytes();
    let text = text.as_bytes();
    while text_index < text.len() {
        if pattern_index < pattern.len() && pattern[pattern_index] == text[text_index] {
            pattern_index += 1;
            text_index += 1;
        } else if pattern_index < pattern.len() && pattern[pattern_index] == b'*' {
            star = Some(pattern_index);
            pattern_index += 1;
            retry = text_index;
        } else if let Some(star_index) = star {
            pattern_index = star_index + 1;
            retry += 1;
            text_index = retry;
        } else {
            return false;
        }
    }
    while pattern_index < pattern.len() && pattern[pattern_index] == b'*' {
        pattern_index += 1;
    }
    pattern_index == pattern.len()
}

fn fallback_languages() -> Vec<LanguageDefinition> {
    // Keep this useful when a packaged runtime omits its source-checkout
    // languages.toml. Grammar names are Helix's usual shipped names.
    const LANGUAGES: &[(&str, &str, &[&str], &str)] = &[
        ("rust", "rust", &["rs"], "rs|rust"),
        ("python", "python", &["py", "pyi", "pyw"], "py|python"),
        (
            "javascript",
            "javascript",
            &["js", "mjs", "cjs"],
            "js|javascript",
        ),
        ("typescript", "typescript", &["ts"], "ts|typescript"),
        ("tsx", "tsx", &["tsx"], "tsx|typescriptreact"),
        ("go", "go", &["go"], "go"),
        ("c", "c", &["c"], "c"),
        (
            "cpp",
            "cpp",
            &["cc", "cpp", "cxx", "hpp", "h"],
            "cpp|c\\+\\+",
        ),
        ("java", "java", &["java"], "java"),
        ("ruby", "ruby", &["rb", "rake", "gemspec"], "rb|ruby"),
        ("bash", "bash", &["sh", "bash", "zsh"], "sh|bash|shell|zsh"),
        ("toml", "toml", &["toml", "Cargo.lock"], "toml"),
        ("yaml", "yaml", &["yml", "yaml"], "yaml|yml"),
        ("json", "json", &["json", "jsonl"], "json"),
        (
            "markdown",
            "markdown",
            &["md", "markdown", "mdx"],
            "md|markdown",
        ),
        ("html", "html", &["html", "htm"], "html"),
        ("css", "css", &["css"], "css"),
        ("scss", "scss", &["scss", "sass"], "scss|sass"),
        ("lua", "lua", &["lua"], "lua"),
        ("zig", "zig", &["zig"], "zig"),
        ("haskell", "haskell", &["hs", "lhs"], "hs|haskell"),
        ("ocaml", "ocaml", &["ml", "mli"], "ml|ocaml"),
        ("nix", "nix", &["nix"], "nix"),
        ("scheme", "scheme", &["scm", "ss", "rkt"], "scheme|racket"),
        ("clojure", "clojure", &["clj", "cljs", "cljc"], "clojure"),
        ("elixir", "elixir", &["ex", "exs"], "elixir|ex"),
        ("erlang", "erlang", &["erl", "hrl"], "erlang"),
        ("php", "php", &["php"], "php"),
        ("swift", "swift", &["swift"], "swift"),
        ("kotlin", "kotlin", &["kt", "kts"], "kotlin"),
        ("scala", "scala", &["scala", "sc"], "scala"),
        ("c-sharp", "c_sharp", &["cs", "csx"], "c-?sharp"),
        ("dart", "dart", &["dart"], "dart"),
        ("sql", "sql", &["sql"], "sql"),
        ("dockerfile", "dockerfile", &["Dockerfile"], "dockerfile"),
        ("make", "make", &["Makefile", "makefile"], "make"),
        ("xml", "xml", &["xml", "svg"], "xml"),
        ("vue", "vue", &["vue"], "vue"),
        ("svelte", "svelte", &["svelte"], "svelte"),
        ("proto", "proto", &["proto"], "proto"),
    ];
    LANGUAGES
        .iter()
        .map(
            |(name, grammar, file_types, injection_pattern)| LanguageDefinition {
                name: (*name).to_owned(),
                grammar: (*grammar).to_owned(),
                file_types: file_types
                    .iter()
                    .map(|file_type| FileType::NameOrExtension((*file_type).to_owned()))
                    .collect(),
                injection_regex: injection_regex(injection_pattern),
            },
        )
        .collect()
}

#[cfg(test)]
mod tests {
    use std::{env, fs, path::PathBuf, sync::Arc};

    use tempfile::tempdir;

    use super::*;

    #[test]
    fn registry_parses_extensions_filenames_and_glob_suffixes() {
        let directory = tempdir().unwrap();
        let manifest = directory.path().join("languages.toml");
        fs::write(
            &manifest,
            r#"
[[language]]
name = "demo"
grammar = "demo-grammar"
injection-regex = "demo|alias"
file-types = ["demo", "ExactFile"]

[[language]]
name = "generated"
file-types = [{ glob = ".demo.in" }]
"#,
        )
        .unwrap();
        let registry = LanguageRegistry::from_definitions(parse_languages_toml(&manifest).unwrap());
        assert_eq!(
            registry
                .language_for_path(Path::new("src/file.demo"))
                .unwrap()
                .idx(),
            0
        );
        assert_eq!(
            registry
                .language_for_path(Path::new("ExactFile"))
                .unwrap()
                .idx(),
            0
        );
        assert_eq!(
            registry
                .language_for_path(Path::new("generated.demo.in"))
                .unwrap()
                .idx(),
            1
        );
        assert_eq!(registry.language_for_injection("alias").unwrap().idx(), 0);
        assert_eq!(registry.languages[0].grammar, "demo-grammar");
    }

    #[test]
    fn fallback_registry_handles_common_files() {
        let registry = LanguageRegistry::fallback();
        let rust = registry.language_for_path(Path::new("lib.rs")).unwrap();
        assert_eq!(registry.languages[rust.idx()].name, "rust");
        let docker = registry.language_for_path(Path::new("Dockerfile")).unwrap();
        assert_eq!(registry.languages[docker.idx()].name, "dockerfile");
    }

    #[test]
    fn engine_highlights_runtime_grammars_when_available() {
        let Some(runtime) = test_runtime() else {
            return;
        };
        let engine = HighlightEngine::with_runtime(runtime);
        let rust = "pub fn greeting(name: &str) -> String { format!(\"hi {name}\") }\n";
        let rust_spans = engine.highlight(Path::new("example.rs"), rust).unwrap();
        assert!(rust_spans.iter().any(|span| {
            &rust[span.byte_range.clone()] == "fn" && span.scope.as_ref() == "keyword.function"
        }));

        let markdown = "# title\n\n```rust\nfn fenced() {}\n```\n";
        let markdown_spans = engine.highlight(Path::new("example.md"), markdown).unwrap();
        assert!(markdown_spans.iter().any(|span| {
            &markdown[span.byte_range.clone()] == "fn" && span.scope.as_ref() == "keyword.function"
        }));

        let same = engine.highlight(Path::new("example.rs"), rust).unwrap();
        assert!(Arc::ptr_eq(&rust_spans, &same));
    }

    #[test]
    fn engine_returns_none_for_size_cap_and_unknown_language() {
        let Some(runtime) = test_runtime() else {
            return;
        };
        let engine = HighlightEngine::with_runtime(runtime);
        assert!(
            engine
                .highlight(Path::new("unknown.nope"), "anything")
                .is_none()
        );
        let oversized = "x".repeat(MAX_CONTENT_BYTES + 1);
        assert!(
            engine
                .highlight(Path::new("large.rs"), &oversized)
                .is_none()
        );
    }

    fn test_runtime() -> Option<PathBuf> {
        env::var_os("HELIX_RUNTIME")
            .map(PathBuf::from)
            .or_else(|| {
                env::var_os("HOME")
                    .map(|home| PathBuf::from(home).join("Development/helix/runtime"))
            })
            .filter(|runtime| runtime.join("grammars").is_dir())
    }
}
