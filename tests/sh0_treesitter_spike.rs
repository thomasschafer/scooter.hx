//! SH0 is deliberately an integration test: tree-house stays out of the dylib.

use std::{
    collections::HashMap,
    env,
    fmt::Write as _,
    fs,
    ops::Range,
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

use ropey::Rope;
use tree_house::{
    highlighter::{Highlight, Highlighter},
    read_query, InjectionLanguageMarker, Language, LanguageConfig, LanguageLoader, Syntax,
};
use tree_house_bindings::Grammar;

const PARSE_TIMEOUT: Duration = Duration::from_secs(1);

#[derive(Debug, PartialEq, Eq)]
struct Span {
    bytes: Range<usize>,
    scope: String,
}

struct RuntimeLoader {
    languages: HashMap<String, Language>,
    configs: Vec<LanguageConfig>,
    scopes: Vec<String>,
}

impl RuntimeLoader {
    fn new(runtime: &Path, names: &[&str]) -> Self {
        let languages = names
            .iter()
            .enumerate()
            .map(|(index, name)| {
                (
                    (*name).to_owned(),
                    Language::new(u32::try_from(index).expect("fewer than u32::MAX languages")),
                )
            })
            .collect();
        let mut scopes = Vec::new();
        let configs = names
            .iter()
            .map(|name| {
                let grammar_path = grammar_path(runtime, name);
                // SAFETY: Helix's grammar directory contains tree-sitter grammar dylibs.
                let grammar =
                    unsafe { Grammar::new(name, &grammar_path) }.unwrap_or_else(|error| {
                        panic!("load {name} from {}: {error}", grammar_path.display())
                    });
                eprintln!(
                    "SH0 loaded {name} from {} (tree-sitter ABI {})",
                    grammar_path.display(),
                    grammar.abi_version()
                );
                let highlights = runtime_query(runtime, name, "highlights.scm");
                let injections = runtime_query(runtime, name, "injections.scm");
                let locals = runtime_query(runtime, name, "locals.scm");
                let config = LanguageConfig::new(grammar, &highlights, &injections, &locals)
                    .unwrap_or_else(|error| panic!("compile {name} queries: {error}"));
                config.configure(|scope| {
                    let index = if let Some(index) = scopes.iter().position(|known| known == scope)
                    {
                        index
                    } else {
                        scopes.push(scope.to_owned());
                        scopes.len() - 1
                    };
                    Some(Highlight::new(
                        u32::try_from(index).expect("fewer than u32::MAX scopes"),
                    ))
                });
                config
            })
            .collect();

        Self {
            languages,
            configs,
            scopes,
        }
    }

    fn language(&self, name: &str) -> Language {
        self.languages[name]
    }
}

impl LanguageLoader for RuntimeLoader {
    fn language_for_marker(&self, marker: InjectionLanguageMarker<'_>) -> Option<Language> {
        match marker {
            InjectionLanguageMarker::Name(name) => self.languages.get(name).copied(),
            InjectionLanguageMarker::Match(text) => self.languages.get(&text.to_string()).copied(),
            InjectionLanguageMarker::Filename(_) | InjectionLanguageMarker::Shebang(_) => None,
        }
    }

    fn get_config(&self, language: Language) -> Option<&LanguageConfig> {
        self.configs.get(language.idx())
    }
}

fn runtime_dir() -> Option<PathBuf> {
    env::var_os("HELIX_RUNTIME")
        .map(PathBuf::from)
        .or_else(|| {
            env::var_os("HOME").map(|home| PathBuf::from(home).join("Development/helix/runtime"))
        })
        .filter(|path| path.join("grammars").is_dir() && path.join("queries").is_dir())
}

fn grammar_path(runtime: &Path, language: &str) -> PathBuf {
    let grammars = runtime.join("grammars");
    ["dylib", "so"]
        .into_iter()
        .map(|extension| grammars.join(format!("{language}.{extension}")))
        .find(|path| path.is_file())
        .unwrap_or_else(|| panic!("no grammar for {language} in {}", grammars.display()))
}

fn runtime_query(runtime: &Path, language: &str, query: &str) -> String {
    read_query(language, |name| {
        fs::read_to_string(runtime.join("queries").join(name).join(query)).unwrap_or_default()
    })
}

fn highlight(loader: &RuntimeLoader, language: &str, source: &str) -> Vec<Span> {
    let source = Rope::from_str(source);
    let syntax = Syntax::new(
        source.slice(..),
        loader.language(language),
        PARSE_TIMEOUT,
        loader,
    )
    .expect("parse source");
    let mut highlighter = Highlighter::new(&syntax, source.slice(..), loader, 0..);
    let mut spans = Vec::new();
    let mut position = highlighter.next_event_offset();
    let source_len = u32::try_from(source.len_bytes()).expect("source fits tree-sitter byte range");

    while position != u32::MAX {
        let _ = highlighter.advance();
        let next = highlighter.next_event_offset().min(source_len);
        if position < next
            && let Some(highlight) = highlighter.active_highlights().next_back()
        {
            spans.push(Span {
                bytes: usize::try_from(position).expect("u32 fits usize")
                    ..usize::try_from(next).expect("u32 fits usize"),
                scope: loader.scopes[highlight.idx()].clone(),
            });
        }
        position = highlighter.next_event_offset();
    }

    spans
}

fn scopes_for_text(source: &str, spans: &[Span]) -> Vec<(String, String)> {
    spans
        .iter()
        .map(|span| (source[span.bytes.clone()].to_owned(), span.scope.clone()))
        .collect()
}

#[test]
fn shipped_helix_grammars_highlight_and_inject() {
    let Some(runtime) = runtime_dir() else {
        eprintln!("SH0 skipped: set HELIX_RUNTIME to a Helix runtime to exercise shipped grammars");
        return;
    };
    let loader = RuntimeLoader::new(&runtime, &["rust", "c", "cpp", "markdown"]);

    let cpp_query = runtime_query(&runtime, "cpp", "highlights.scm");
    assert!(cpp_query.contains("\"return\" @keyword.control.return"));

    let rust = "pub fn greeting(name: &str) -> String { format!(\"hi {name}\") }\n";
    let cpp = "int main() { return 42; }\n";
    let rust_spans = highlight(&loader, "rust", rust);
    let cpp_spans = highlight(&loader, "cpp", cpp);
    eprintln!("SH0 rust spans: {:?}", scopes_for_text(rust, &rust_spans));
    eprintln!("SH0 cpp spans: {:?}", scopes_for_text(cpp, &cpp_spans));

    assert!(scopes_for_text(rust, &rust_spans)
        .iter()
        .any(|(text, scope)| text == "fn" && scope == "keyword.function"));
    assert!(scopes_for_text(cpp, &cpp_spans)
        .iter()
        .any(|(text, scope)| text == "return" && scope == "keyword.control.return"));

    let markdown = "# title\n\n```rust\nfn fenced() {}\n```\n";
    let fenced_start =
        u32::try_from(markdown.find("fn fenced").unwrap()).expect("byte offset fits");
    let fenced_end = fenced_start + u32::try_from("fn fenced".len()).expect("length fits");
    let source = Rope::from_str(markdown);
    let syntax = Syntax::new(
        source.slice(..),
        loader.language("markdown"),
        PARSE_TIMEOUT,
        &loader,
    )
    .expect("parse markdown with injections");
    let layers: Vec<_> = syntax
        .layers_for_byte_range(fenced_start, fenced_end)
        .map(|layer| syntax.layer(layer).language)
        .collect();
    assert_eq!(
        layers,
        vec![loader.language("markdown"), loader.language("rust")]
    );
    let markdown_spans = highlight(&loader, "markdown", markdown);
    eprintln!(
        "SH0 markdown spans: {:?}",
        scopes_for_text(markdown, &markdown_spans)
    );
    assert!(scopes_for_text(markdown, &markdown_spans)
        .iter()
        .any(|(text, scope)| text == "fn" && scope == "keyword.function"));

    let mut large_rust = String::with_capacity(100_000);
    for line in 0..2_000 {
        writeln!(
            large_rust,
            "pub fn line_{line}() {{ let text = \"value\"; }}"
        )
        .expect("write to String");
    }
    let start = Instant::now();
    let large_spans = highlight(&loader, "rust", &large_rust);
    let elapsed = start.elapsed();
    eprintln!(
        "SH0 2,000-line Rust highlight: {elapsed:?} ({} spans)",
        large_spans.len()
    );
    assert!(!large_spans.is_empty());
}
