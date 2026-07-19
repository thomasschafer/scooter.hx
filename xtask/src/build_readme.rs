use std::{
    collections::HashMap,
    fmt::Write,
    fs,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result, bail};
use cargo_metadata::MetadataCommand;
use scooter_core::config::KeysConfig;
use scooter_hx::docs::{option_specs, plugin_key_specs};
use syn::{Attribute, Fields, Item, ItemStruct, Meta, parse_file};

const TOC_START: &str = "<!-- TOC START -->";
const TOC_END: &str = "<!-- TOC END -->";
const CONFIG_START: &str = "<!-- CONFIG START -->";
const CONFIG_END: &str = "<!-- CONFIG END -->";
const KEYS_START: &str = "<!-- KEYS START -->";
const KEYS_END: &str = "<!-- KEYS END -->";
const CONTENTS_HEADING: &str = "## Contents";

pub fn generate_readme(readme_path: &Path, check_only: bool) -> Result<()> {
    // Normalise CRLF so a Windows checkout with git autocrlf compares equal
    // to the LF content this generator produces.
    let original = fs::read_to_string(readme_path)
        .with_context(|| format!("failed to read {}", readme_path.display()))?
        .replace("\r\n", "\n");
    let with_toc = generate_toc(&original)?;
    let with_config = replace_section(&with_toc, CONFIG_START, CONFIG_END, &config_docs())?;
    let updated = replace_section(&with_config, KEYS_START, KEYS_END, &keys_docs()?)?;

    if updated == original {
        println!("README is up to date");
        return Ok(());
    }
    if check_only {
        bail!("README is out of date; run `cargo xtask readme`");
    }
    fs::write(readme_path, updated)
        .with_context(|| format!("failed to write {}", readme_path.display()))?;
    println!("updated {}", readme_path.display());
    Ok(())
}

fn generate_toc(content: &str) -> Result<String> {
    let mut toc = String::new();
    let mut in_code_block = false;
    let mut in_toc = false;
    for line in content.lines() {
        if line.starts_with("```") || line.starts_with("~~~") {
            in_code_block = !in_code_block;
            continue;
        }
        if in_code_block {
            continue;
        }
        match line {
            TOC_START => in_toc = true,
            TOC_END => in_toc = false,
            CONTENTS_HEADING if !in_toc => {}
            _ if in_toc => {}
            _ if line.starts_with("## ") => {
                let title = &line[3..];
                writeln!(toc, "- [{title}](#{})", anchor(title))?;
            }
            _ => {}
        }
    }
    replace_section(content, TOC_START, TOC_END, &toc)
}

fn anchor(title: &str) -> String {
    title
        .chars()
        .filter_map(|character| {
            if character.is_alphanumeric() {
                Some(character.to_lowercase().next().expect("character exists"))
            } else if character.is_whitespace() {
                Some('-')
            } else {
                None
            }
        })
        .collect()
}

fn config_docs() -> String {
    let mut docs =
        String::from("| Setting | Value | Default | Effect |\n| --- | --- | --- | --- |\n");
    for option in option_specs() {
        let _ = writeln!(
            docs,
            "| `{}` | {} | {} | {} |",
            option.symbol, option.value_type, option.default, option.description
        );
    }
    docs
}

fn keys_docs() -> Result<String> {
    let descriptions = key_descriptions(&scooter_keys_source()?)?;
    let defaults = toml::Value::try_from(KeysConfig::default())?;
    let mut bindings = Vec::new();
    collect_key_defaults(&defaults, "", &mut bindings)?;
    let mut rows = bindings
        .into_iter()
        .map(|(path, keys)| {
            key_description(&path, &descriptions)
                .map(|description| (path, keys, description.to_string()))
        })
        .collect::<Result<Vec<_>>>()?;
    rows.extend(plugin_key_specs().iter().map(|key| {
        (
            key.path.to_string(),
            key.default.to_string(),
            key.description.to_string(),
        )
    }));
    rows.sort_by(|left, right| left.0.cmp(&right.0));

    let mut docs = format!(
        "Defaults from scooter-core {}, matching the Scooter TUI, plus Scooter's plugin bindings.\n\n| Binding path | Default key(s) | Description |\n| --- | --- | --- |\n",
        scooter_core_version()?
    );
    for (path, keys, description) in rows {
        writeln!(docs, "| `{path}` | `{keys}` | {description} |")?;
    }
    Ok(docs)
}

fn scooter_core_version() -> Result<String> {
    let metadata = MetadataCommand::new().exec()?;
    metadata
        .packages
        .iter()
        .find(|package| package.name == "scooter-core" && package.source.is_some())
        .map(|package| package.version.to_string())
        .context("could not locate scooter-core registry dependency in cargo metadata")
}

fn key_description<'a>(path: &str, descriptions: &'a HashMap<String, String>) -> Result<&'a str> {
    match path {
        "search.fields.unlock_prepopulated_fields" => {
            Ok("Allow editing of prepopulated search fields.")
        }
        "search.results.open_in_editor" => {
            Ok("Open the selected result in Helix and hide Scooter.")
        }
        _ => descriptions
            .get(path)
            .map(String::as_str)
            .with_context(|| format!("no doc comment for scooter-core key path '{path}'")),
    }
}

fn scooter_keys_source() -> Result<PathBuf> {
    let metadata = MetadataCommand::new().exec()?;
    let package = metadata
        .packages
        .iter()
        .find(|package| package.name == "scooter-core" && package.source.is_some())
        .context("could not locate scooter-core registry dependency in cargo metadata")?;
    let manifest_dir = package
        .manifest_path
        .parent()
        .context("scooter-core manifest has no parent")?;
    let keys = manifest_dir.join("src/config/keys.rs");
    keys.exists()
        .then(|| keys.into_std_path_buf())
        .context("cargo metadata located scooter-core, but src/config/keys.rs is missing")
}

fn key_descriptions(source_path: &Path) -> Result<HashMap<String, String>> {
    let source = fs::read_to_string(source_path)
        .with_context(|| format!("failed to read {}", source_path.display()))?;
    let parsed = parse_file(&source)
        .with_context(|| format!("failed to parse {}", source_path.display()))?;
    let structs: HashMap<String, ItemStruct> = parsed
        .items
        .iter()
        .filter_map(|item| match item {
            Item::Struct(item) => Some((item.ident.to_string(), item.clone())),
            _ => None,
        })
        .collect();
    let mut descriptions = HashMap::new();
    collect_struct_descriptions(
        structs
            .get("KeysConfig")
            .context("KeysConfig missing from keys.rs")?,
        &structs,
        "",
        &mut descriptions,
    );
    Ok(descriptions)
}

fn collect_struct_descriptions(
    item: &ItemStruct,
    structs: &HashMap<String, ItemStruct>,
    prefix: &str,
    descriptions: &mut HashMap<String, String>,
) {
    let Fields::Named(fields) = &item.fields else {
        return;
    };
    for field in &fields.named {
        let Some(ident) = &field.ident else { continue };
        let path = if prefix.is_empty() {
            ident.to_string()
        } else {
            format!("{prefix}.{ident}")
        };
        let type_name = match &field.ty {
            syn::Type::Path(path) => path
                .path
                .segments
                .last()
                .map(|segment| segment.ident.to_string()),
            _ => None,
        };
        if let Some(nested) = type_name
            .and_then(|name| structs.get(&name))
            .filter(|nested| matches!(nested.fields, Fields::Named(_)))
        {
            collect_struct_descriptions(nested, structs, &path, descriptions);
        } else {
            descriptions.insert(path, doc_comment(&field.attrs));
        }
    }
}

fn doc_comment(attributes: &[Attribute]) -> String {
    let mut lines = Vec::new();
    for attribute in attributes {
        if attribute.path().is_ident("doc")
            && let Meta::NameValue(meta) = &attribute.meta
            && let syn::Expr::Lit(literal) = &meta.value
            && let syn::Lit::Str(text) = &literal.lit
        {
            lines.push(text.value().trim().to_string());
        }
    }
    lines.join(" ")
}

fn collect_key_defaults(
    value: &toml::Value,
    prefix: &str,
    bindings: &mut Vec<(String, String)>,
) -> Result<()> {
    let table = value
        .as_table()
        .context("KeysConfig did not serialize as a TOML table")?;
    for (name, value) in table {
        let path = if prefix.is_empty() {
            name.to_string()
        } else {
            format!("{prefix}.{name}")
        };
        if value.is_table() {
            collect_key_defaults(value, &path, bindings)?;
        } else {
            let keys = match value {
                toml::Value::String(key) => key.clone(),
                toml::Value::Array(keys) => keys
                    .iter()
                    .map(toml::Value::as_str)
                    .collect::<Option<Vec<_>>>()
                    .context("key binding array contained a non-string")?
                    .join("`, `"),
                _ => bail!("key binding '{path}' did not serialize as a string or string array"),
            };
            bindings.push((path, keys));
        }
    }
    Ok(())
}

fn replace_section(content: &str, start: &str, end: &str, replacement: &str) -> Result<String> {
    let start_at = content
        .find(start)
        .with_context(|| format!("missing README marker {start}"))?;
    let after_start = start_at + start.len();
    let end_relative = content[after_start..]
        .find(end)
        .with_context(|| format!("missing README marker {end}"))?;
    let end_at = after_start + end_relative;
    Ok(format!(
        "{}\n{}{}",
        &content[..after_start],
        replacement,
        &content[end_at..]
    ))
}
