#!/usr/bin/env bash
# Assemble the small Helix runtime needed by the highlighted-preview tests.
set -euo pipefail

# Keep this pin as the single source of truth for both the runtime queries and
# the grammar revisions read from Helix's languages.toml.
HELIX_REV="0522d519fd5227f77ecef387a87e51b732907562"

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <runtime-dir>" >&2
  exit 2
fi

target_dir="$1"
mkdir -p "$target_dir"
target_dir="$(cd "$target_dir" && pwd)"

work_dir="$(mktemp -d "${TMPDIR:-/tmp}/scooter-hx-runtime.XXXXXX")"
trap 'rm -rf "$work_dir"' EXIT

helix_dir="$work_dir/helix"
git init -q "$helix_dir"
# The pinned commit is on this fork's steel-event-system branch. Fetch its
# history with blobs filtered out, then sparse checkout only the runtime data.
git -C "$helix_dir" remote add origin https://github.com/mattwparas/helix.git
git -C "$helix_dir" fetch -q --depth 1 --filter=blob:none origin "$HELIX_REV"
git -C "$helix_dir" sparse-checkout init --no-cone
git -C "$helix_dir" sparse-checkout set \
  languages.toml \
  runtime/queries/rust \
  runtime/queries/c \
  runtime/queries/cpp \
  runtime/queries/markdown
git -C "$helix_dir" checkout -q --detach "$HELIX_REV"

stage_dir="$work_dir/runtime"
mkdir -p "$stage_dir/grammars" "$stage_dir/queries"

grammar_source() {
  local grammar="$1"
  local source
  source="$(awk -v grammar="$grammar" '
    /^\[\[grammar\]\]$/ { active = 0 }
    $0 == sprintf("name = %c%s%c", 34, grammar, 34) { active = 1; next }
    active && /^source = / { print; exit }
  ' "$helix_dir/languages.toml")"
  if [[ -z "$source" ]]; then
    echo "could not find grammar source for $grammar in Helix $HELIX_REV" >&2
    exit 1
  fi
  printf '%s\n' "$source"
}

build_grammar() {
  local grammar="$1"
  local source repo rev subpath checkout source_dir build_dir
  source="$(grammar_source "$grammar")"
  repo="$(printf '%s\n' "$source" | sed -n 's/.*git = "\([^"]*\)".*/\1/p')"
  rev="$(printf '%s\n' "$source" | sed -n 's/.*rev = "\([^"]*\)".*/\1/p')"
  subpath="$(printf '%s\n' "$source" | sed -n 's/.*subpath = "\([^"]*\)".*/\1/p')"
  if [[ -z "$repo" || -z "$rev" ]]; then
    echo "could not parse grammar source for $grammar: $source" >&2
    exit 1
  fi

  checkout="$work_dir/$grammar"
  git init -q "$checkout"
  git -C "$checkout" remote add origin "$repo"
  git -C "$checkout" fetch -q --depth=1 origin "$rev"
  git -C "$checkout" checkout -q --detach FETCH_HEAD

  source_dir="$checkout"
  if [[ -n "$subpath" ]]; then
    source_dir="$source_dir/$subpath"
  fi
  if [[ ! -f "$source_dir/src/parser.c" ]]; then
    echo "grammar $grammar at $repo@$rev has no src/parser.c" >&2
    exit 1
  fi

  build_dir="$work_dir/build-$grammar"
  mkdir -p "$build_dir"
  "${CC:-cc}" -fPIC -O2 -c "$source_dir/src/parser.c" -o "$build_dir/parser.o"
  if [[ -f "$source_dir/src/scanner.cc" ]]; then
    "${CXX:-c++}" -fPIC -O2 -c "$source_dir/src/scanner.cc" -o "$build_dir/scanner.o"
    "${CXX:-c++}" -shared "$build_dir/parser.o" "$build_dir/scanner.o" -o "$stage_dir/grammars/$grammar.so"
  elif [[ -f "$source_dir/src/scanner.c" ]]; then
    "${CC:-cc}" -fPIC -O2 -c "$source_dir/src/scanner.c" -o "$build_dir/scanner.o"
    "${CC:-cc}" -shared "$build_dir/parser.o" "$build_dir/scanner.o" -o "$stage_dir/grammars/$grammar.so"
  else
    "${CC:-cc}" -shared "$build_dir/parser.o" -o "$stage_dir/grammars/$grammar.so"
  fi
}

for grammar in rust c cpp markdown; do
  cp -R "$helix_dir/runtime/queries/$grammar" "$stage_dir/queries/"
  build_grammar "$grammar"
done

# Replace only the runtime pieces this script owns, leaving any other caller
# files in the target directory untouched. Re-running is therefore safe.
rm -rf "$target_dir/grammars" "$target_dir/queries"
mv "$stage_dir/grammars" "$stage_dir/queries" "$target_dir/"
echo "assembled Helix runtime $HELIX_REV in $target_dir"
