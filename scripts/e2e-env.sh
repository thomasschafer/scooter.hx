#!/usr/bin/env bash
# Shared, isolated environment for scooter.hx end-to-end checks.

set -euo pipefail

REPO="$(git rev-parse --show-toplevel)"
export REPO

export STEEL_HOME="$REPO/.dev/steel-home"
export XDG_CONFIG_HOME="$REPO/.dev/config"
export XDG_CACHE_HOME="$REPO/.dev/cache"
export HELIX_RUNTIME="$HOME/Development/helix/runtime"
export FIXTURE_DIR="$REPO/.dev/fixtures/basic"
export HX_BINARY="$HOME/Development/helix/target/release/hx"

if [[ ! -d "$STEEL_HOME/cogs/helix" ]]; then
  printf '%s\n' "Missing generated Helix Steel modules at $STEEL_HOME/cogs/helix." >&2
  printf '%s\n' "Run: STEEL_HOME=$REPO/.dev/steel-home cargo xtask code-gen" >&2
  printf '%s\n' "from the Helix checkout, then rerun this script." >&2
  return 1 2>/dev/null || exit 1
fi

mkdir -p "$XDG_CONFIG_HOME/helix" "$XDG_CACHE_HOME" "$FIXTURE_DIR"

printf '(require "%s")\n' "$REPO/scooter.scm" > "$XDG_CONFIG_HOME/helix/init.scm"
printf '%s\n' 'alpha: first fixture line' 'alpha: second fixture line' > "$FIXTURE_DIR/alpha.txt"
printf '%s\n' 'bravo: a separate fixture' 'bravo: another fixture line' > "$FIXTURE_DIR/bravo.txt"
printf '%s\n' '# Scooter S1 fixture' 'static, deterministic content' > "$FIXTURE_DIR/README.md"
