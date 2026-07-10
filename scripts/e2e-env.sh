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
export SEARCH_FIXTURE_DIR="$REPO/.dev/fixtures/search"
export HX_BINARY="$HOME/Development/helix/target/release/hx"

if [[ ! -d "$STEEL_HOME/cogs/helix" ]]; then
  printf '%s\n' "Missing generated Helix Steel modules at $STEEL_HOME/cogs/helix." >&2
  printf '%s\n' "Run: STEEL_HOME=$REPO/.dev/steel-home cargo xtask code-gen" >&2
  printf '%s\n' "from the Helix checkout, then rerun this script." >&2
  return 1 2>/dev/null || exit 1
fi

mkdir -p "$XDG_CONFIG_HOME/helix" "$XDG_CACHE_HOME" "$FIXTURE_DIR" "$SEARCH_FIXTURE_DIR"

printf '(require "%s")\n' "$REPO/scooter.scm" > "$XDG_CONFIG_HOME/helix/init.scm"
printf '%s\n' 'alpha: first fixture line' 'alpha: second fixture line' > "$FIXTURE_DIR/alpha.txt"
printf '%s\n' 'bravo: a separate fixture' 'bravo: another fixture line' > "$FIXTURE_DIR/bravo.txt"
printf '%s\n' '# Scooter S1 fixture' 'static, deterministic content' > "$FIXTURE_DIR/README.md"

printf '%s\n' 'alpha one' 'alphabet one' > "$SEARCH_FIXTURE_DIR/one.txt"
printf '%s\n' 'alpha two' 'alphabet two' > "$SEARCH_FIXTURE_DIR/two.txt"
printf '%s\n' 'alpha three' > "$SEARCH_FIXTURE_DIR/three.txt"

e2e_capture_pane() {
  tmux -L "$TMUX_SOCKET" capture-pane -p -t "$PANE_TARGET" -S - 2>/dev/null || true
}

e2e_print_diagnostics() {
  printf '%s\n' '--- tmux pane ---' >&2
  e2e_capture_pane >&2
  printf '%s\n' '--- Helix log ---' >&2
  if [[ -f "$XDG_CACHE_HOME/helix/helix.log" ]]; then
    sed -n '1,240p' "$XDG_CACHE_HOME/helix/helix.log" >&2
  else
    printf '%s\n' '(no Helix log found)' >&2
  fi
}

e2e_fail() {
  printf '%s\n' "e2e test failed: $*" >&2
  e2e_print_diagnostics
  exit 1
}

e2e_cleanup() {
  tmux -L "$TMUX_SOCKET" kill-session -t "$TMUX_SESSION" 2>/dev/null || true
}

e2e_wait_for_present() {
  local needle="$1"
  local deadline=$((SECONDS + 15))

  while (( SECONDS < deadline )); do
    if [[ "$(e2e_capture_pane)" == *"$needle"* ]]; then
      return 0
    fi
    sleep 0.2
  done

  e2e_fail "timed out waiting for '$needle'"
}

e2e_wait_for_absent() {
  local needle="$1"
  local deadline=$((SECONDS + 10))

  while (( SECONDS < deadline )); do
    if [[ "$(e2e_capture_pane)" != *"$needle"* ]]; then
      return 0
    fi
    sleep 0.2
  done

  e2e_fail "timed out waiting for '$needle' to disappear"
}

e2e_wait_for_helix() {
  local deadline=$((SECONDS + 15))

  while (( SECONDS < deadline )); do
    if [[ "$(tmux -L "$TMUX_SOCKET" display-message -p -t "$PANE_TARGET" '#{pane_current_command}' 2>/dev/null || true)" == hx ]]; then
      return 0
    fi
    sleep 0.2
  done

  e2e_fail 'timed out waiting for Helix to start'
}
