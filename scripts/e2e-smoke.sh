#!/usr/bin/env bash
# Exercise the installed dylib in a real, isolated Helix session.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=e2e-env.sh
source "$SCRIPT_DIR/e2e-env.sh"

TMUX_SOCKET="scooter-s1-${PPID}-${RANDOM}"
TMUX_SESSION="scooter-s1"
PANE_TARGET="$TMUX_SESSION:0.0"

capture_pane() {
  tmux -L "$TMUX_SOCKET" capture-pane -p -t "$PANE_TARGET" -S - 2>/dev/null || true
}

print_diagnostics() {
  printf '%s\n' '--- tmux pane ---' >&2
  capture_pane >&2
  printf '%s\n' '--- Helix log ---' >&2
  if [[ -f "$XDG_CACHE_HOME/helix/helix.log" ]]; then
    sed -n '1,240p' "$XDG_CACHE_HOME/helix/helix.log" >&2
  else
    printf '%s\n' '(no Helix log found)' >&2
  fi
}

fail() {
  printf '%s\n' "e2e smoke test failed: $*" >&2
  print_diagnostics
  exit 1
}

cleanup() {
  tmux -L "$TMUX_SOCKET" kill-session -t "$TMUX_SESSION" 2>/dev/null || true
}
trap cleanup EXIT INT TERM

wait_for_present() {
  local needle="$1"
  local deadline=$((SECONDS + 15))

  while (( SECONDS < deadline )); do
    if [[ "$(capture_pane)" == *"$needle"* ]]; then
      return 0
    fi
    sleep 0.2
  done

  fail "timed out waiting for '$needle'"
}

wait_for_absent() {
  local needle="$1"
  local deadline=$((SECONDS + 10))

  while (( SECONDS < deadline )); do
    if [[ "$(capture_pane)" != *"$needle"* ]]; then
      return 0
    fi
    sleep 0.2
  done

  fail "timed out waiting for '$needle' to disappear"
}

wait_for_helix() {
  local deadline=$((SECONDS + 15))

  while (( SECONDS < deadline )); do
    if [[ "$(tmux -L "$TMUX_SOCKET" display-message -p -t "$PANE_TARGET" '#{pane_current_command}' 2>/dev/null || true)" == hx ]]; then
      return 0
    fi
    sleep 0.2
  done

  fail 'timed out waiting for Helix to start'
}

if [[ ! -x "$HX_BINARY" ]]; then
  fail "Helix test binary is not executable: $HX_BINARY"
fi

if ! command -v tmux >/dev/null; then
  fail 'tmux is required'
fi

if ! STEEL_HOME="$STEEL_HOME" cargo steel-lib; then
  fail 'cargo steel-lib could not build and install the dylib'
fi

tmux -L "$TMUX_SOCKET" new-session -d -x 120 -y 40 -s "$TMUX_SESSION" -c "$FIXTURE_DIR" "$HX_BINARY alpha.txt" \
  || fail 'could not start the Helix tmux session'

wait_for_helix
wait_for_present 'alpha: first fixture line'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter' Enter
wait_for_present 'S1 TOOLCHAIN SPIKE'
wait_for_present 'STATIC FRAME READY'

tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Escape
wait_for_absent 'S1 TOOLCHAIN SPIKE'
