#!/usr/bin/env bash
# Exercise the native results list and preview pane at both layout breakpoints.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=e2e-env.sh
source "$SCRIPT_DIR/e2e-env.sh"

CAPTURE_DIR="$REPO/.dev/e2e-captures"
mkdir -p "$CAPTURE_DIR"

TMUX_SOCKET=""
TMUX_SESSION=""
PANE_TARGET=""

if [[ ! -x "$HX_BINARY" ]]; then
  e2e_fail "Helix test binary is not executable: $HX_BINARY"
fi

if ! command -v tmux >/dev/null; then
  e2e_fail 'tmux is required'
fi

if ! STEEL_HOME="$STEEL_HOME" cargo steel-lib; then
  e2e_fail 'cargo steel-lib could not build and install the dylib'
fi

run_size() {
  local terminal_width="$1"
  local terminal_height="$2"

  TMUX_SOCKET="scooter-e4-${terminal_width}x${terminal_height}-${PPID}-${RANDOM}"
  TMUX_SESSION="scooter-e4-${terminal_width}x${terminal_height}"
  PANE_TARGET="$TMUX_SESSION:0.0"

  tmux -L "$TMUX_SOCKET" new-session -d -x "$terminal_width" -y "$terminal_height" \
    -s "$TMUX_SESSION" -c "$PREVIEW_FIXTURE_DIR" "$HX_BINARY" preview.txt \
    || e2e_fail "could not start the ${terminal_width}x${terminal_height} Helix tmux session"

  e2e_wait_for_helix
  e2e_wait_for_present 'preview context before first result'
  tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter' Enter
  e2e_wait_for_present 'Search text'
  tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" 'alpha'
  e2e_wait_for_present 'Results: 2 [Search complete]'

  tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Tab
  tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" 'OMEGA'
  tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Enter
  e2e_wait_for_present '(1)'
  if [[ "$(e2e_capture_pane)" == *'(1) preview context'* ]]; then
    e2e_fail 'preview context unexpectedly contains a line number'
  fi
  e2e_wait_for_present 'preview context before first result'
  e2e_wait_for_present '- alpha first result'
  e2e_wait_for_present '+ OMEGA first result'

  tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" j
  e2e_wait_for_present '+ OMEGA second result'
  tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Space
  e2e_wait_for_present '[ ] preview.txt:4'

  e2e_capture_pane > "$CAPTURE_DIR/preview-${terminal_width}x${terminal_height}.txt"
  e2e_cleanup
}

trap e2e_cleanup EXIT INT TERM

run_size 160 45
run_size 100 30
