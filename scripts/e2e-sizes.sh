#!/usr/bin/env bash
# Exercise the fields renderer at the terminal sizes used for E3 review.

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

  TMUX_SOCKET="scooter-e3-${terminal_width}x${terminal_height}-${PPID}-${RANDOM}"
  TMUX_SESSION="scooter-e3-${terminal_width}x${terminal_height}"
  PANE_TARGET="$TMUX_SESSION:0.0"

  tmux -L "$TMUX_SOCKET" new-session -d -x "$terminal_width" -y "$terminal_height" \
    -s "$TMUX_SESSION" -c "$SEARCH_FIXTURE_DIR" "$HX_BINARY README.md" \
    || e2e_fail "could not start the ${terminal_width}x${terminal_height} Helix tmux session"

  e2e_wait_for_helix
  e2e_wait_for_present 'static, deterministic content'
  tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter' Enter
  e2e_wait_for_present 'Search text'
  tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" 'alpha'
  e2e_wait_for_present 'alpha'

  if [[ "$terminal_width" == 80 && "$terminal_height" == 24 ]]; then
    # The 19-row popup content fits six whole fields plus the one-row gap;
    # there is no row left for the banner. Check the field survives instead.
    if [[ "$(e2e_capture_pane)" == *'Results:'* ]]; then
      e2e_fail '80x24 unexpectedly rendered a banner without room for it'
    fi
  else
    e2e_wait_for_present 'Results: 5 [Search complete]'
  fi

  e2e_capture_pane > "$CAPTURE_DIR/${terminal_width}x${terminal_height}.txt"
  e2e_cleanup
}

trap e2e_cleanup EXIT INT TERM

run_size 80 24
run_size 120 40
run_size 220 55
