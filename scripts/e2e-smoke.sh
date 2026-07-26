#!/usr/bin/env bash
# Exercise the installed dylib in a real, isolated Helix session.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=e2e-env.sh
source "$SCRIPT_DIR/e2e-env.sh"

TMUX_SOCKET="scooter-s1-${PPID}-${RANDOM}"
TMUX_SESSION="scooter-s1"
PANE_TARGET="$TMUX_SESSION:0.0"

trap e2e_cleanup EXIT INT TERM

if [[ ! -x "$HX_BINARY" ]]; then
  e2e_fail "Helix test binary is not executable: $HX_BINARY"
fi

if ! command -v tmux >/dev/null; then
  e2e_fail 'tmux is required'
fi

if ! STEEL_HOME="$STEEL_HOME" cargo steel-lib; then
  e2e_fail 'cargo steel-lib could not build and install the dylib'
fi

tmux -L "$TMUX_SOCKET" new-session -d -x 120 -y 40 -s "$TMUX_SESSION" -c "$FIXTURE_DIR" "$HX_BINARY alpha.txt" \
  || e2e_fail 'could not start the Helix tmux session'

e2e_wait_for_helix
e2e_wait_for_present 'alpha: first fixture line'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter' Enter
e2e_wait_for_present 'Search text'
e2e_wait_for_present 'scooter'

tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Escape
e2e_wait_for_absent 'Search text'

tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter' Enter
e2e_wait_for_present 'Search text'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" M-h
e2e_wait_for_absent 'Search text'
