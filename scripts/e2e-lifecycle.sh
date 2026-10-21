#!/usr/bin/env bash
# Exercise hiding/resuming a live search plus reset and prompt session teardown.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=e2e-env.sh
source "$SCRIPT_DIR/e2e-env.sh"

TMUX_SOCKET="scooter-e6-lifecycle-${PPID}-${RANDOM}"
TMUX_SESSION="scooter-e6-lifecycle"
PANE_TARGET="$TMUX_SESSION:0.0"
trap e2e_cleanup EXIT INT TERM

if ! command -v tmux >/dev/null; then
  e2e_fail 'tmux is required'
fi

if ! STEEL_HOME="$STEEL_HOME" cargo steel-lib; then
  e2e_fail 'cargo steel-lib could not build and install the dylib'
fi

rm -rf "$LIFECYCLE_FIXTURE_DIR"
mkdir -p "$LIFECYCLE_FIXTURE_DIR"
# The search must stay in progress long enough for the 0.2s pane polling to
# observe "Still searching..."; 2,000 files can finish in under that.
LIFECYCLE_FILE_COUNT=20000
for ((index = 1; index <= LIFECYCLE_FILE_COUNT; index++)); do
  printf 'lifecycle-match %04d\n' "$index" > "$LIFECYCLE_FIXTURE_DIR/file-${index}.txt"
done

tmux -L "$TMUX_SOCKET" new-session -d -x 120 -y 40 -s "$TMUX_SESSION" \
  -c "$LIFECYCLE_FIXTURE_DIR" "$HX_BINARY" file-1.txt \
  || e2e_fail 'could not start the Helix tmux session'

e2e_wait_for_helix
e2e_wait_for_present 'lifecycle-match 0001'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter' Enter
e2e_wait_for_present 'Search text'

tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" 'lifecycle-match'
# Let the debounced task begin before hiding. The poll loop must then stop
# while the worker continues independently of the hidden component.
e2e_wait_for_present 'Still searching...'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Escape
e2e_wait_for_absent 'Search text'

tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter' Enter
e2e_wait_for_present "Results: $LIFECYCLE_FILE_COUNT [Search complete]"

tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" C-r
e2e_wait_for_present 'Results: 0 [Search is empty]'

tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" 'lifecycle-match'
e2e_wait_for_present 'Still searching...'
start_seconds=$SECONDS
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" C-c
e2e_wait_for_absent 'Search text'
if (( SECONDS - start_seconds > 1 )); then
  e2e_fail 'C-c did not close the in-flight session promptly'
fi

tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter' Enter
e2e_wait_for_present 'Search text'
e2e_wait_for_present 'Results: 0 [Search is empty]'
