#!/usr/bin/env bash
# Exercise foreground/background result opens and non-dirty buffer reloads.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=e2e-env.sh
source "$SCRIPT_DIR/e2e-env.sh"

OPEN_FIXTURE_DIR="$(mktemp -d "$REPO/.dev/fixtures/open.XXXXXX")"
TMUX_SOCKET="scooter-h3-open-${PPID}-${RANDOM}"
TMUX_SESSION="scooter-h3-open"
PANE_TARGET="$TMUX_SESSION:0.0"

cleanup() {
  e2e_cleanup
  rm -rf "$OPEN_FIXTURE_DIR"
}
trap cleanup EXIT INT TERM

if [[ ! -x "$HX_BINARY" ]]; then
  e2e_fail "Helix test binary is not executable: $HX_BINARY"
fi

if ! command -v tmux >/dev/null; then
  e2e_fail 'tmux is required'
fi

if ! STEEL_HOME="$STEEL_HOME" cargo steel-lib; then
  e2e_fail 'cargo steel-lib could not build and install the dylib'
fi

printf '%s\n' 'initial editor buffer' > "$OPEN_FIXTURE_DIR/seed.txt"
printf '%s\n' \
  'first context' \
  'alpha first target' \
  'first trailing context' > "$OPEN_FIXTURE_DIR/first.txt"
printf '%s\n' \
  'second context' \
  'alpha second target' \
  'second trailing context' > "$OPEN_FIXTURE_DIR/second.txt"

tmux -L "$TMUX_SOCKET" new-session -d -x 120 -y 40 -s "$TMUX_SESSION" \
  -c "$OPEN_FIXTURE_DIR" "$HX_BINARY" seed.txt \
  || e2e_fail 'could not start the Helix tmux session'

e2e_wait_for_helix
e2e_wait_for_present 'initial editor buffer'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter' Enter
e2e_wait_for_present 'Search text'

tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" alpha
e2e_wait_for_present 'Results: 2 [Search complete]'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Enter g

# Foreground open closes the popup but keeps the result session in memory.
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" e
e2e_wait_for_absent 'Search text'
e2e_wait_for_present 'alpha '
foreground_capture="$(e2e_capture_pane)"
if [[ "$foreground_capture" == *first.txt* ]]; then
  foreground_file='first.txt'
  background_file='second.txt'
elif [[ "$foreground_capture" == *second.txt* ]]; then
  foreground_file='second.txt'
  background_file='first.txt'
else
  e2e_fail 'foreground open did not show a result filename in the statusline'
fi

tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter' Enter
e2e_wait_for_present 'Results: 2 [Search complete]'

# The standalone default background binding is Alt-o.
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" j M-o
e2e_wait_for_present 'Search text'

# Results-focus Escape returns to the fields; a second Escape follows the
# normal fields-focus hide path. The background-opened buffer is then visible.
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Escape
e2e_wait_for_present 'Results: 2 [Search complete]'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Escape
e2e_wait_for_absent 'Search text'
e2e_wait_for_present 'alpha '
e2e_wait_for_present "$background_file"
if [[ "$(e2e_capture_pane)" == *"$foreground_file"* ]]; then
  e2e_fail 'background open did not switch to a different result buffer'
fi

# The background-opened result buffer is clean. Replace every alpha match;
# completion must reload that active buffer before the results screen quits.
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter-new' Enter
e2e_wait_for_present 'Search text'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" alpha
e2e_wait_for_present 'Results: 2 [Search complete]'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Tab
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" OMEGA
e2e_press_until_present Enter 'Successful replacements'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Enter
e2e_wait_for_absent 'Successful replacements'
e2e_wait_for_present 'OMEGA '
