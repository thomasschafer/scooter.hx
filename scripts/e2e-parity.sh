#!/usr/bin/env bash
# Exercise PAR1's keyboard collision fix, bracketed paste, and plugin binding.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=e2e-env.sh
source "$SCRIPT_DIR/e2e-env.sh"

TMUX_SOCKET=""
TMUX_SESSION=""
PANE_TARGET=""

cleanup() {
  e2e_cleanup
  printf '(require "%s")\n' "$REPO/scooter.scm" > "$XDG_CONFIG_HOME/helix/init.scm"
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

start_session() {
  TMUX_SOCKET="scooter-par1-${PPID}-${RANDOM}"
  TMUX_SESSION="scooter-par1"
  PANE_TARGET="$TMUX_SESSION:0.0"
  tmux -L "$TMUX_SOCKET" new-session -d -x 120 -y 40 -s "$TMUX_SESSION" \
    -c "$SEARCH_FIXTURE_DIR" "$HX_BINARY" one.txt \
    || e2e_fail 'could not start the Helix tmux session'
  e2e_wait_for_helix
  e2e_wait_for_present 'alpha one'
  tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter' Enter
  e2e_wait_for_present 'Search text'
}

# Bracketed tmux paste reaches the Rust paste FFI, strips its newline, and
# schedules the normal debounced search.
start_session
printf 'alpha\n' | tmux -L "$TMUX_SOCKET" load-buffer -b scooter-par1-paste -
tmux -L "$TMUX_SOCKET" paste-buffer -p -b scooter-par1-paste -t "$PANE_TARGET"
e2e_wait_for_present 'Results:'
e2e_wait_for_present '[Search complete]'

# A-e is a core binding, not a background-open shortcut, in both focus states.
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" M-e
e2e_wait_for_present 'Escape sequences: ON'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Enter M-e
e2e_wait_for_present 'Escape sequences: OFF'
e2e_cleanup

# A rebound plugin binding continues to background-open while leaving Scooter
# visible.  A-o is deliberately not interpreted as a plugin shortcut here.
printf '(require "%s")\n' "$REPO/scooter.scm" > "$XDG_CONFIG_HOME/helix/init.scm"
printf '%s\n' '(scooter-keys! "plugin.open_in_editor_bg" "A-p")' >> "$XDG_CONFIG_HOME/helix/init.scm"
start_session
# This exact term occurs only in two.txt, so a background open has one
# unambiguous target while the shared `alpha` corpus remains five results.
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" 'background-two-target'
e2e_wait_for_present 'Results: 1 [Search complete]'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Enter M-p
e2e_wait_for_present 'Search text'
e2e_wait_for_present 'two.txt'
e2e_cleanup

# Collision reporting uses the same session-creation path as core conflicts.
printf '(require "%s")\n' "$REPO/scooter.scm" > "$XDG_CONFIG_HOME/helix/init.scm"
printf '%s\n' '(scooter-keys! "plugin.open_in_editor_bg" "A-e")' >> "$XDG_CONFIG_HOME/helix/init.scm"
TMUX_SOCKET="scooter-par1-conflict-${PPID}-${RANDOM}"
TMUX_SESSION="scooter-par1-conflict"
PANE_TARGET="$TMUX_SESSION:0.0"
tmux -L "$TMUX_SOCKET" new-session -d -x 120 -y 40 -s "$TMUX_SESSION" \
  -c "$SEARCH_FIXTURE_DIR" "$HX_BINARY" one.txt \
  || e2e_fail 'could not start the conflicting Helix tmux session'
e2e_wait_for_helix
e2e_wait_for_present 'alpha one'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter' Enter
e2e_wait_for_present 'Key binding conflict detected!'
e2e_wait_for_absent 'Search text'
e2e_cleanup

# Two plugin bindings must also be unique.  This follows the same
# session-creation path and must leave the Scooter window unopened.
printf '(require "%s")\n' "$REPO/scooter.scm" > "$XDG_CONFIG_HOME/helix/init.scm"
printf '%s\n' '(scooter-keys! "plugin.open_in_editor_bg" "A-p")' >> "$XDG_CONFIG_HOME/helix/init.scm"
printf '%s\n' '(scooter-keys! "plugin.hide" "A-p")' >> "$XDG_CONFIG_HOME/helix/init.scm"
TMUX_SOCKET="scooter-par1-plugin-conflict-${PPID}-${RANDOM}"
TMUX_SESSION="scooter-par1-plugin-conflict"
PANE_TARGET="$TMUX_SESSION:0.0"
tmux -L "$TMUX_SOCKET" new-session -d -x 120 -y 40 -s "$TMUX_SESSION" \
  -c "$SEARCH_FIXTURE_DIR" "$HX_BINARY" one.txt \
  || e2e_fail 'could not start the plugin-conflicting Helix tmux session'
e2e_wait_for_helix
e2e_wait_for_present 'alpha one'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter' Enter
e2e_wait_for_present 'Key binding conflict detected!'
e2e_wait_for_absent 'Search text'
e2e_cleanup

# Default Escape hides from fields. A custom, otherwise-unbound hide chord
# works from results focus and from the post-replacement Results screen.
printf '(require "%s")\n' "$REPO/scooter.scm" > "$XDG_CONFIG_HOME/helix/init.scm"
start_session
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Escape
e2e_wait_for_absent 'Search text'
e2e_cleanup

printf '(require "%s")\n' "$REPO/scooter.scm" > "$XDG_CONFIG_HOME/helix/init.scm"
printf '%s\n' '(scooter-keys! "plugin.hide" "C-q")' >> "$XDG_CONFIG_HOME/helix/init.scm"
start_session
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" alpha
e2e_wait_for_present '[Search complete]'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Enter C-q
e2e_wait_for_absent 'Search text'
e2e_cleanup

start_session
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" alpha
e2e_wait_for_present 'Results:'
e2e_wait_for_present '[Search complete]'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Tab
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" OMEGA
e2e_wait_for_present '+ OMEGA one'
e2e_press_until_present Enter 'Successful replacements'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" C-q
e2e_wait_for_absent 'Successful replacements'
e2e_cleanup

# Field text is never a valid hide chord, and session creation reports why.
printf '(require "%s")\n' "$REPO/scooter.scm" > "$XDG_CONFIG_HOME/helix/init.scm"
printf '%s\n' '(scooter-keys! "plugin.hide" "q")' >> "$XDG_CONFIG_HOME/helix/init.scm"
TMUX_SOCKET="scooter-fin1-hide-${PPID}-${RANDOM}"
TMUX_SESSION="scooter-fin1-hide"
PANE_TARGET="$TMUX_SESSION:0.0"
tmux -L "$TMUX_SOCKET" new-session -d -x 120 -y 40 -s "$TMUX_SESSION" \
  -c "$SEARCH_FIXTURE_DIR" "$HX_BINARY" one.txt \
  || e2e_fail 'could not start the invalid hide-binding Helix session'
e2e_wait_for_helix
e2e_wait_for_present 'OMEGA one'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter' Enter
e2e_wait_for_present 'Invalid plugin.hide binding'
e2e_wait_for_absent 'Search text'
