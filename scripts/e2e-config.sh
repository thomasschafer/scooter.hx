#!/usr/bin/env bash
# Exercise the public Steel configuration surface against isolated Helix sessions.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=e2e-env.sh
source "$SCRIPT_DIR/e2e-env.sh"

TMUX_SOCKET=""
TMUX_SESSION=""
PANE_TARGET=""

restore_standard_init() {
  printf '(require "%s")\n' "$REPO/scooter.scm" > "$XDG_CONFIG_HOME/helix/init.scm"
}

cleanup() {
  e2e_cleanup
  restore_standard_init
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

printf '(require "%s")\n' "$REPO/scooter.scm" > "$XDG_CONFIG_HOME/helix/init.scm"
printf '%s\n' "(scooter-set! 'multiline #t)" >> "$XDG_CONFIG_HOME/helix/init.scm"
printf '%s\n' '(scooter-keys! "search.results.move_down" "n")' \
  >> "$XDG_CONFIG_HOME/helix/init.scm"

printf '%s\n' \
  'multiline-first-a' \
  'multiline-second' \
  'between matches' \
  'multiline-first-b' \
  'multiline-second' > "$SEARCH_FIXTURE_DIR/multiline.txt"

TMUX_SOCKET="scooter-c1-config-${PPID}-${RANDOM}"
TMUX_SESSION="scooter-c1-config"
PANE_TARGET="$TMUX_SESSION:0.0"
tmux -L "$TMUX_SOCKET" new-session -d -x 120 -y 40 -s "$TMUX_SESSION" \
  -c "$SEARCH_FIXTURE_DIR" "$HX_BINARY" multiline.txt \
  || e2e_fail 'could not start the configured Helix tmux session'

e2e_wait_for_helix
e2e_wait_for_present 'multiline-first-a'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter' Enter
e2e_wait_for_present 'Search text'

# The literal regex escape spans a newline. It only produces these two results
# when `multiline` has already been applied to the freshly created engine.
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" \
  'multiline-first-[ab]\nmultiline-second'
e2e_wait_for_present 'Results: 2 [Search complete]'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Enter
e2e_wait_for_present 'multiline.txt:1'

# `j` was removed by the single-string override, while `n` is now the only
# move-down binding. Toggle each selected result to make the selection visible.
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" j Space
e2e_wait_for_present '[ ] multiline.txt:1'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" n Space
e2e_wait_for_present '[ ] multiline.txt:4'
e2e_cleanup

# A conflict returns core's full message as an ordinary string. Steel shows it
# in Helix's error status and must not construct or open a Scooter window.
printf '(require "%s")\n' "$REPO/scooter.scm" > "$XDG_CONFIG_HOME/helix/init.scm"
printf '%s\n' '(scooter-keys! "general.quit" "C-r")' \
  >> "$XDG_CONFIG_HOME/helix/init.scm"

TMUX_SOCKET="scooter-c1-conflict-${PPID}-${RANDOM}"
TMUX_SESSION="scooter-c1-conflict"
PANE_TARGET="$TMUX_SESSION:0.0"
tmux -L "$TMUX_SOCKET" new-session -d -x 120 -y 40 -s "$TMUX_SESSION" \
  -c "$SEARCH_FIXTURE_DIR" "$HX_BINARY" multiline.txt \
  || e2e_fail 'could not start the conflicting Helix tmux session'

e2e_wait_for_helix
e2e_wait_for_present 'multiline-first-a'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter' Enter
e2e_wait_for_present 'Key binding conflict detected!'
e2e_wait_for_absent 'Search text'
