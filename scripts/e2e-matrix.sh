#!/usr/bin/env bash
# PAR1 standing option-wiring net. Run this script twice in the final sweep.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=e2e-env.sh
source "$SCRIPT_DIR/e2e-env.sh"

TMUX_SOCKET="scooter-par1-matrix-${PPID}-${RANDOM}"
TMUX_SESSION="scooter-par1-matrix"
PANE_TARGET="$TMUX_SESSION:0.0"

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

printf '(require "%s")\n' "$REPO/scooter.scm" > "$XDG_CONFIG_HOME/helix/init.scm"
printf '%s\n' "(scooter-set! 'multiline #t)" >> "$XDG_CONFIG_HOME/helix/init.scm"
printf '%s\n' "(scooter-set! 'advanced-regex #t)" >> "$XDG_CONFIG_HOME/helix/init.scm"
printf '%s\n' "(scooter-set! 'wrap-text #t)" >> "$XDG_CONFIG_HOME/helix/init.scm"
printf '%s\n' "(scooter-set! 'syntax-highlighting #f)" >> "$XDG_CONFIG_HOME/helix/init.scm"
printf '%s\n' '(scooter-keys! "search.results.move_down" "n")' >> "$XDG_CONFIG_HOME/helix/init.scm"

printf '%s\n' \
  'matrix-first-a' \
  'matrix-second with a deliberately long preview tail matrix-wrap-marker matrix-wrap-marker matrix-wrap-marker matrix-wrap-marker matrix-wrap-marker matrix-wrap-marker matrix-wrap-marker matrix-wrap-marker matrix-wrap-marker matrix-wrap-marker matrix-wrap-required' \
  'between' \
  'matrix-first-b' \
  'matrix-second with a deliberately long preview tail matrix-wrap-marker matrix-wrap-marker matrix-wrap-marker matrix-wrap-marker matrix-wrap-marker matrix-wrap-marker matrix-wrap-marker matrix-wrap-marker matrix-wrap-marker matrix-wrap-marker matrix-wrap-required' \
  > "$SEARCH_FIXTURE_DIR/matrix.txt"

tmux -L "$TMUX_SOCKET" new-session -d -x 100 -y 34 -s "$TMUX_SESSION" \
  -c "$SEARCH_FIXTURE_DIR" "$HX_BINARY" matrix.txt \
  || e2e_fail 'could not start the Helix tmux session'
e2e_wait_for_helix
e2e_wait_for_present 'matrix-first-a'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter' Enter
e2e_wait_for_present 'Search text'

# The lookahead requires the advanced regex engine; the literal newline
# requires multiline. Two matches prove both settings were consumed.
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" \
  '(?=matrix-first-[ab]\nmatrix-second)matrix-first-[ab]\nmatrix-second'
e2e_wait_for_present 'Results: 2 [Search complete]'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Enter n Space
e2e_wait_for_present '[ ] matrix.txt:4'
e2e_wait_for_present 'matrix-wrap-required'
if ! e2e_capture_pane | grep -E '↪ .*matrix-wrap-required' >/dev/null; then
  e2e_fail 'wrap marker was not rendered on a wrapped preview row'
fi

# The default A-e must still reach core under the complete option matrix.
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" M-e
e2e_wait_for_present 'Escape sequences: ON'
