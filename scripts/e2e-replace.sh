#!/usr/bin/env bash
# Exercise overlays and a complete replacement against a disposable fixture.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=e2e-env.sh
source "$SCRIPT_DIR/e2e-env.sh"

CAPTURE_DIR="$REPO/.dev/e2e-captures"
mkdir -p "$CAPTURE_DIR"
REPLACE_FIXTURE_DIR="$(mktemp -d "$REPO/.dev/fixtures/replace.XXXXXX")"
cp -R "$SEARCH_FIXTURE_DIR"/. "$REPLACE_FIXTURE_DIR"/

TMUX_SOCKET="scooter-e5-replace-${PPID}-${RANDOM}"
TMUX_SESSION="scooter-e5-replace"
PANE_TARGET="$TMUX_SESSION:0.0"

cleanup() {
  e2e_cleanup
  rm -rf "$REPLACE_FIXTURE_DIR"
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

tmux -L "$TMUX_SOCKET" new-session -d -x 120 -y 40 -s "$TMUX_SESSION" \
  -c "$REPLACE_FIXTURE_DIR" "$HX_BINARY" one.txt \
  || e2e_fail 'could not start the Helix tmux session'

e2e_wait_for_helix
e2e_wait_for_present 'alpha one'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter' Enter
e2e_wait_for_present 'Search text'

tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" C-h
e2e_wait_for_present 'Help'
e2e_wait_for_present 'jump to results'
e2e_capture_pane > "$CAPTURE_DIR/e5-help-popup.txt"
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Escape
e2e_wait_for_absent 'Help'
e2e_wait_for_present 'Search text'

tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" '('
e2e_wait_for_present 'Invalid search'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Enter
e2e_wait_for_present 'Errors'
e2e_assert_popup_border_has_uniform_style 'Errors'
e2e_assert_popup_interior_matches_border_background 'Errors'
e2e_capture_pane > "$CAPTURE_DIR/f1-error-popup.txt"
e2e_capture_pane_with_style > "$CAPTURE_DIR/f1-error-popup.ansi"
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Escape
e2e_wait_for_absent 'Errors'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" C-r
e2e_wait_for_present 'Results: 0 [Search is empty]'

tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" M-m
e2e_wait_for_present 'Multiline: ON'
e2e_wait_for_absent 'Multiline: ON'

tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" alpha
e2e_wait_for_present 'Results: 5 [Search complete]'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Tab
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" OMEGA
e2e_wait_for_present '+ OMEGA one'
# Replacement previews update asynchronously after the first visible diff. The
# core deliberately rejects replacement until that short update finishes.
sleep 1
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Enter
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Enter
e2e_wait_for_present 'Successful replacements'
e2e_capture_pane > "$CAPTURE_DIR/e5-results.txt"

if ! grep -q 'OMEGA one' "$REPLACE_FIXTURE_DIR/one.txt"; then
  e2e_fail 'replacement did not update the disposable fixture'
fi
if grep -q 'alpha one' "$REPLACE_FIXTURE_DIR/one.txt"; then
  e2e_fail 'original search text remains in the disposable fixture'
fi

tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Enter
e2e_wait_for_absent 'Successful replacements'
e2e_wait_for_absent 'Search text'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter' Enter
e2e_wait_for_present 'Search text'
e2e_wait_for_present 'Results: 0 [Search is empty]'
