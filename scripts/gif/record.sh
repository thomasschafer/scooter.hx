#!/usr/bin/env bash
# Regenerate media/preview.gif: drive hx+scooter in tmux, sample the screen
# with atomic cursor+capture calls, synthesize an asciinema cast, render via
# agg. Requires agg (brew install agg) and the isolated dev environment from
# scripts/e2e-env.sh. The demo runs on a throwaway fixture copy: scooter
# mutates files during the replacement.
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"
source scripts/e2e-env.sh

WORK="${TMPDIR:-/tmp}/scooter-hx-gif"
GIF_FIX="$WORK/fixture"
FRAMES="$WORK/frames"
SOCK=gifrec
COLS=142
ROWS=38

rm -rf "$WORK" && mkdir -p "$GIF_FIX" "$FRAMES"
cp -R ~/Development/helix/helix-core/src "$GIF_FIX/helix-core"

STEEL_HOME="$STEEL_HOME" cargo steel-lib >/dev/null 2>&1

tmux -L "$SOCK" kill-server 2>/dev/null || true
tmux -L "$SOCK" new-session -d -x "$COLS" -y "$ROWS" -s gif -c "$GIF_FIX" "$HX_BINARY helix-core/transaction.rs"
tmux -L "$SOCK" set -t gif status off
sleep 4

capture_loop() {
  local ms
  while tmux -L "$SOCK" has-session -t gif 2>/dev/null; do
    ms=$(perl -MTime::HiRes=time -e 'printf "%d", time()*1000')
    # One tmux client invocation keeps the cursor query and the screen
    # capture near-atomic; separate calls sample different redraws while
    # typing, landing the rendered cursor on the wrong cell.
    tmux -L "$SOCK" display -p -t gif '@CURSOR #{cursor_x} #{cursor_y}' \; \
      capture-pane -e -p -t gif > "$FRAMES/f_${ms}.txt" 2>/dev/null || true
    sleep 0.08
  done
}
capture_loop &
CAP_PID=$!

send() { tmux -L "$SOCK" send-keys -t gif "$@"; }
type_slow() {
  local text="$1"
  local i
  for ((i = 0; i < ${#text}; i++)); do
    tmux -L "$SOCK" send-keys -t gif -l "${text:$i:1}"
    sleep 0.09
  done
}

sleep 0.8
type_slow ':scooter'
send Enter
sleep 1.0
type_slow 'foo'
sleep 1.2
send Tab
sleep 0.5
type_slow 'bar'
sleep 0.9
send Enter
sleep 1.2
for _ in 1 2 3 4; do
  send j
  sleep 0.45
done
send Space
sleep 0.9
send Enter
sleep 2.2
send Enter
sleep 1.6

tmux -L "$SOCK" kill-server 2>/dev/null || true
wait "$CAP_PID" 2>/dev/null || true

python3 scripts/gif/build_cast.py "$FRAMES" "$WORK/demo.cast" "$COLS" "$ROWS"
agg --font-size 14 "$WORK/demo.cast" "$WORK/preview.gif" 2>/dev/null
ffprobe -v error -select_streams v:0 -show_entries stream=width,height,nb_frames -of csv=p=0 "$WORK/preview.gif"
echo "output: $WORK/preview.gif ($(wc -c < "$WORK/preview.gif" | tr -d ' ') bytes)"
