#!/usr/bin/env bash
# Deterministic gif pipeline: drive hx+scooter in tmux, sample the screen with
# tmux capture-pane -e on a timer, synthesize an asciinema cast, render via agg.
set -euo pipefail

cd /Users/tomschafer/Development/scooter.hx
source scripts/e2e-env.sh

SCRATCH=/private/tmp/claude-501/-Users-tomschafer-Development-scooter-hx/46725038-b467-462a-ad1e-8e8a02c3abd2/scratchpad/gif
GIF_FIX="$SCRATCH/fixture"
SOCK=gifrec
COLS=142
ROWS=38
FRAMES="$SCRATCH/frames"

# Throwaway fixture in /tmp scratch: scooter mutates files during the demo.
rm -rf "$GIF_FIX" && mkdir -p "$GIF_FIX"
cp -R ~/Development/helix/helix-core/src "$GIF_FIX/helix-core"

STEEL_HOME="$STEEL_HOME" cargo steel-lib >/dev/null 2>&1

tmux -L "$SOCK" kill-server 2>/dev/null || true
tmux -L "$SOCK" new-session -d -x "$COLS" -y "$ROWS" -s gif -c "$GIF_FIX" "$HX_BINARY helix-core/transaction.rs"
tmux -L "$SOCK" set -t gif status off
sleep 4

rm -rf "$FRAMES" && mkdir -p "$FRAMES"
capture_loop() {
  while tmux -L "$SOCK" has-session -t gif 2>/dev/null; do
    local ms
    ms=$(python3 -c 'import time; print(int(time.time()*1000))')
    {
      tmux -L "$SOCK" capture-pane -e -p -t gif
      printf '@CURSOR %s\n' "$(tmux -L "$SOCK" display -p -t gif '#{cursor_x} #{cursor_y}')"
    } > "$FRAMES/f_${ms}.txt" 2>/dev/null || true
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
sleep 2.2
send Tab
sleep 0.5
type_slow 'bar'
sleep 1.6
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

python3 "$SCRATCH/build_cast.py" "$FRAMES" "$SCRATCH/demo.cast" "$COLS" "$ROWS"
agg --font-size 14 "$SCRATCH/demo.cast" "$SCRATCH/preview.gif" 2>/dev/null
ffprobe -v error -select_streams v:0 -show_entries stream=width,height,nb_frames -of csv=p=0 "$SCRATCH/preview.gif"
ls -la "$SCRATCH/preview.gif" | awk '{print $5}'
