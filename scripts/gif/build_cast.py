#!/usr/bin/env python3
"""Build an asciinema v2 cast from timestamped tmux ANSI captures.

Each frame file starts with a line `@CURSOR <x> <y>` (0-based pane coords)
followed by the `tmux capture-pane -e -p` screen body. Malformed frames are
skipped rather than rendered.
"""
import json
import sys
from pathlib import Path

frames_dir = Path(sys.argv[1])
out_path = Path(sys.argv[2])
cols, rows = int(sys.argv[3]), int(sys.argv[4])

frames = sorted(frames_dir.glob("f_*.txt"))
if not frames:
    sys.exit("no frames captured")

events = []
t0 = None
previous = None
for frame in frames:
    ms = int(frame.stem.split("_")[1])
    raw = frame.read_text(errors="replace")
    marker, _, body = raw.partition("\n")
    if not marker.startswith("@CURSOR ") or not body.strip():
        continue
    try:
        cx, cy = (int(v) for v in marker.split()[1:3])
    except ValueError:
        continue
    if t0 is None:
        t0 = ms
    lines = body.split("\n")[:rows]
    # Address every row explicitly: printing a newline after the bottom row
    # would scroll the screen, shifting content one row off the cursor.
    screen = "".join(
        f"\x1b[{row + 1};1H{line}\x1b[K" for row, line in enumerate(lines)
    )
    screen += f"\x1b[{cy + 1};{cx + 1}H\x1b[?25h"
    if screen == previous:
        continue
    previous = screen
    events.append([round((ms - t0) / 1000.0, 3), "o", screen])

if not events:
    sys.exit("no valid frames")

# Hold the final frame briefly so the gif does not loop abruptly.
events.append([events[-1][0] + 1.2, "o", "\x1b[?25h"])

with out_path.open("w") as out:
    out.write(json.dumps({"version": 2, "width": cols, "height": rows}) + "\n")
    for event in events:
        out.write(json.dumps(event) + "\n")
print(f"cast: {len(events)} events, {events[-1][0]}s")
