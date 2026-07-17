#!/usr/bin/env python3
"""Build an asciinema v2 cast from timestamped tmux ANSI captures."""
import json
import sys
from pathlib import Path

frames_dir = Path(sys.argv[1])
out_path = Path(sys.argv[2])
cols, rows = int(sys.argv[3]), int(sys.argv[4])

frames = sorted(frames_dir.glob("f_*.txt"))
if not frames:
    sys.exit("no frames captured")

t0 = int(frames[0].stem.split("_")[1])
events = []
previous = None
for frame in frames:
    ms = int(frame.stem.split("_")[1])
    raw = frame.read_text(errors="replace")
    # Last line of the capture file is "cursor_x cursor_y".
    body, _, cursor = raw.rpartition("\n@CURSOR ")
    if not body:
        body, cursor = raw, "0 0"
    cx, cy = (int(v) for v in cursor.split())
    lines = body.split("\n")[:rows]
    screen = "\x1b[H" + "".join(f"{line}\x1b[K\r\n" for line in lines)
    screen += f"\x1b[{cy + 1};{cx + 1}H\x1b[?25h"
    if screen == previous:
        continue
    previous = screen
    events.append([round((ms - t0) / 1000.0, 3), "o", screen])

# Hold the final frame briefly so the gif does not loop abruptly.
events.append([events[-1][0] + 1.2, "o", "\x1b[?25h"])

with out_path.open("w") as out:
    out.write(json.dumps({"version": 2, "width": cols, "height": rows}) + "\n")
    for event in events:
        out.write(json.dumps(event) + "\n")
print(f"cast: {len(events)} events, {events[-1][0]}s")
