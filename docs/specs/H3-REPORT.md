# H3 report: Helix behaviours

## Delivered

- Foreground result opens now consume `("open-file" path line)` in Steel. The
  component returns the normal `hide` status first, then a thread-local Helix
  callback opens the path, goes to the result line, and centres the view. The
  engine session remains intact for `:scooter` to resume.
- Added `open-file-bg` for Alt plus the first configured
  `search.results.open_in_editor` binding when that binding is a plain
  character. The engine sends the configured foreground key to scooter-core
  and tags only the resulting launch action as background. The chord is a
  no-op in field focus; non-character or modified foreground bindings do not
  receive a background shortcut.
- Added `reload-docs`: processing `ReplacementCompleted` queues the action,
  including when the completion waits in a hidden session. Steel reloads every
  open, non-dirty Helix document when it consumes the action.
- Added `scripts/e2e-open.sh`, covering foreground open and resume,
  background open of a distinct result while the popup remains visible, and
  replacement-driven reload of the clean active buffer.
- Documented configured foreground/background opening, the intentional lack
  of scooter TOML `editor_open` support, and automatic clean-buffer reloads
  in the README.

## Validation

Two consecutive complete passes succeeded on the final uncommitted tree:

- `scripts/check.sh` — build, clippy with `-D warnings`, and 59 Rust tests.
- `scripts/e2e-smoke.sh`
- `scripts/e2e-live-search.sh`
- `scripts/e2e-sizes.sh`
- `scripts/e2e-preview.sh`
- `scripts/e2e-replace.sh`
- `scripts/e2e-lifecycle.sh`
- `scripts/e2e-config.sh`
- `scripts/e2e-open.sh`

No commit was created, and no files in `../scooter` were modified.
