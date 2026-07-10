# scooter.hx

scooter.hx is Scooter's native [Helix](https://helix-editor.com/) plugin. It
embeds the same `scooter-core` engine as the
[Scooter TUI](https://github.com/thomasschafer/scooter), so searching,
selection, previews, and replacement follow the TUI's behaviour. The window
and all of its styling are rendered natively by Helix, using your Helix theme.

> **Stale media — release decision pending.** The checked-in
> [`media/preview.gif`](media/preview.gif) and
> [`media/preview.png`](media/preview.png) predate this rewrite and are
> intentionally not embedded here. They need to be regenerated or removed
> before release.

## Requirements

scooter.hx requires Helix built from its
[`steel-event-system`](https://github.com/mattwparas/helix/tree/steel-event-system)
branch and the Steel toolchain installed from that checkout (including
`cargo xtask steel`). Follow that branch's
[Steel instructions](https://github.com/mattwparas/helix/blob/steel-event-system/STEEL.md).

The native dylib must use the exact `steel-core` and `steel-derive` revision
that Helix uses. This repository pins
[`dec633b908afeafeaf62bab457a92e2bf873745a`](https://github.com/mattwparas/steel/tree/dec633b908afeafeaf62bab457a92e2bf873745a);
changing the pin independently can make Helix reject the dylib at load time.

## Installation

Add the following after installing with either method below:

```scheme
(require "scooter/scooter.scm")
```

Put it in Helix's `init.scm` (normally under `~/.config/helix/`).

### Forge

```sh
forge pkg install --git https://github.com/thomasschafer/scooter.hx.git
```

### Build from source

```sh
git clone https://github.com/thomasschafer/scooter.hx.git
cd scooter.hx
cargo steel-lib
```

For a source checkout, require the checked-out Scheme file instead:

```scheme
(require "/absolute/path/to/scooter.hx/scooter.scm")
```

## Usage

- `:scooter` opens a new session or resumes the current one.
- `:scooter-new` cancels the current session and starts again with fresh
  fields and the current configuration.

`esc` hides the window when Scooter has nothing else to dismiss. The search
and any background work continue, and `:scooter` resumes that session. `C-c`
quits instead, cancelling work and discarding the session.

When the results list is focused, `e` opens the selected match in Helix and
hides the window; `A-e` opens it in the background and leaves the window open.
After a replacement completes, scooter.hx reloads every open, non-dirty Helix
buffer so its contents are current.

### Default keymap

This table is generated from `scooter_core::config::KeysConfig::default()` at
revision `9387c36`; it is the same default map as the Scooter TUI. Bindings
are written in Scooter's key syntax.

| Context | Action | Default binding(s) |
| --- | --- | --- |
| All screens | Quit session | `C-c` |
| All screens | Reset fields and cancel work | `C-r` |
| All screens | Show help | `C-h` |
| Search screen | Toggle preview wrapping | `C-l` |
| Search screen | Toggle hidden files | `C-t` |
| Search screen | Toggle multiline search | `A-m` |
| Search screen | Toggle replacement escape sequences | `A-e` |
| Fields focused | Unlock prepopulated fields | `A-u` |
| Fields focused | Search / advance | `enter` |
| Fields focused | Next field | `tab` |
| Fields focused | Previous field | `S-tab` |
| Results focused | Replace included results | `enter` |
| Results focused | Return to fields | `esc`, `C-o` |
| Results focused | Open selected result | `e` |
| Results focused | Move down | `j`, `down`, `C-n` |
| Results focused | Move up | `k`, `up`, `C-p` |
| Results focused | Move down half a page | `C-d` |
| Results focused | Move up half a page | `C-u` |
| Results focused | Move down a page | `C-f`, `pagedown` |
| Results focused | Move up a page | `C-b`, `pageup` |
| Results focused | First result | `g` |
| Results focused | Last result | `G` |
| Results focused | Toggle selected result | `space` |
| Results focused | Toggle all results | `a` |
| Results focused | Toggle multiselect | `v` |
| Results focused | Reverse multiselect direction | `A-;` |
| Replacement results | Next error | `j`, `down`, `C-n` |
| Replacement results | Previous error | `k`, `up`, `C-p` |
| Replacement results | Close results | `enter`, `q` |

## Configuration

Configure scooter.hx in `init.scm`, after its `require` line. Settings are
captured when a session is created: they apply to the first `:scooter` with no
active session, or to `:scooter-new`; a hidden session retains its existing
settings until then.

```scheme
(scooter-set! 'multiline #t)
(scooter-set! 'wrap-text #t)
(scooter-set! 'window-size 0.85)
(scooter-keys! "search.results.move_down" '("j" "down"))
```

### `scooter-set!`

`(scooter-set! 'setting value)` accepts these settings. Later calls to the
same setting win.

| Setting | Value | Default | Effect |
| --- | --- | --- | --- |
| `multiline` | boolean | `#f` | Allow searches to span line boundaries. |
| `hidden` | boolean | `#f` | Include hidden files and directories. |
| `advanced-regex` | boolean | `#f` | Enable the advanced regex engine. |
| `include-git-folders` | boolean | `#f` | Search Git metadata directories. |
| `escape-sequences` | boolean | `#f` | Interpret `\\n`, `\\t`, and `\\\\` in replacement text. |
| `wrap-text` | boolean | `#f` | Wrap long preview lines. |
| `window-size` | number, `0.5`–`1.0` | `0.9` | Set the window size as a terminal ratio. |

### `scooter-keys!`

`(scooter-keys! "path" bindings)` replaces one action's bindings. `bindings`
may be one string or a list of strings, using Scooter's syntax: modifiers are
`S-`, `C-`, and `A-`, so examples include `"C-o"`, `"A-m"`, and `"S-tab"`.
The path omits the leading `keys.`; for example,
`"general.quit"`, `"search.fields.trigger_search"`,
`"search.results.move_down"`, and `"results.quit"` are valid. Any binding
path shown in the default keymap can be used.

Scooter validates the complete map when creating a session. An invalid key or
a conflicting binding is reported in Helix's error area and the window does
not open. The foreground open action follows a remapped
`search.results.open_in_editor`; its background shortcut is Alt plus that
action's first binding only when it is an unmodified character key.

## Differences from the Scooter TUI

- Preview syntax highlighting is not implemented yet; previews use plain text
  with Helix-theme diff styling. See the [planned future work](docs/REWRITE-PLAN.md#future-work-post-v1).
- Scooter's TOML `editor_open` configuration does not apply. scooter.hx
  always opens results in Helix.
- `esc` hides the plugin window when it would otherwise be unhandled; in the
  results list it retains its normal action of returning to the fields.

## Development and local validation

`scripts/check.sh` is the portable Rust check used during development: it
builds all targets, runs Clippy with warnings denied, and runs the test suite
(including frame snapshots) against one pinned local toolchain.

The tmux e2e harness is deliberately local-only because it requires a local
Helix binary from `steel-event-system`, its runtime, Steel cogs, and tmux. It
uses an isolated `.dev/` Steel and Helix configuration, never your normal
configuration. After preparing that environment as described in
[`docs/REWRITE-PLAN.md`](docs/REWRITE-PLAN.md#target-environment), run:

```sh
scripts/e2e-smoke.sh
scripts/e2e-live-search.sh
scripts/e2e-sizes.sh
scripts/e2e-preview.sh
scripts/e2e-replace.sh
scripts/e2e-lifecycle.sh
scripts/e2e-config.sh
scripts/e2e-open.sh
```

For an interactive isolated development session, use `scripts/try.sh
[directory]`.
