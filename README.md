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

## Contents

<!-- TOC START -->
- [Requirements](#requirements)
- [Installation](#installation)
- [Usage](#usage)
- [Configuration](#configuration)
- [Differences from the Scooter TUI](#differences-from-the-scooter-tui)
- [Development and local validation](#development-and-local-validation)
<!-- TOC END -->

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

<!-- KEYS START -->
| Binding path | Default key(s) | Description |
| --- | --- | --- |
| `general.quit` | `C-c` | Exit scooter |
| `general.reset` | `C-r` | Cancel in-progress operations, reset fields to default values and return to search screen |
| `general.show_help_menu` | `C-h` | Show the help menu containing keymaps |
| `results.quit` | `enter`, `q` | Exit scooter. This is in addition to the `quit` command in the `general` section. |
| `results.scroll_errors_down` | `j`, `down`, `C-n` | Navigate to the error below |
| `results.scroll_errors_up` | `k`, `up`, `C-p` | Navigate to the error above |
| `search.fields.focus_next_field` | `tab` | Focus on the next field |
| `search.fields.focus_previous_field` | `S-tab` | Focus on the previous field |
| `search.fields.trigger_search` | `enter` | Trigger a search |
| `search.fields.unlock_prepopulated_fields` | `A-u` | Allow editing of fields that were populated using CLI args, such as `--search_text foo`. (Note that you can use the `disable_prepopulated_fields` config option to change the default behaviour.) |
| `search.results.back_to_fields` | `esc`, `C-o` | Move focus back to the search fields |
| `search.results.flip_multiselect_direction` | `A-;` | Flip the direction of the multiselect selection |
| `search.results.move_bottom` | `G` | Navigate to the last search result |
| `search.results.move_down` | `j`, `down`, `C-n` | Navigate to the search result below |
| `search.results.move_down_full_page` | `C-f`, `pagedown` | Navigate to the search result a page below |
| `search.results.move_down_half_page` | `C-d` | Navigate to the search result half a page below |
| `search.results.move_top` | `g` | Navigate to the first search result |
| `search.results.move_up` | `k`, `up`, `C-p` | Navigate to the search result above |
| `search.results.move_up_full_page` | `C-b`, `pageup` | Navigate to the search result a page above |
| `search.results.move_up_half_page` | `C-u` | Navigate to the search result half a page above |
| `search.results.open_in_editor` | `e` | Open the currently selected search result in your editor. The editor command can be overriden using the `editor_open` section of your config. |
| `search.results.toggle_all_selected` | `a` | Toggle whether all results will be replaced or ignored |
| `search.results.toggle_multiselect_mode` | `v` | Toggle whether multiselect mode is enabled |
| `search.results.toggle_selected_inclusion` | `space` | Toggle whether the currently highlighted result will be replaced or ignored |
| `search.results.trigger_replacement` | `enter` | Trigger a replacement |
| `search.toggle_hidden_files` | `C-t` | Toggle inclusion of hidden files and directories, such as those whose name starts with a dot (.) |
| `search.toggle_interpret_escape_sequences` | `A-e` | Toggle interpretation of escape sequences in replacement text (\n becomes newline, \t becomes tab, \\ becomes backslash) |
| `search.toggle_multiline` | `A-m` | Toggle multiline search mode, which allows patterns to match across line boundaries |
| `search.toggle_preview_wrapping` | `C-l` | Toggle wrapping of lines that don't fit within the width of the preview |
<!-- KEYS END -->

## Configuration

Configure scooter.hx in `init.scm`, after its `require` line. Settings are
captured when a session is created: they apply to the first `:scooter` with no
active session, or to `:scooter-new`; a hidden session retains its existing
settings until then.

```scheme
(scooter-set! 'multiline #t)
(scooter-set! 'wrap-text #t)
(scooter-set! 'syntax-highlighting #f)
(scooter-set! 'window-size 0.85)
(scooter-set! 'runtime-dir "/path/to/helix/runtime")
(scooter-keys! "search.results.move_down" '("j" "down"))
```

<!-- CONFIG START -->
| Setting | Value | Default | Effect |
| --- | --- | --- | --- |
| `multiline` | boolean | `#f` | Allow search patterns to match across line boundaries. |
| `hidden` | boolean | `#f` | Include hidden files and directories. |
| `advanced-regex` | boolean | `#f` | Enable Scooter's advanced regular-expression engine. |
| `include-git-folders` | boolean | `#f` | Search Git metadata directories as well as normal files. |
| `escape-sequences` | boolean | `#f` | Interpret `\n`, `\t`, and `\\` in replacement text. |
| `wrap-text` | boolean | `#f` | Wrap long preview lines. |
| `syntax-highlighting` | boolean | `#t` | Highlight preview context with Helix grammars. |
| `window-size` | number, `0.5`–`1.0` | `0.9` | Set the window size as a terminal ratio. |
| `runtime-dir` | string path | Helix runtime discovery | Override the runtime used to load preview syntax grammars. |

### `scooter-keys!`

`(scooter-keys! "path" bindings)` replaces one action's bindings. `bindings` may be one string or a list of strings, using Scooter's syntax: modifiers are `S-`, `C-`, and `A-`. The path omits the leading `keys.`.

```scheme
(scooter-keys! "search.results.move_down" '("j" "down"))
```
<!-- CONFIG END -->

Scooter validates the complete map when creating a session. An invalid key or
a conflicting binding is reported in Helix's error area and the window does
not open. The foreground open action follows a remapped
`search.results.open_in_editor`; its background shortcut is Alt plus that
action's first binding only when it is an unmodified character key.

## Differences from the Scooter TUI

- Preview context uses Helix grammar highlighting and the editor background rather than the TUI preview surface; diff lines retain Helix-theme diff styling.
- Scooter's TOML `editor_open` configuration does not apply. scooter.hx
  always opens results in Helix.
- `esc` hides the plugin window when it would otherwise be unhandled; in the
  results list it retains its normal action of returning to the fields.

## Development and local validation

`scripts/check.sh` is the portable Rust check used during development: it
builds all targets, runs Clippy with warnings denied, and runs the test suite
(including frame snapshots) against one pinned local toolchain.

Regenerate README reference sections with `cargo xtask readme`; CI verifies
them with `cargo xtask readme --check`.

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
