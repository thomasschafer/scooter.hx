# scooter.hx

scooter.hx is a find-and-replace plugin for [Helix](https://github.com/helix-editor/helix/).

Search with either a fixed string or a regular expression, enter a replacement, and interactively toggle which instances you want to replace.

If the instance you're attempting to replace has changed since the search was performed, e.g. if you've switched branches and that line no longer exists, that particular replacement won't occur: you'll see all such cases at the end.

![scooter.hx preview](media/preview.gif)

## Usage

This plugin provides the following commands (which you can bind a keymap to in the usual way):
- `:scooter` - resume an existing session if one exists, or otherwise start a new session
- `:scooter-new` - start a new session, cancelling any in-progress operations

Once open, you can hit `escape` to hide the window (and open it again with `:scooter`), or `ctrl-c` to quit and cancel any in-progress searches/replacements.

When viewing the search results, you can hit `e` to open up one of the results (keeping the existing session running). You can also use `<alt>-e` to open a search result in the background, without hiding the window. 

## Installation

Follow the instructions [here](https://github.com/mattwparas/helix/blob/steel-event-system/STEEL.md) to install Helix on the plugin branch.

Then, install the plugin with one of the installation methods below.

Once installed, you can add the following to `init.scm` in your Helix config directory:

```scheme
(require "scooter/scooter.scm")
```

### Using Forge

Forge is the Steel package manager, and will have been installed in the previous step. Run the following:

```sh
forge pkg install --git https://github.com/thomasschafer/scooter.hx.git
```

### Building from source

1. Clone and `cd` into this repo and run `cargo steel-lib`
1. Add `(require "<path>/scooter.hx/scooter.scm")` to `init.scm` in your Helix config (where `<path>` is the absolute path of the folder containing the scooter.hx repo)
