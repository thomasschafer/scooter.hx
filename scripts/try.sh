#!/usr/bin/env bash
# Launch the dev build of Helix + the plugin, fully isolated from your real
# helix/steel setup, in a directory of your choice.
#
# Usage: scripts/try.sh [directory]   (defaults to the current directory)
#
# Then use :scooter / :scooter-new as usual. Esc hides the window (session
# kept), ctrl-c quits the session.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TARGET_DIR="$(cd "${1:-$PWD}" && pwd)"

# e2e-env.sh locates the repo with git, so source it from inside the repo
# regardless of where this script was invoked from.
cd "$SCRIPT_DIR/.."
# shellcheck source=e2e-env.sh
source "$SCRIPT_DIR/e2e-env.sh"

STEEL_HOME="$STEEL_HOME" cargo steel-lib >/dev/null

cd "$TARGET_DIR"
exec "$HX_BINARY"
