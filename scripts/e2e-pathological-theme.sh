#!/usr/bin/env bash
# A theme with only ui.cursor must still render Scooter with terminal-default
# colours; this is deliberately stricter than transparent base16 coverage.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
E2E_THEME=def1_pathological E2E_PATHOLOGICAL_THEME=1 "$SCRIPT_DIR/e2e-smoke.sh"
E2E_THEME=def1_pathological E2E_PATHOLOGICAL_THEME=1 "$SCRIPT_DIR/e2e-live-search.sh"
