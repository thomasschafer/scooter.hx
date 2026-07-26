#!/usr/bin/env bash
# Transparent themes define no ui.background colour; this guards the style
# table's Color/Reset fallbacks (a #false colour crashes the render callback).
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
E2E_THEME=base16_transparent "$SCRIPT_DIR/e2e-smoke.sh"
E2E_THEME=base16_transparent "$SCRIPT_DIR/e2e-live-search.sh"
