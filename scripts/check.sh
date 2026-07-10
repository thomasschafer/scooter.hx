#!/usr/bin/env bash
# Run the full local validation suite (build, clippy, tests) with a single,
# consistent toolchain.
#
# Background: on this machine `cargo`/`rustc` on PATH come from the Nix profile
# (currently 1.94) while `cargo-clippy` is a rustup shim (stable = 1.89), so a
# bare `cargo clippy` fails with E0514 crate-version mismatches. `rustup run`
# does not reliably win over the Nix PATH either, so this script invokes the
# rustup toolchain binaries directly and pins RUSTC, in a dedicated target dir.
# CI uses a single toolchain, so this is a local-only concern.

set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

TOOLCHAIN="${SCOOTER_HX_TOOLCHAIN:-stable-aarch64-apple-darwin}"
TCBIN="$HOME/.rustup/toolchains/$TOOLCHAIN/bin"

if [[ ! -x "$TCBIN/cargo" ]]; then
  echo "Toolchain not found at $TCBIN (set SCOOTER_HX_TOOLCHAIN)" >&2
  exit 1
fi

export PATH="$TCBIN:$PATH"
export RUSTC="$TCBIN/rustc"
export CARGO_TARGET_DIR="target/check"

run() {
  echo "==> $*"
  "$TCBIN/cargo" "$@"
}

run build --all-targets
run clippy --all-targets -- -D warnings
run test
echo "All checks passed ($("$RUSTC" --version))"
