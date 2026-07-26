#!/usr/bin/env bash
# Exercise the native results list and preview pane at both layout breakpoints.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=e2e-env.sh
source "$SCRIPT_DIR/e2e-env.sh"

CAPTURE_DIR="$REPO/.dev/e2e-captures"
mkdir -p "$CAPTURE_DIR"

TMUX_SOCKET=""
TMUX_SESSION=""
PANE_TARGET=""

restore_standard_init() {
  printf '(require "%s")\n' "$REPO/scooter.scm" > "$XDG_CONFIG_HOME/helix/init.scm"
}

if [[ ! -x "$HX_BINARY" ]]; then
  e2e_fail "Helix test binary is not executable: $HX_BINARY"
fi

if ! command -v tmux >/dev/null; then
  e2e_fail 'tmux is required'
fi

if ! STEEL_HOME="$STEEL_HOME" cargo steel-lib; then
  e2e_fail 'cargo steel-lib could not build and install the dylib'
fi

run_size() {
  local terminal_width="$1"
  local terminal_height="$2"

  TMUX_SOCKET="scooter-e4-${terminal_width}x${terminal_height}-${PPID}-${RANDOM}"
  TMUX_SESSION="scooter-e4-${terminal_width}x${terminal_height}"
  PANE_TARGET="$TMUX_SESSION:0.0"

  tmux -L "$TMUX_SOCKET" new-session -d -x "$terminal_width" -y "$terminal_height" \
    -s "$TMUX_SESSION" -c "$PREVIEW_FIXTURE_DIR" "$HX_BINARY" preview.txt \
    || e2e_fail "could not start the ${terminal_width}x${terminal_height} Helix tmux session"

  e2e_wait_for_helix
  e2e_wait_for_present 'preview context before first result'
  tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter' Enter
  e2e_wait_for_present 'Search text'
  tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" 'alpha'
  e2e_wait_for_present 'Results: 2 [Search complete]'

  tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Tab
  tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" 'OMEGA'
  tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Enter
  e2e_wait_for_present '(1)'
  if [[ "$(e2e_capture_pane)" == *'(1) preview context'* ]]; then
    e2e_fail 'preview context unexpectedly contains a line number'
  fi
  e2e_wait_for_present 'preview context before first result'
  e2e_wait_for_present '- alpha first result'
  e2e_wait_for_present '+ OMEGA first result'

  tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" j
  e2e_wait_for_present '+ OMEGA second result'
  tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Space
  e2e_wait_for_present '[ ] preview.txt:4'

  e2e_capture_pane > "$CAPTURE_DIR/preview-${terminal_width}x${terminal_height}.txt"
  e2e_cleanup
}

cleanup() {
  e2e_cleanup
  restore_standard_init
}
trap cleanup EXIT INT TERM

run_size 160 45
run_size 100 30

run_highlighting() {
  local label="$1"
  local disabled="$2"

  restore_standard_init
  if [[ "$disabled" == true ]]; then
    printf '%s\n' "(scooter-set! 'syntax-highlighting #f)" >> "$XDG_CONFIG_HOME/helix/init.scm"
  fi

  TMUX_SOCKET="scooter-sh2-${label}-${PPID}-${RANDOM}"
  TMUX_SESSION="scooter-sh2-${label}"
  PANE_TARGET="$TMUX_SESSION:0.0"
  tmux -L "$TMUX_SOCKET" new-session -d -x 160 -y 45 -s "$TMUX_SESSION" \
    -c "$RUST_PREVIEW_FIXTURE_DIR" "$HX_BINARY" preview.rs \
    || e2e_fail "could not start the ${label} highlighting session"

  e2e_wait_for_helix
  e2e_wait_for_present 'preview_context_before'
  tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter' Enter
  e2e_wait_for_present 'Search text'
  tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" 'alpha'
  e2e_wait_for_present 'Results: 1 [Search complete]'
  tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" Tab 'OMEGA' Enter
  e2e_wait_for_present 'pub fn preview_context_before'
  e2e_wait_for_present '+ let OMEGA = number'

  local capture
  capture="$(e2e_capture_pane_with_style)"
  if [[ "$disabled" == true ]]; then
    if ! printf '%s\n' "$capture" | perl -CS -e '
      my @lines = <STDIN>;
      for my $line (@lines) {
        next unless index($line, q{pub fn preview_context_before}) >= 0;
        my $plain = $line;
        $plain =~ s/\e\[[0-9;]*m//g;
        my $at = index($plain, q{pub fn preview_context_before});
        next if $at < 2;
        my ($fg, $prefix_fg) = (q{}, q{});
        my $column = 0;
        while (length $line) {
          if ($line =~ s/^\e\[([0-9;]*)m//) {
            my @codes = split /;/, $1;
            for (my $i = 0; $i <= $#codes; $i++) {
              if ($codes[$i] == 0 || $codes[$i] == 39) { $fg = q{} }
              elsif ($codes[$i] == 38 && $codes[$i + 1] == 2) { $fg = join q{;}, @codes[$i .. $i + 4]; $i += 4 }
              elsif ($codes[$i] >= 30 && $codes[$i] <= 37) { $fg = $codes[$i] }
            }
            next;
          }
          my $character = substr($line, 0, 1, q{});
          if ($column == $at - 1) { $prefix_fg = $fg }
          if ($column == $at) {
            exit($fg eq $prefix_fg ? 0 : 1);
          }
          $column++;
        }
      }
      exit 1;
    '; then
      e2e_fail 'disabled syntax highlighting still changed the Rust context foreground'
    fi
  else
    if ! printf '%s\n' "$capture" | perl -CS -e '
      my @lines = <STDIN>;
      my ($editor_bg, $surface_bg, $preview_bg, $plain_fg, $scope_fg, $italic_comment);
      for my $line (@lines) {
        $italic_comment ||= $line =~ /\e\[[0-9;]*3[0-9;]*m[^\n]*\/\//;
        my $raw = $line;
        $raw =~ s/\e\[[0-9;]*m//g;
        my ($fg, $bg, $column) = (q{}, q{}, 0);
        my ($at_context, $at_title) = (index($raw, q{pub fn preview_context_before}), index($raw, q{Search text}));
        while (length $line) {
          if ($line =~ s/^\e\[([0-9;]*)m//) {
            my @codes = split /;/, $1;
            for (my $i = 0; $i <= $#codes; $i++) {
              if ($codes[$i] == 0) { ($fg, $bg) = (q{}, q{}) }
              elsif ($codes[$i] == 39) { $fg = q{} }
              elsif ($codes[$i] == 49) { $bg = q{} }
              elsif ($codes[$i] == 38 && $codes[$i + 1] == 2) { $fg = join q{;}, @codes[$i .. $i + 4]; $i += 4 }
              elsif ($codes[$i] == 48 && $codes[$i + 1] == 2) { $bg = join q{;}, @codes[$i .. $i + 4]; $i += 4 }
              elsif ($codes[$i] >= 30 && $codes[$i] <= 37) { $fg = $codes[$i] }
              elsif ($codes[$i] >= 40 && $codes[$i] <= 47) { $bg = $codes[$i] }
            }
            next;
          }
          if ($at_context >= 2 && $column == $at_context - 1) { $plain_fg = $fg }
          if ($at_context >= 0 && $column == $at_context) { ($scope_fg, $preview_bg) = ($fg, $bg) }
          if ($at_title >= 0 && $column == $at_title) { $surface_bg = $bg }
          if ($column == 0 && !defined $editor_bg) { $editor_bg = $bg }
          substr($line, 0, 1, q{});
          $column++;
        }
      }
      exit 1 unless defined $scope_fg && defined $plain_fg && $scope_fg ne $plain_fg;
      exit 1 unless defined $preview_bg && defined $editor_bg && $preview_bg eq $editor_bg;
      exit 1 unless defined $surface_bg && $preview_bg ne $surface_bg;
      exit 1 unless $italic_comment;
    '; then
      e2e_fail 'Rust preview SGR foreground/background/italic-comment assertion failed'
    fi
  fi
  printf '%s' "$capture" > "$CAPTURE_DIR/preview-highlight-${label}.ansi"
  e2e_cleanup
}

run_highlighting enabled false
run_highlighting disabled true

# A large Rust fixture exercises repeated full-file cache hits while moving
# quickly through results. The normal e2e waits are the responsiveness guard.
for number in $(seq 1 400); do
  printf 'pub fn item_%s() { let alpha = %s; }\n' "$number" "$number"
done > "$RUST_PREVIEW_FIXTURE_DIR/large.rs"
restore_standard_init
TMUX_SOCKET="scooter-sh2-navigation-${PPID}-${RANDOM}"
TMUX_SESSION="scooter-sh2-navigation"
PANE_TARGET="$TMUX_SESSION:0.0"
tmux -L "$TMUX_SOCKET" new-session -d -x 160 -y 45 -s "$TMUX_SESSION" \
  -c "$RUST_PREVIEW_FIXTURE_DIR" "$HX_BINARY" large.rs \
  || e2e_fail 'could not start the large Rust preview session'
e2e_wait_for_helix
e2e_wait_for_present 'item_1'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" ':scooter' Enter
e2e_wait_for_present 'Search text'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" 'alpha' Enter
e2e_wait_for_present 'Results: 401 [Search complete]'
tmux -L "$TMUX_SOCKET" send-keys -t "$PANE_TARGET" j j j j j j j j j j k k k k k k k k k k
e2e_wait_for_present 'large.rs:'
e2e_cleanup
