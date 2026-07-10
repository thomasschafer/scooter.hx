#!/usr/bin/env bash
# Shared, isolated environment for scooter.hx end-to-end checks.

set -euo pipefail

REPO="$(git rev-parse --show-toplevel)"
export REPO

export STEEL_HOME="$REPO/.dev/steel-home"
export XDG_CONFIG_HOME="$REPO/.dev/config"
export XDG_CACHE_HOME="$REPO/.dev/cache"
export HELIX_RUNTIME="$HOME/Development/helix/runtime"
export FIXTURE_DIR="$REPO/.dev/fixtures/basic"
export SEARCH_FIXTURE_DIR="$REPO/.dev/fixtures/search"
export PREVIEW_FIXTURE_DIR="$REPO/.dev/fixtures/preview"
export LIFECYCLE_FIXTURE_DIR="$REPO/.dev/fixtures/lifecycle"
export HX_BINARY="$HOME/Development/helix/target/release/hx"

if [[ ! -d "$STEEL_HOME/cogs/helix" ]]; then
  printf '%s\n' "Missing generated Helix Steel modules at $STEEL_HOME/cogs/helix." >&2
  printf '%s\n' "Run: STEEL_HOME=$REPO/.dev/steel-home cargo xtask code-gen" >&2
  printf '%s\n' "from the Helix checkout, then rerun this script." >&2
  return 1 2>/dev/null || exit 1
fi

mkdir -p "$XDG_CONFIG_HOME/helix" "$XDG_CACHE_HOME" "$FIXTURE_DIR" "$SEARCH_FIXTURE_DIR" "$PREVIEW_FIXTURE_DIR" "$LIFECYCLE_FIXTURE_DIR"

printf '(require "%s")\n' "$REPO/scooter.scm" > "$XDG_CONFIG_HOME/helix/init.scm"
# A themed popup exposes accidental inheritance between the field, popup, and
# border runs. Plain pane captures below remain independent of this setting.
printf '%s\n' 'theme = "catppuccin_mocha"' > "$XDG_CONFIG_HOME/helix/config.toml"
printf '%s\n' 'alpha: first fixture line' 'alpha: second fixture line' > "$FIXTURE_DIR/alpha.txt"
printf '%s\n' 'bravo: a separate fixture' 'bravo: another fixture line' > "$FIXTURE_DIR/bravo.txt"
printf '%s\n' '# Scooter S1 fixture' 'static, deterministic content' > "$FIXTURE_DIR/README.md"

printf '%s\n' 'alpha one' 'alphabet one' > "$SEARCH_FIXTURE_DIR/one.txt"
printf '%s\n' 'alpha two' 'alphabet two' > "$SEARCH_FIXTURE_DIR/two.txt"
printf '%s\n' 'alpha three' > "$SEARCH_FIXTURE_DIR/three.txt"
printf '%s\n' '# Scooter E3 fixture' 'static, deterministic content' > "$SEARCH_FIXTURE_DIR/README.md"

printf '%s\n' \
  'preview context before first result' \
  'alpha first result' \
  'preview context between results' \
  'alpha second result' \
  'preview context after second result' > "$PREVIEW_FIXTURE_DIR/preview.txt"

e2e_capture_pane() {
  tmux -L "$TMUX_SOCKET" capture-pane -p -t "$PANE_TARGET" -S - 2>/dev/null || true
}

e2e_capture_pane_with_style() {
  tmux -L "$TMUX_SOCKET" capture-pane -e -p -t "$PANE_TARGET" -S - 2>/dev/null || true
}

e2e_assert_popup_border_has_uniform_style() {
  local title="$1"
  local capture
  capture="$(e2e_capture_pane_with_style)"

  if ! printf '%s\n' "$capture" | perl -CS -e '
    my $title = shift;
    while (my $line = <STDIN>) {
      next unless index($line, $title) >= 0;
      my $title_at = index($line, $title);
      my $before_title = substr($line, 0, $title_at);
      # Find this popup top-left corner, rather than every style sequence
      # from the terminal row. tmux may retain DEC special-graphics controls
      # in an ANSI capture, hence the fallback for `ESC ( 0 l`.
      my $corner = rindex($before_title, "\x{250c}");
      $corner = rindex($before_title, "\e(0l") if $corner < 0;
      if ($corner < 0) {
        print STDERR "could not locate popup top-left corner\n";
        exit 1;
      }
      my @style_starts;
      while ($before_title =~ /(?:\e\[[0-9;]*m)+/g) {
        push @style_starts, $-[0];
      }
      my ($style_start) = reverse grep { $_ <= $corner } @style_starts;
      if (!defined $style_start) {
        print STDERR "could not locate popup border style\n";
        exit 1;
      }
      # The title intentionally retains the popup/content style, so drop the
      # SGR sequence emitted immediately before it. From the popup corner
      # through the cell before that title style, a uniform border is exactly
      # one contiguous SGR sequence.
      my $border_end = $before_title;
      $border_end =~ s/(?:\e\[[0-9;]*m)+$//;
      my $span = substr($line, $style_start, length($border_end) - $style_start);
      my @prefixes = ($span =~ /((?:\e\[[0-9;]*m)+)/g);
      if (@prefixes != 1) {
        print STDERR "popup border style changed within its span\n";
        exit 1;
      }
      exit 0;
    }
    print STDERR "could not locate popup title $title\n";
    exit 1;
  ' "$title"; then
    e2e_fail "popup '$title' top border does not have one uniform SGR style"
  fi
}

e2e_print_diagnostics() {
  printf '%s\n' '--- tmux pane ---' >&2
  e2e_capture_pane >&2
  printf '%s\n' '--- Helix log ---' >&2
  if [[ -f "$XDG_CACHE_HOME/helix/helix.log" ]]; then
    sed -n '1,240p' "$XDG_CACHE_HOME/helix/helix.log" >&2
  else
    printf '%s\n' '(no Helix log found)' >&2
  fi
}

e2e_fail() {
  printf '%s\n' "e2e test failed: $*" >&2
  e2e_print_diagnostics
  exit 1
}

e2e_cleanup() {
  tmux -L "$TMUX_SOCKET" kill-session -t "$TMUX_SESSION" 2>/dev/null || true
}

e2e_wait_for_present() {
  local needle="$1"
  local deadline=$((SECONDS + 15))

  while (( SECONDS < deadline )); do
    if [[ "$(e2e_capture_pane)" == *"$needle"* ]]; then
      return 0
    fi
    sleep 0.2
  done

  e2e_fail "timed out waiting for '$needle'"
}

e2e_wait_for_absent() {
  local needle="$1"
  local deadline=$((SECONDS + 10))

  while (( SECONDS < deadline )); do
    if [[ "$(e2e_capture_pane)" != *"$needle"* ]]; then
      return 0
    fi
    sleep 0.2
  done

  e2e_fail "timed out waiting for '$needle' to disappear"
}

e2e_wait_for_helix() {
  local deadline=$((SECONDS + 15))

  while (( SECONDS < deadline )); do
    if [[ "$(tmux -L "$TMUX_SOCKET" display-message -p -t "$PANE_TARGET" '#{pane_current_command}' 2>/dev/null || true)" == hx ]]; then
      return 0
    fi
    sleep 0.2
  done

  e2e_fail 'timed out waiting for Helix to start'
}
