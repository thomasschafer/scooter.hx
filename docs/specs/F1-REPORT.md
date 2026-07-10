# F1 parity fixes report

## Delivered

- Overlay styles now explicitly set foreground and background colours. `popup`
  preserves `ui.popup` colours where supplied and completes missing values from
  the active theme; the toast border does the same. This prevents
  `frame-set-string!` from retaining a colour from content underneath an
  overlay.
- Added the `Scooter` title to the outer window border, at the requested
  two-column offset.
- Preview context now has the TUI's two-space prefix and no line numbers.
- Result rows now reserve a right-aligned absolute ` (n)` index. Unselected
  marker/line/index accessories use `info`; selected rows receive a full-width
  selection fill. Selection is suppressed while fields are focussed, with the
  added primary/range excluded tags mapped from the theme's `error` colour.
- Replacement tally titles are left-aligned. Popup titles remain centred and
  popup content retains its one-cell horizontal padding, matching
  `create_popup_block`.
- Added headless coverage for the index/accessory layout, focus gating,
  full-row selection fill, and both excluded selection variants. The e2e
  replacement flow now opens the deterministic regex error popup and checks
  its ANSI capture for a single contiguous border style.

## Captures

Wide results view (`scripts/e2e-preview.sh`, 160x45):

```text
    1  preview context before first result
    2  alpha first result
    3  p┌─ Scooter ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    4  a│       ┌─Search text─────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐        │
    5  p│       │alpha                                                                                                                        │        │
    ~   │       └─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘        │
        │       ┌─Replace text────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐        │
        │       │OMEGA                                                                                                                        │        │
        │       └─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘        │
        │                                                                                                                                              │
        │       Results: 2 [Search complete]                                                                               [Time taken: 0.053s]        │
        │                                                                                                                                              │
        │       [x] preview.txt:2                              (1)   preview context before first result                                               │
        │       [ ] preview.txt:4                              (2)   alpha first result                                                                │
        │                                                            preview context between results                                                   │
        │                                                          - alpha second result                                                               │
        │                                                          + OMEGA second result                                                               │
        │                                                            preview context after second result                                               │
        │                                                                                                                                              │
        │                      <space> toggle / <esc> back to search fields / <enter> replace selected / <C-h> help / <C-c> quit                       │
        └──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
```

Error popup (`scripts/e2e-replace.sh`):

```text
    1  alpha one
    2  alphabet one
    ~ ┌─ Scooter ────────────────────────────────────────────────────────────────────────────────────────────────┐
      │     ┌─Search text (Error: Couldn't parse regex)───────────────────────────────────────────────────┐      │
      │     │(                                                                                            │      │
      │     └─────────────────────────────────────────────────────────────────────────────────────────────┘      │
      │     ┌─Replace text────────────────────────────────────────────────────────────────────────────────┐      │
      │     │                                                                                             │      │
      │     └─────────────────────────────────────────────────────────────────────────────────────────────┘      │
      │     ┌───┐                                                                                                │
      │     │   │ Fixed strings                                                                                  │
      │     └───┘                                                                                                │
      │     ┌───┐                                                                                                │
      │     │   │ Match whole word                                                                               │
      │     └───┘                                                                                                │
      │     ┌───┐                                                                                                │
      │     │ X┌─────────────────────────────────────────Errors─────────────────────────────────────────┐        │
      │     └──│ Search text                                                                            │        │
      │     ┌─F│ regex parse error:                                                                     │─┐      │
      │     │  │     (                                                                                  │ │      │
      │     └──│     ^                                                                                  │─┘      │
      │     ┌─F│ error: unclosed group                                                                  │─┐      │
      │     │  └────────────────────────────────────────────────────────────────────────────────────────┘ │      │
      │     └─────────────────────────────────────────────────────────────────────────────────────────────┘      │
      │                                                                                                          │
      │     Results: 0 [Invalid search]                                                                          │
      │                                                                                                          │
      │            <enter> jump to results / <tab> focus next / <C-r> reset / <C-h> help / <C-c> quit            │
      └──────────────────────────────────────────────────────────────────────────────────────────────────────────┘
```

The styled `tmux capture-pane -e` companion capture passed the uniform-border
assertion. It is generated under `.dev/e2e-captures/` by the e2e script and is
not tracked.

## Validation

- `scripts/check.sh` passed: build, clippy with `-D warnings`, and 16 tests.
- `e2e-smoke.sh`, `e2e-live-search.sh`, `e2e-sizes.sh`, `e2e-preview.sh`, and
  `e2e-replace.sh` each passed twice consecutively.
- No files in `../scooter` were modified. This work is intentionally
  uncommitted.
