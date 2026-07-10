# E5 report: overlays and replacement screens

Implemented the E5 screen set:

- A centred, truncated key-hint footer reserves the final content row.
- Error, help, and text overlays clear their rectangular background with the
  `popup` style, then draw their border and content above the active screen.
- Toasts use a green diff border and dismiss through the existing poll path.
- Performing-replacement progress and final success/error results screens are
  rendered from the core replacement states.
- Added `scripts/e2e-replace.sh`, which uses a fresh fixture copy per run and
  validates overlays, toast expiry, replacement, quit, and fresh-session
  behaviour.

The render-grid coverage now includes every E5 overlay/screen state. Headless
coverage also exercises popup Esc handling, toast dismissal via `pump`, a full
on-disk replacement flow, result errors, and Esc on replacement/results
screens.

## Validation

- `scripts/check.sh` passed (build, clippy with `-D warnings`, 13 tests).
- Each e2e script passed twice consecutively on the final tree:
  `e2e-smoke.sh`, `e2e-live-search.sh`, `e2e-sizes.sh`, `e2e-preview.sh`, and
  `e2e-replace.sh`.

## Captures

Help popup (`scripts/e2e-replace.sh`):

```text
    1  alpha one
    2  alphabet one
    ~ ┌──────────────────────────────────────────────────────────────────────────────────────────────────────────┐
      │     ┌─Search text─────────────────────────────────────────────────────────────────────────────────┐      │
      │     │                                                                                             │      │
      │     └─────────────────────────────────────────────────────────────────────────────────────────────┘      │
      │     ┌─Replace text────────────────────────────────────────────────────────────────────────────────┐      │
      │     │                                                                                             │      │
      │     └─────────────────────────────────────────────────────────────────────────────────────────────┘      │
      │     ┌───┐                                                                                                │
      │     │   │ Fixed strings                                                                                  │
      │     └───┘                                                                                                │
      │     ┌──┌──────────────────────────────────────────Help──────────────────────────────────────────┐        │
      │     │  │ <enter> jump to results                                                                │        │
      │     └──│   <tab> focus next                                                                     │        │
      │     ┌──│ <S-tab> focus previous                                                                 │        │
      │     │ X│ <space> toggle checkbox                                                                │        │
      │     └──│   <A-u> unlock pre-populated fields                                                    │        │
      │     ┌─F│   <C-l> toggle text wrapping in preview                                                │─┐      │
      │     │  │   <C-t> toggle hidden files                                                            │ │      │
      │     └──│   <A-m> toggle multiline                                                               │─┘      │
      │     ┌─F│   <A-e> toggle escape sequences                                                        │─┐      │
      │     │  │   <C-r> reset                                                                          │ │      │
      │     └──│   <C-h> help                                                                           │─┘      │
      │        │   <esc> close popup                                                                    │        │
      │     Res│   <C-c> quit                                                                           │        │
      │        └────────────────────────────────────────────────────────────────────────────────────────┘        │
      │                                                                                                          │
      │                                                                                                          │
      │            <enter> jump to results / <tab> focus next / <C-r> reset / <C-h> help / <C-c> quit            │
      └──────────────────────────────────────────────────────────────────────────────────────────────────────────┘
 NOR   one.txt                                                                                               1 sel  1:1
```

Replacement results (`scripts/e2e-replace.sh`):

```text
    1  alpha one
    2  alphabet one
    ~ ┌──────────────────────────────────────────────────────────────────────────────────────────────────────────┐
      │                                                                                                          │
      │                                                                                                          │
      │                                                                                                          │
      │                                                                                                          │
      │                                                                                                          │
      │                                                                                                          │
      │                                                                                                          │
      │                                                                                                          │
      │                                                                                                          │
      │                                                                                                          │
      │                                                Success!                                                  │
      │     ┌──────────────────────────────Successful replacements (lines):───────────────────────────────┐      │
      │     │5                                                                                            │      │
      │     └─────────────────────────────────────────────────────────────────────────────────────────────┘      │
      │     ┌──────────────────────────────────────Ignored (lines):───────────────────────────────────────┐      │
      │     │0                                                                                            │      │
      │     └─────────────────────────────────────────────────────────────────────────────────────────────┘      │
      │     ┌───────────────────────────────────────────Errors:───────────────────────────────────────────┐      │
      │     │0                                                                                            │      │
      │     └─────────────────────────────────────────────────────────────────────────────────────────────┘      │
      │                                                                                                          │
      │                                                                                                          │
      │                                  <C-r> reset / <C-h> help / <C-c> quit                                   │
      └──────────────────────────────────────────────────────────────────────────────────────────────────────────┘
 NOR   one.txt                                                                                               1 sel  1:1
```
