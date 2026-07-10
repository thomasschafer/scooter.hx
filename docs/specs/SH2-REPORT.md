# SH2 report: highlighted preview integration

## Result

SH2 is complete. Preview context now uses the SH1 runtime highlight engine for
small, language-detected files. Diff lines retain their existing diff styles,
the two-character context and diff prefixes stay unhighlighted, and styled
segments survive truncation and wrapping. The preview pane alone is filled
with Helix's editor background (`ui.background`); the rest of the Scooter
window remains on `ui.popup`.

`(scooter-set! 'syntax-highlighting #f)` disables the full-file highlight path
and leaves context plain. The default is enabled.

## Preview read and cache behaviour

For a file at or below the SH1 512 KiB cap, rendering reads the complete UTF-8
file once, derives the visible line window and byte offsets from that buffer,
then sends that same buffer to the highlighter. Larger files keep the previous
windowed read and render plain. `MAX_CONTENT_BYTES` is now `pub(crate)` so the
renderer and engine share the cap. A test-only computation counter makes the
render-level cache assertion observable: a second render of the same selected
Rust result performs no additional highlight computation.

## 160x45 plain capture

Catppuccin Mocha, `preview.rs`, search `alpha`, replacement `OMEGA`:

```text
    1  pub fn preview_context_before() { let number = 42; }
    2  let alpha = number;
    3  p┌─ Scooter ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    ~   │       ┌─Search text─────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐        │
        │       │alpha                                                                                                                        │        │
        │       └─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘        │
        │       ┌─Replace text────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐        │
        │       │OMEGA                                                                                                                        │        │
        │       └─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘        │
        │                                                                                                                                              │
        │       Results: 1 [Search complete]                                                                               [Time taken: 0.057s]        │
        │                                                                                                                                              │
        │       [x] preview.rs:2                               (1)   pub fn preview_context_before() { let number = 42; }                              │
        │                                                          - let alpha = number;                                                               │
        │                                                          + let OMEGA = number;                                                               │
        │                                                            pub fn preview_context_after() -> usize { 7 }                                     │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                                                                                                                                              │
        │                      <space> toggle / <esc> back to search fields / <enter> replace selected / <C-h> help / <C-c> quit                       │
        └──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘

 NOR preview.rs                                                                                                                                    1 sel  1:1
```

## SGR evidence

The extended `e2e-preview.sh` uses `capture-pane -e` under Catppuccin Mocha
and parses the cells rather than relying on a fixed palette. In the enabled
capture, the preview `pub` scope has foreground `38;2;203;166;247`; its plain
prefix has no extra foreground patch, so it is distinct. Its background is
`48;2;30;30;46`, equal to the editor cell background. The Search text popup
surface is `48;2;49;50;68`, proving the preview background differs from the
popup surface. The disabled capture requires that the same Rust token's
foreground equals its plain context prefix. Both enabled and disabled checks
passed twice as part of the full e2e runs.

## Snapshot changes

There are three new Rust snapshots: wide, narrow, and wrapping. Their legends
contain `s:keyword`, `s:keyword.function`, `s:function`, punctuation, type,
operator, and numeric scope tags, while their old/new diff rows stay entirely
`diff-removed`/`diff-added`.

Fifteen existing snapshots changed only where a results preview exists. Their
new `preview` runs fill the otherwise unpainted right/bottom cells of that
preview rectangle; this is the intentional editor-background fill. The canvas
text, non-preview styles, and `.txt` context tags remain unchanged. Snapshots
without a preview are byte-identical.

## Validation

- `HELIX_RUNTIME=~/Development/helix/runtime scripts/check.sh` passed: build,
  Clippy with denied warnings, 65 unit/snapshot tests, and the SH0 runtime
  spike.
- Every e2e script passed twice consecutively: `e2e-config`,
  `e2e-lifecycle`, `e2e-live-search`, `e2e-open`, `e2e-preview`,
  `e2e-replace`, `e2e-sizes`, and `e2e-smoke`.
- The preview e2e additionally navigates rapidly through 400 Rust matches and
  completes under the harness's normal waits.

No commit was created. No files in `../scooter` or the Helix checkout were
modified.
