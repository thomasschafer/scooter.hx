# E4 results preview report

## Delivered

- Replaced the S2 result rows with the native list/preview split. It uses the
  TUI breakpoint: a 2/5-width list beside the preview above 110 columns, and
  a five-row list above the preview at or below it.
- Result rows now show inclusion, a tail-truncated path, and the line number;
  primary selection uses `selection`, multiselect range entries use
  `selection-secondary`, and unselected line numbers use `info`.
- Added plain-text, synchronous previews with numbered context, file-change
  guarding, control-character stripping, single-line word diff emphasis, and
  multiline detail up to 20,000 bytes. Larger multiline replacements use the
  requested simple `-`/`+` fallback. There is deliberately no highlight task
  or cache.
- Wrapping preserves styled segments and prefixes continuation lines with
  `↪ `; unlike the TUI's grapheme/chunk-aware wrapper, its breakpoints are
  Unicode code points.
- Added `selection-secondary`, `diff-added-emph`, and `diff-removed-emph` to
  the Steel theme table. The latter two reverse the corresponding diff scope.
- Added render-grid and headless interaction coverage, plus
  `scripts/e2e-preview.sh` at 160x45 and 100x30.

## 160x45 results/preview capture

```text
        Results: 2 [Search complete]                                                                               [Time taken: 0.107s]

        [x] preview.txt:2                                  (1) preview context before first result
        [ ] preview.txt:4                                  (2) alpha first result
                                                          (3) preview context between results
                                                          - alpha second result
                                                          + OMEGA second result
                                                          (5) preview context after second result
```

This has the same structural arrangement as the TUI reference: result list on
the left, one-column gutter, and the numbered context plus `-`/`+` diff pane
on the right. Helix resolves the semantic tags through its active theme, so
the capture does not encode terminal colours; diff and word-emphasis runs use
the diff scopes and their reversed variants.

## Validation

- `scripts/check.sh` passed after implementation.
- E2E passes 1 and 2: `smoke`, `live-search`, `sizes`, and `preview` all
  passed consecutively in the isolated Helix/Steel environment.

No files in `../scooter` were modified, and this work is intentionally
uncommitted.
