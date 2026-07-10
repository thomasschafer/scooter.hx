# E3 implementation report

## Delivered

- Reworked the fields renderer for TUI-fidelity text fields: title and error are adjacent semantic runs in the top border, while borders retain their active/text styling.
- Rendered checkbox fields as a fixed 5×3 bordered control containing ` X ` when selected, with a one-cell-gapped, vertically centred plain title beside it.
- Matched the search banner structure: split left status styling, right-aligned elapsed time, optional preview-progress middle text, correct empty/pre-debounce behaviour, and an invalid-search banner when the core exposes a field error before creating a search state.
- Clamped both exported cursor paths to the text field interior, and hardened small-frame layout down to 0×0.
- Added a property-style render grid test for frame bounds and non-overlap, plus headless coverage for checkbox toggling, invalid-regex error/title/banner rendering, and field collapse after Enter.
- Added `scripts/e2e-sizes.sh`, covering isolated 80×24, 120×40, and 220×55 Helix sessions. Captures are saved beneath the ignored `.dev/e2e-captures/` directory for visual inspection.

## Screenshot review

The captures show text-field titles in their borders and compact checkbox boxes with their titles to the right. At 80×24, the 19-row popup content fits six complete fields and its required gap, so the banner intentionally has no room; the E2E test asserts a field title instead. The larger sizes show all fields and the banner.

120×40 capture (fields and banner):

```text
    1  # Scooter E3 fixture
    2  static, deterministic content
    ~ ┌──────────────────────────────────────────────────────────────────────────────────────────────────────────┐
      │     ┌─Search text─────────────────────────────────────────────────────────────────────────────────┐      │
      │     │alpha                                                                                        │      │
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
      │     │ X │ Match case                                                                                     │
      │     └───┘                                                                                                │
      │     ┌─Files to include────────────────────────────────────────────────────────────────────────────┐      │
      │     │                                                                                             │      │
      │     └─────────────────────────────────────────────────────────────────────────────────────────────┘      │
      │     ┌─Files to exclude────────────────────────────────────────────────────────────────────────────┐      │
      │     │                                                                                             │      │
      │     └─────────────────────────────────────────────────────────────────────────────────────────────┘      │
      │                                                                                                          │
      │     Results: 5 [Search complete]                                               [Time taken: 0.053s]      │
      │                                                                                                          │
      │     > [x] one.txt:1                                                                                      │
      │       [x] one.txt:2                                                                                      │
      │       [x] three.txt:1                                                                                    │
      │       [x] two.txt:1                                                                                      │
      │       [x] two.txt:2                                                                                      │
      └──────────────────────────────────────────────────────────────────────────────────────────────────────────┘
```

## Deviations

None. The results list remains deliberately minimal because preview rendering is E4 scope.

## Upstream needs for U1

None. The existing public field and search-state accessors were sufficient; the invalid-before-search-state case is handled locally from the exposed field error.

## Validation

All of the following passed from the repository root:

```sh
scripts/check.sh && scripts/check.sh
bash -n scripts/e2e-env.sh scripts/e2e-smoke.sh scripts/e2e-live-search.sh scripts/e2e-sizes.sh
git diff --check
scripts/e2e-smoke.sh && scripts/e2e-smoke.sh
scripts/e2e-live-search.sh && scripts/e2e-live-search.sh
scripts/e2e-sizes.sh && scripts/e2e-sizes.sh
```

No commit was created and `../scooter` was not modified.
