# E7 report: headless frame snapshot suite

## Delivered

- Added `insta` 1.47.2 with the `filters` feature (resolving to the same 1.48.0 lockfile version as `../scooter`).
- Added `src/snapshot_tests.rs`, a headless suite that renders `Frame` values into a fixed character canvas and compact per-row semantic-tag legend. Cursor coordinates are appended when present; multiline control characters are shown visibly (`↵`, `␍`, `⇥`) so every run remains reviewable in a one-row canvas.
- Added 26 snapshots under `src/snapshots/`, covering fresh/populated fields, field errors and popup close, completed searches at narrow/wide sizes and both focus states, all requested preview states, multiselect variants, help, toast, replacement progress/results, and empty/pending banners.
- Normalised time-taken, replacement elapsed/progress, and temporary-directory paths with insta filters. All search fixtures contain one file, and completed searches are pumped before snapshotting. The pending-debounce snapshot is the explicit exception: it snapshots the deterministic state before its timer is pumped.

No renderer defect was found and no production rendering code changed. Existing loose renderer tests were retained; no tests were removed.

## Validation

- `scripts/check.sh` — passed (build, strict clippy, 44 tests including snapshots).
- `scripts/e2e-smoke.sh` — passed.
- `scripts/e2e-live-search.sh` — passed.
- `scripts/e2e-sizes.sh` — passed.
- `scripts/e2e-preview.sh` — passed.
- `scripts/e2e-replace.sh` — passed.
- `scripts/e2e-lifecycle.sh` — passed.

No commits were created and `../scooter` was not modified.
