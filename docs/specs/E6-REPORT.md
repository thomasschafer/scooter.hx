# E6 report: pump and session lifecycle hardening

## Delivered

- `Scooter-pump` and `Scooter-handle-key` now return `(status action...)`.
  `Event::LaunchEditor` is retained as an `("open-file" path line)` action,
  including through post-key draining and when the window was hidden. Steel
  consumes and logs those actions until H3 connects them to Helix.
- The engine queues launch actions independently of the bounded event drain,
  so a `DRAIN_LIMIT` boundary cannot discard them.
- Runtime ownership is optional and `quit` now signals cancellation before
  taking it through Tokio's `shutdown_background()`. The idempotent engine
  drop path uses the same shutdown, keeping the Helix UI thread non-blocking.
- Added lifecycle coverage for hidden search completion, hidden toast expiry,
  reset isolation from stale search events, deferred editor actions, clean
  teardown, the complete `busy?` state matrix, and post-panic FFI recovery.
- Popup border runs use the new `popup-border` tag while titles retain the
  popup tag. Steel maps it to `diff.plus` with explicit foreground/background
  colours. The e2e harness now uses `catppuccin_mocha` and verifies only the
  popup's own top-border span before its title.
- Added `scripts/e2e-lifecycle.sh`: it builds a disposable 2,000-file fixture,
  verifies search completion after hide/resume, reset, prompt `C-c` teardown,
  and a new empty session.

## Deviations and upstream needs

None. `App::reset()` replaces its event channels and background receiver; the
new mid-search reset test confirms that old background events cannot populate
the fresh state. No files in `../scooter` were modified.

## Validation

Passed on the final uncommitted tree:

- `scripts/check.sh` — build, clippy with `-D warnings`, and 33 tests.
- Each command passed twice consecutively:
  - `scripts/e2e-smoke.sh`
  - `scripts/e2e-live-search.sh`
  - `scripts/e2e-sizes.sh`
  - `scripts/e2e-preview.sh`
  - `scripts/e2e-replace.sh`
  - `scripts/e2e-lifecycle.sh`

