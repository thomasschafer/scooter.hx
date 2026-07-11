# PAR1: TUI parity sweep — restore every reachable behaviour

Read `docs/REWRITE-PLAN.md` for context. This backlog comes from a systematic walk of scooter-core's full command/keymap surface (`commands.rs`, `config/keys.rs`, `app.rs` special cases) against the plugin's key path. Work items in order with the usual loop. `../scooter` and the helix checkout are read-only.

## 1. (CRITICAL) The escape-sequences toggle is unreachable: A-e collision

Core's default `keys.search.toggle_interpret_escape_sequences` is `A-e` (keys.rs:171). The engine's background-open feature intercepts alt+<first open_in_editor char> — `A-e` with defaults — in results focus (converted to background open) AND swallows it in fields focus (`should_ignore_background_open_key`). Net effect: the escape-sequences toggle can never be triggered by keyboard anywhere in the plugin, and its toast/behaviour is only reachable via `scooter-set!`.

Fix (all parts):
- Background open becomes a first-class configurable plugin binding: wire path `keys.plugin.open_in_editor_bg`, configurable via the existing `scooter-keys!` ("plugin.open_in_editor_bg"), default `A-o`. It no longer derives from the foreground binding.
- It participates in conflict detection at engine creation: if the configured chord collides with ANY core binding that is reachable on the search screen (walk the config's general + search sections), engine creation fails with the same style of human-readable error as core's own conflicts.
- Remove the fields-focus swallow entirely: in fields focus the chord simply forwards to core like any other key (core ignores unmapped alt-chords in text fields).
- The interception in results focus applies ONLY to the configured bg-open chord; every other alt-chord forwards untouched.
- Update: README (D1 regenerates the config table — add the new spec row to `OPTION_SPECS`/keys docs as appropriate), and the e2e that exercised `A-e`-adjacent behaviour.
- Tests: `A-e` now toggles escape sequences (toast text) from BOTH focus states, matching the TUI; `A-o` background-opens from results focus; a rebound `plugin.open_in_editor_bg` works; a deliberately colliding binding errors at creation.

## 2. (HIGH) Paste does nothing

The old plugin handled Helix paste events (bracketed paste / cmd-v inside Helix); the rewrite's event handler recognises only key events, so pasting into the search/replace fields is silently dropped. Fix: in `ui/window.scm`, detect `paste-event?` and pass `paste-event-string` through a new FFI (`Scooter-paste (engine text)`) that inserts into the currently focussed text field via the App: newlines stripped to spaces (mirror the old plugin's `strip-newlines`), no-op when a checkbox is focussed or a popup is open... verify what core does — prefer routing through `App::handle_key_event` char-by-char ONLY if there is no better core path; check whether `SearchFields`' highlighted field exposes `insert_text` mutably through public API (`TextField::insert_text` is pub; `highlighted_field_mut` is pub) and use that directly inside the runtime guard, matching the TUI-adjacent behaviour (the debounce must still fire — mimic what `enter_chars_into_field` does around field mutation, or route through a synthesized EnterChars per char if that is the only correct way to trigger the search scheduling; correctness of the debounce path matters more than elegance).
- e2e: tmux `load-buffer`/`paste-buffer` with bracketed paste into the search field; assert the pasted text lands and the search fires.

## 3. (MED) META/SUPER modifiers are masked off

The wire contract masks modifiers to shift|ctrl|alt (`(bitwise-and ... 7)`), but core's `TextField::handle_keys` maps META+backspace -> delete-to-start and META+right / End -> end-of-line (cmd on macOS in CSI-u terminals). Extend the contract: pass super and meta bits through (check helix's modifier bit values in the components API/keyboard.rs of the helix checkout; core's `KeyModifiers` has SUPER/META — map helix's bits to core's correctly in `key::decode`). Update the contract comment in both window.scm and key.rs, plus decode tests.

## 4. (MED) Discoverability of the background-open binding

The help popup and footer come from `App::keymaps_*` and cannot know about the plugin-only binding. Inject a row at the engine level: when the results section is focussed (and the binding is active), append `<A-o> open in background` (rendering the ACTUAL configured chord) to the full help list, adjacent to core's `open in editor` row. Footer/compact list: leave as-is (space is tight; TUI shows open-in-editor only in the full list too — verify and mirror placement).

## 5. (LOW) Contract test: every core default binding reaches core

Add an engine test that iterates every default binding in `KeysConfig::default()` (all sections) and asserts `handle_key` forwards it to core — i.e. the engine's interception layer touches ONLY: esc (hide semantics, per the documented rules) and the configured bg-open chord in results focus. This pins the interception surface so future plugin shortcuts cannot silently eat core bindings again (this is exactly how item 1's bug slipped in).

## 6. (LOW) Config-matrix e2e

New `scripts/e2e-matrix.sh`: one session configured with wrap-text on, syntax-highlighting off, multiline on, `advanced-regex` on, and a rebound `move_down`; drive: a multiline (`\n`-spanning) advanced-regex search (use a lookahead to prove the advanced engine), navigate with the rebound key, verify wrapped long preview lines and no scope-styled runs (syntax off), toggle escape sequences via `A-e` and verify the replacement preview updates. Pass twice consecutively. This is the standing net for option-wiring gaps.

## 7. Upstream note (report only — do not modify ../scooter)

Core panics on `OpenInEditor` with zero results (`primary_selected_field_mut().expect(...)` in app.rs's `CommandSearchFocusResults::OpenInEditor` arm) — the TUI would crash on `e` with an empty, focussed results list; the plugin's FFI guard already contains it. Include in the report a proposed one-line upstream patch (guard the None case) for the maintainer to fold into the scooter PR.

## Sweep confirmations (verify, fix if wrong, then list in the report)

Walk and confirm each reaches core correctly, noting any failure as an extra fix: A-m multiline toggle (both focus states), C-t hidden files, C-l wrap, A-u unlock (harmless no-op), A-; flip multiselect, v multiselect, esc exits multiselect before back-to-fields, g/G with shift canonicalisation, C-f/C-b/pageup/pagedown/C-d/C-u paging, C-n/C-p movement, enter on incomplete search -> "Search still in progress" error popup, C-h help on every screen incl. Results, C-c quit precedence over popups, C-r reset on every screen, results-screen j/k error scrolling and q/enter quit, checkbox space toggling and the fixed-strings error-clearing path, tab/S-tab field cycling wraparound.

## Working loop

Per item: `HELIX_RUNTIME=~/Development/helix/runtime scripts/check.sh` + relevant e2e. Finish with the full suite twice (including the new matrix script) and a fresh-eyes tmux pass exercising item 1's fix under default config. Snapshot diffs justified per item. Write `docs/specs/PAR1-REPORT.md` with per-item evidence, the sweep confirmation table, and the upstream patch proposal. Do not commit.
