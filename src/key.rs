//! Translation between the portable Steel key contract and scooter-core keys.

use scooter_core::keyboard::{KeyCode, KeyEvent, KeyModifiers};

const SHIFT_MODIFIER: usize = 1;
const CONTROL_MODIFIER: usize = 2;
const ALT_MODIFIER: usize = 4;

/// Decode the S1 key-name table into a canonical scooter-core key event.
pub(crate) fn decode(code: &str, modifiers: usize) -> Option<KeyEvent> {
    let code = match code {
        "esc" => KeyCode::Esc,
        "enter" => KeyCode::Enter,
        "tab" => KeyCode::Tab,
        "backspace" => KeyCode::Backspace,
        "left" => KeyCode::Left,
        "right" => KeyCode::Right,
        "up" => KeyCode::Up,
        "down" => KeyCode::Down,
        "home" => KeyCode::Home,
        "end" => KeyCode::End,
        "pageup" => KeyCode::PageUp,
        "pagedown" => KeyCode::PageDown,
        "delete" => KeyCode::Delete,
        _ => {
            let mut chars = code.chars();
            let character = chars.next()?;
            if chars.next().is_some() {
                return None;
            }
            KeyCode::Char(character)
        }
    };

    let mut modifier_flags = KeyModifiers::NONE;
    if modifiers & SHIFT_MODIFIER != 0 {
        modifier_flags.insert(KeyModifiers::SHIFT);
    }
    if modifiers & CONTROL_MODIFIER != 0 {
        modifier_flags.insert(KeyModifiers::CONTROL);
    }
    if modifiers & ALT_MODIFIER != 0 {
        modifier_flags.insert(KeyModifiers::ALT);
    }

    let mut event = KeyEvent::new(code, modifier_flags);
    event.canonicalize();
    Some(event)
}

#[cfg(test)]
mod tests {
    use scooter_core::keyboard::{KeyCode, KeyModifiers};

    use super::decode;

    #[test]
    fn decodes_named_keys_and_modifier_bits() {
        let event = decode("pageup", 2 | 4).expect("valid key");
        assert_eq!(event.code, KeyCode::PageUp);
        assert_eq!(event.modifiers, KeyModifiers::CONTROL | KeyModifiers::ALT);
    }

    #[test]
    fn canonicalizes_shifted_character_keys() {
        let event = decode("A", 1).expect("valid key");
        assert_eq!(event.code, KeyCode::Char('A'));
        assert_eq!(event.modifiers, KeyModifiers::NONE);
    }

    #[test]
    fn rejects_unknown_multicharacter_codes() {
        assert!(decode("unknown", 0).is_none());
    }
}
