use scooter_core::fields::TextField;

pub fn textfield_text(field: &TextField) -> String {
    field.text().to_string()
}

pub fn textfield_set_text(field: &mut TextField, text: &str) {
    field.text = text.to_string();
    field.cursor_idx = field.text.chars().count().min(field.cursor_idx);
}

pub fn textfield_insert_text(field: &mut TextField, text: &str) {
    let index = byte_index(field);
    field.text.insert_str(index, text);
    field.cursor_idx += text.chars().count();
}

pub fn textfield_cursor_idx(field: &TextField) -> usize {
    field.cursor_idx
}

pub fn textfield_set_cursor_idx(field: &mut TextField, idx: usize) {
    field.cursor_idx = clamp_cursor(field, idx);
}

// TODO: pull these methods out in scooter-core and use
fn byte_index(field: &TextField) -> usize {
    field
        .text
        .char_indices()
        .map(|(i, _)| i)
        .nth(field.cursor_idx)
        .unwrap_or(field.text.len())
}

fn clamp_cursor(field: &TextField, new_cursor_pos: usize) -> usize {
    new_cursor_pos.clamp(0, field.text.chars().count())
}
