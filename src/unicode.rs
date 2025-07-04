use unicode_width::UnicodeWidthStr;

pub fn display_width(s: &str) -> usize {
    UnicodeWidthStr::width(s)
}

pub fn truncate_to_width(s: &str, max_width: usize) -> String {
    if max_width == 0 {
        return String::new();
    }

    let mut width = 0;
    let mut result = String::new();

    for ch in s.chars() {
        let ch_width = unicode_width::UnicodeWidthChar::width(ch).unwrap_or(0);
        if width + ch_width > max_width {
            break;
        }
        width += ch_width;
        result.push(ch);
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display_width() {
        assert_eq!(display_width("hello"), 5);
        assert_eq!(display_width("😆"), 2);
        assert_eq!(display_width("😆😆😆"), 6);
        assert_eq!(display_width("hello😆world"), 12);
        assert_eq!(display_width("你好"), 4); // CJK characters
    }

    #[test]
    fn test_truncate_to_width() {
        assert_eq!(truncate_to_width("hello", 3), "hel");
        assert_eq!(truncate_to_width("😆😆😆", 4), "😆😆");
        assert_eq!(truncate_to_width("😆😆😆", 5), "😆😆");
        assert_eq!(truncate_to_width("hello😆world", 7), "hello😆");
        assert_eq!(truncate_to_width("abc", 10), "abc");
        assert_eq!(truncate_to_width("", 10), "");
        assert_eq!(truncate_to_width("anything", 0), "");
    }
}
