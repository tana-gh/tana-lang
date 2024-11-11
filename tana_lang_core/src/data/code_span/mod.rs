mod iter;

pub use iter::*;

use std::{
    ptr,
    str,
};

#[derive(Clone, Debug)]
pub struct CodeSpan<'a> {
    pub line: usize,
    pub column: usize,
    pub slice: &'a str,
    pub line_slice: &'a str,
}

impl<'a> PartialEq for CodeSpan<'a> {
    fn eq(&self, _other: &Self) -> bool {
        true
    }

    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}

impl<'a> CodeSpan<'a> {
    pub fn eof() -> Self {
        Self {
            line: 0,
            column: 0,
            slice: "",
            line_slice: "",
        }
    }

    pub fn is_eof(&self) -> bool {
        self.line == 0 && self.column == 0 && self.slice.is_empty()
    }

    pub fn new(line: usize, column: usize, slice: &'a str, line_slice: &'a str) -> Self {
        Self { line, column, slice, line_slice }
    }

    pub fn extend(&self, tail: &Self) -> Self {
        if self.is_eof() {
            return tail.clone();
        }
        else if tail.is_eof() {
            return self.clone();
        }
        let head = self.slice.as_ptr() as usize;
        let last = tail.slice.as_ptr() as usize;
        let len = last - head + tail.slice.len();
        let slice = unsafe { ptr::slice_from_raw_parts(self.slice.as_ptr(), len).as_ref().unwrap() };
        let s = unsafe { str::from_utf8_unchecked(slice) };
        Self {
            line: self.line,
            column: self.column,
            slice: s,
            line_slice: self.line_slice,
        }
    }

    pub fn error_header(&self) -> String {
        if self.is_eof() {
            String::new()
        }
        else {
            format!("({}, {}): ", self.line, self.column)
        }
    }

    pub fn error_footer(&self) -> String {
        if self.is_eof() {
            String::new()
        }
        else {
            format!("\n{}\n{}", self.line_slice, self.underline())
        }
    }

    fn underline(&self) -> String {
        let slice_head = self.slice.as_ptr() as usize;
        let line_head = self.line_slice.as_ptr() as usize;
        let spaces = (0 .. (slice_head - line_head)).map(|_| ' ').collect::<String>();
        let underline =
            if spaces.len() + self.slice.len() < self.line_slice.len() {
                (0 .. self.slice.len()).map(|_| '^').collect::<String>()
            }
            else if spaces.len() < self.line_slice.len() {
                (0 .. self.line_slice.len() - spaces.len()).map(|_| '^').collect::<String>()
            }
            else {
                String::new()
            };
        format!("{}{}", spaces, underline)
    }

    pub fn strict_eq(&self, other: &Self) -> bool {
        self.line == other.line &&
        self.column == other.column &&
        self.slice == other.slice
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extend() {
        let s = "abcdefgh";
        let span1 = CodeSpan::new(1, 1, &s[0..3], s);
        let span2 = CodeSpan::new(1, 4, &s[3..6], s);
        let span3 = CodeSpan::new(1, 1, &s[0..6], s);
        assert!(span1.extend(&span2).strict_eq(&span3));
    }

    #[test]
    fn test_extend_with_eof() {
        let s = "abcdefgh";
        let span1 = CodeSpan::new(1, 1, &s[0..3], s);
        let span2 = CodeSpan::eof();
        assert!(span1.extend(&span2).strict_eq(&span1));
        assert!(span2.extend(&span1).strict_eq(&span1));
    }

    #[test]
    fn test_error_footer() {
        let s = "abcdefgh";
        let span1 = CodeSpan::new(1, 1, &s[0..3], s);
        let span2 = CodeSpan::new(1, 4, &s[3..6], s);
        let span3 = CodeSpan::new(1, 1, &s[0..6], s);
        let span4 = CodeSpan::eof();
        assert_eq!(span1.error_footer(), "\nabcdefgh\n^^^");
        assert_eq!(span2.error_footer(), "\nabcdefgh\n   ^^^");
        assert_eq!(span3.error_footer(), "\nabcdefgh\n^^^^^^");
        assert_eq!(span4.error_footer(), "");
    }
}
