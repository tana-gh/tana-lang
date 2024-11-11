use std::{
    str::{
        CharIndices,
        Lines,
    },
    iter::Peekable
};

use crate::data::*;

#[derive(Clone, Debug)]
pub struct CodeSpanCharIter<'a> {
    s: &'a str,
    indices: CharIndices<'a>,
    next_indices: CharIndices<'a>,
    lines: Peekable<Lines<'a>>,
    line: usize,
    column: usize,
    line_head: bool,
}

impl<'a> CodeSpanCharIter<'a> {
    pub fn new(s: &'a str) -> Self {
        let indices = s.char_indices();
        let mut next_indices = s.char_indices();
        next_indices.next();
        let lines = s.lines().peekable();
        Self {
            s,
            indices,
            next_indices,
            lines,
            line: 0,
            column: 0,
            line_head: true,
        }
    }

    fn next_span(&mut self, slice: &'a str, line_slice: &'a str, c: char) -> CodeSpan<'a> {
        if self.line_head {
            self.line += 1;
            self.column = 1;
            self.line_head = false;
        }
        else {
            self.column += 1;
        }
        if c == '\n' {
            self.line_head = true;
        }
        CodeSpan::new(self.line, self.column, slice, line_slice)
    }
}

impl<'a> Iterator for CodeSpanCharIter<'a> {
    type Item = (CodeSpan<'a>, char);

    fn next(&mut self) -> Option<Self::Item> {
        let index = self.indices.next();
        let next_index = self.next_indices.next();
        let line_slice = self.lines.peek();
        let slice = match (index, next_index) {
            (Some((index, _)), Some((next_index, _))) =>
                Some(&self.s[index .. next_index]),
            (Some((index, _)), None) =>
                Some(&self.s[index ..]),
            _ =>
                None,
        };
        match slice {
            Some(slice) => {
                let line_slice = *line_slice.unwrap();
                let c = slice.chars().next().unwrap();
                if c == '\n' {
                    self.lines.next();
                }
                Some((self.next_span(slice, line_slice, c), c))
            },
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next() {
        let s = "abc\ndef\nghi";
        let mut iter = CodeSpanCharIter::new(s);
        assert_eq!(iter.next(), Some((CodeSpan::new(1, 1, "a", s), 'a')));
        assert_eq!(iter.next(), Some((CodeSpan::new(1, 2, "b", s), 'b')));
        assert_eq!(iter.next(), Some((CodeSpan::new(1, 3, "c", s), 'c')));
        assert_eq!(iter.next(), Some((CodeSpan::new(1, 4, "\n", s), '\n')));
        assert_eq!(iter.next(), Some((CodeSpan::new(2, 1, "d", s), 'd')));
        assert_eq!(iter.next(), Some((CodeSpan::new(2, 2, "e", s), 'e')));
        assert_eq!(iter.next(), Some((CodeSpan::new(2, 3, "f", s), 'f')));
        assert_eq!(iter.next(), Some((CodeSpan::new(2, 4, "\n", s), '\n')));
        assert_eq!(iter.next(), Some((CodeSpan::new(3, 1, "g", s), 'g')));
        assert_eq!(iter.next(), Some((CodeSpan::new(3, 2, "h", s), 'h')));
        assert_eq!(iter.next(), Some((CodeSpan::new(3, 3, "i", s), 'i')));
        assert_eq!(iter.next(), None);
    }
}
