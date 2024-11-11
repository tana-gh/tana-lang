use std::iter::Peekable;
use anyhow::{
    bail,
    Result,
};
use crate::data::*;

pub fn lex<'input>(input: &'input str) -> Result<Vec<Token<'input>>> {
    let mut tokens = Vec::new();
    let mut errs = Vec::new();
    let mut lexer =
        Lexer {
            iter: CodeSpanCharIter::new(input).peekable()
        };
    loop {
        match lexer.assume() {
            Ok(Some(token @ Token(TokenEnum::Eof, _))) => {
                tokens.push(token);
                break;
            },
            Ok(Some(token)) =>
                tokens.push(token),
            Ok(None) =>
                (),
            Err(e) => {
                errs.push(e);
                lexer.iter.next();
            },
        }
    }
    if errs.len() == 0 {
        Ok(tokens)
    }
    else {
        bail!(Errors(errs));
    }
}

struct Lexer<'input> {
    iter: Peekable<CodeSpanCharIter<'input>>,
}

impl<'input> Lexer<'input> {
    fn assume(&mut self) -> Result<Option<Token<'input>>> {
        if let Some(token) = self.assume_eof()? {
            Ok(Some(token))
        }
        else if let Some(_) = self.assume_whitespace()? {
            Ok(None)
        }
        else if let Some(token) = self.assume_token()? {
            Ok(Some(token))
        }
        else {
            let (span, c) = &self.iter.peek().unwrap();
            bail!(LexerError::invalid_token(span, *c));
        }
    }

    fn assume_eof(&mut self) -> Result<Option<Token<'input>>> {
        if let None = self.iter.peek() {
            Ok(Some(Token(TokenEnum::Eof, CodeSpan::eof())))
        }
        else {
            Ok(None)
        }
    }

    fn assume_whitespace(&mut self) -> Result<Option<()>> {
        let mut consumed = false;
        while Self::map_char_span(self.iter.peek(), false, Self::is_whitespace) {
            self.iter.next();
            consumed = true;
        }
        if consumed {
            Ok(Some(()))
        }
        else {
            Ok(None)
        }
    }

    fn assume_token(&mut self) -> Result<Option<Token<'input>>> {
        if let Some(token) = self.assume_semicolon()? {
            Ok(Some(token))
        }
        else if let Some(token) = self.assume_keyword_or_ident()? {
            Ok(Some(token))
        }
        else if let Some(token) = self.assume_num()? {
            Ok(Some(token))
        }
        else if let Some(token) = self.assume_paren()? {
            Ok(Some(token))
        }
        else if let Some(token) = self.assume_symbol_or_op_code()? {
            Ok(Some(token))
        }
        else {
            Ok(None)
        }
    }

    fn assume_semicolon(&mut self) -> Result<Option<Token<'input>>> {
        if Self::map_char_span(self.iter.peek(), false, Self::is_semicolon) {
            let (span, _) = self.iter.next().unwrap();
            Ok(Some(Token(TokenEnum::Semicolon, span)))
        }
        else {
            Ok(None)
        }
    }

    fn assume_keyword_or_ident(&mut self) -> Result<Option<Token<'input>>> {
        if Self::map_char_span(self.iter.peek(), false, Self::is_ident_head) {
            let (span_head, c) = self.iter.next().unwrap();
            let mut token = String::from(c);
            let mut span_tail = span_head.clone();
            while Self::map_char_span(self.iter.peek(), false, Self::is_ident_tail) {
                let (span, c) = self.iter.next().unwrap();
                token.push(c);
                span_tail = span;
            }
            let span = span_head.extend(&span_tail);
            if Self::is_data(&token) {
                Ok(Some(Token(TokenEnum::Data, span)))
            }
            else if Self::is_newty(&token) {
                Ok(Some(Token(TokenEnum::NewTy, span)))
            }
            else if Self::is_ty(&token) {
                Ok(Some(Token(TokenEnum::Ty, span)))
            }
            else if Self::is_fn(&token) {
                Ok(Some(Token(TokenEnum::Fn, span)))
            }
            else if Self::is_upper_ident(&token) {
                Ok(Some(Token(TokenEnum::UpperIdent(token), span)))
            }
            else {
                Ok(Some(Token(TokenEnum::LowerIdent(token), span)))
            }
        }
        else {
            Ok(None)
        }
    }

    fn assume_num(&mut self) -> Result<Option<Token<'input>>> {
        if Self::map_char_span(self.iter.peek(), false, Self::is_num) {
            let (span_head, c) = self.iter.next().unwrap();
            let mut token = String::from(c);
            let mut span_tail = span_head.clone();
            while Self::map_char_span(self.iter.peek(), false, Self::is_num) {
                let (span, c) = self.iter.next().unwrap();
                token.push(c);
                span_tail = span;
            }
            if Self::map_char_span(self.iter.peek(), false, Self::is_point) {
                let (span, dot) = self.iter.next().unwrap();
                token.push(dot);
                span_tail = span;
                while Self::map_char_span(self.iter.peek(), false, Self::is_num) {
                    let (span, c) = self.iter.next().unwrap();
                    token.push(c);
                    span_tail = span;
                }
                let span = span_head.extend(&span_tail);
                Ok(Some(Token(TokenEnum::RealNum(token), span)))
            }
            else {
                let span = span_head.extend(&span_tail);
                Ok(Some(Token(TokenEnum::IntNum(token), span)))
            }
        }
        else {
            Ok(None)
        }
    }

    fn assume_symbol_or_op_code(&mut self) -> Result<Option<Token<'input>>> {
        if Self::map_char_span(self.iter.peek(), false, Self::is_op_code) {
            let (span_head, c) = self.iter.next().unwrap();
            let mut token = String::from(c);
            let mut span_tail = span_head.clone();
            while Self::map_char_span(self.iter.peek(), false, Self::is_op_code) {
                if Self::is_op_code_break(&token, &self.iter.peek().unwrap().1) {
                    break;
                }
                let (span, c) = self.iter.next().unwrap();
                token.push(c);
                span_tail = span;
            }
            let span = span_head.extend(&span_tail);
            if Self::is_arrow(&token) {
                Ok(Some(Token(TokenEnum::Arrow, span)))
            }
            else if Self::is_equal(&token) {
                Ok(Some(Token(TokenEnum::Equal, span)))
            }
            else if Self::is_colon(&token) {
                Ok(Some(Token(TokenEnum::Colon, span)))
            }
            else if Self::is_dot(&token) {
                Ok(Some(Token(TokenEnum::Dot, span)))
            }
            else if Self::is_comma(&token) {
                Ok(Some(Token(TokenEnum::Comma, span)))
            }
            else if Self::is_pipe(&token) {
                Ok(Some(Token(TokenEnum::Pipe, span)))
            }
            else if Self::is_ampersand(&token) {
                Ok(Some(Token(TokenEnum::Ampersand, span)))
            }
            else if Self::is_single_quote(&token) {
                Ok(Some(Token(TokenEnum::SingleQuote, span)))
            }
            else {
                Ok(Some(Token(TokenEnum::OpCode(token), span)))
            }
        }
        else {
            Ok(None)
        }
    }

    fn assume_paren(&mut self) -> Result<Option<Token<'input>>> {
        let c = self.iter.peek();
        if Self::map_char_span(c, false, Self::is_l_paren) {
            let (span, _) = self.iter.next().unwrap();
            Ok(Some(Token(TokenEnum::LParen, span)))
        }
        else if Self::map_char_span(c, false, Self::is_r_paren) {
            let (span, _) = self.iter.next().unwrap();
            Ok(Some(Token(TokenEnum::RParen, span)))
        }
        else if Self::map_char_span(c, false, Self::is_l_brace) {
            let (span, _) = self.iter.next().unwrap();
            Ok(Some(Token(TokenEnum::LBrace, span)))
        }
        else if Self::map_char_span(c, false, Self::is_r_brace) {
            let (span, _) = self.iter.next().unwrap();
            Ok(Some(Token(TokenEnum::RBrace, span)))
        }
        else {
            Ok(None)
        }
    }

    fn map_char_span<T>(span: Option<&(CodeSpan<'input>, char)>, default: T, f: fn(&char) -> T) -> T {
        match span {
            Some((_, c)) => f(c),
            None => default,
        }
    }

    fn is_whitespace(c: &char) -> bool {
        c.is_ascii_whitespace()
    }

    fn is_semicolon(c: &char) -> bool {
        *c == ';'
    }

    fn is_point(c: &char) -> bool {
        *c == '.'
    }

    fn is_ident_head(c: &char) -> bool {
        c.is_ascii_alphabetic()
    }

    fn is_ident_tail(c: &char) -> bool {
        *c == '_' || c.is_ascii_alphanumeric()
    }

    fn is_num(c: &char) -> bool {
        c.is_ascii_digit()
    }

    fn is_op_code(c: &char) -> bool {
        [
            '!',
            '#',
            '$',
            '%',
            '&',
            '*',
            '+',
            '.',
            '/',
            '<',
            '=',
            '>',
            '?',
            '@',
            '\\',
            '^',
            '|',
            '-',
            '~',
            ':',
            ',',
            '\'',
        ].contains(c)
    }

    fn is_op_code_break(s: &str, c: &char) -> bool {
        [
            ("&", '\''),
        ].contains(&(s, *c))
    }

    fn is_l_paren(c: &char) -> bool {
        *c == '('
    }

    fn is_r_paren(c: &char) -> bool {
        *c == ')'
    }

    fn is_l_brace(c: &char) -> bool {
        *c == '{'
    }

    fn is_r_brace(c: &char) -> bool {
        *c == '}'
    }

    fn is_upper_ident(s: &str) -> bool {
        s.chars().next().unwrap().is_uppercase()
    }

    fn is_data(s: &str) -> bool {
        s == "data"
    }

    fn is_newty(s: &str) -> bool {
        s == "newty"
    }

    fn is_ty(s: &str) -> bool {
        s == "ty"
    }

    fn is_fn(s: &str) -> bool {
        s == "fn"
    }

    fn is_arrow(s: &str) -> bool {
        s == "->"
    }

    fn is_equal(s: &str) -> bool {
        s == "="
    }

    fn is_colon(s: &str) -> bool {
        s == ":"
    }

    fn is_dot(s: &str) -> bool {
        s == "."
    }

    fn is_comma(s: &str) -> bool {
        s == ","
    }

    fn is_pipe(s: &str) -> bool {
        s == "|"
    }

    fn is_ampersand(s: &str) -> bool {
        s == "&"
    }

    fn is_single_quote(s: &str) -> bool {
        s == "'"
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_to_tokens(input: &str) -> Vec<TokenEnum> {
        super::lex(input).unwrap().into_iter().map(|Token(token, _)| token).collect()
    }

    fn lex_to_spans(input: &str) -> Vec<CodeSpan> {
        super::lex(input).unwrap().into_iter().map(|Token(_, span)| span).collect()
    }

    #[test]
    fn test_lex_eof() {
        assert_eq!(lex_to_tokens(""), &[TokenEnum::Eof]);
    }

    #[test]
    fn test_lex_whitespace() {
        assert_eq!(lex_to_tokens(" \t\n\r"), &[TokenEnum::Eof]);
    }

    #[test]
    fn test_lex_semicolon() {
        assert_eq!(lex_to_tokens(";"), &[TokenEnum::Semicolon, TokenEnum::Eof]);
    }

    #[test]
    fn test_lex_data() {
        assert_eq!(lex_to_tokens("data"), &[TokenEnum::Data, TokenEnum::Eof]);
    }

    #[test]
    fn test_lex_newty() {
        assert_eq!(lex_to_tokens("newty"), &[TokenEnum::NewTy, TokenEnum::Eof]);
    }

    #[test]
    fn test_lex_ty() {
        assert_eq!(lex_to_tokens("ty"), &[TokenEnum::Ty, TokenEnum::Eof]);
    }

    #[test]
    fn test_lex_fn() {
        assert_eq!(lex_to_tokens("fn"), &[TokenEnum::Fn, TokenEnum::Eof]);
    }

    #[test]
    fn test_lex_upper_ident() {
        assert_eq!(lex_to_tokens("A"), &[TokenEnum::UpperIdent("A".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("A0"), &[TokenEnum::UpperIdent("A0".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("A_0"), &[TokenEnum::UpperIdent("A_0".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("A0_"), &[TokenEnum::UpperIdent("A0_".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("Aa"), &[TokenEnum::UpperIdent("Aa".to_string()), TokenEnum::Eof]);
    }

    #[test]
    fn test_lex_lower_ident() {
        assert_eq!(lex_to_tokens("a"), &[TokenEnum::LowerIdent("a".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("a0"), &[TokenEnum::LowerIdent("a0".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("a_0"), &[TokenEnum::LowerIdent("a_0".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("a0_"), &[TokenEnum::LowerIdent("a0_".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("aa"), &[TokenEnum::LowerIdent("aa".to_string()), TokenEnum::Eof]);
    }

    #[test]
    fn test_lex_int_num() {
        assert_eq!(lex_to_tokens("0"), &[TokenEnum::IntNum("0".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("1"), &[TokenEnum::IntNum("1".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("2"), &[TokenEnum::IntNum("2".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("3"), &[TokenEnum::IntNum("3".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("4"), &[TokenEnum::IntNum("4".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("5"), &[TokenEnum::IntNum("5".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("6"), &[TokenEnum::IntNum("6".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("7"), &[TokenEnum::IntNum("7".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("8"), &[TokenEnum::IntNum("8".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("9"), &[TokenEnum::IntNum("9".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("123456789"), &[TokenEnum::IntNum("123456789".to_string()), TokenEnum::Eof]);
    }

    #[test]
    fn test_lex_real_num() {
        assert_eq!(lex_to_tokens("0.0"), &[TokenEnum::RealNum("0.0".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("0.5"), &[TokenEnum::RealNum("0.5".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("1.0"), &[TokenEnum::RealNum("1.0".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("1.25"), &[TokenEnum::RealNum("1.25".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("125.125"), &[TokenEnum::RealNum("125.125".to_string()), TokenEnum::Eof]);
    }

    #[test]
    fn test_lex_op_code() {
        assert_eq!(lex_to_tokens("!"), &[TokenEnum::OpCode("!".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("#"), &[TokenEnum::OpCode("#".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("$"), &[TokenEnum::OpCode("$".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("%"), &[TokenEnum::OpCode("%".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("&"), &[TokenEnum::Ampersand, TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("*"), &[TokenEnum::OpCode("*".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("+"), &[TokenEnum::OpCode("+".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("."), &[TokenEnum::Dot, TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("/"), &[TokenEnum::OpCode("/".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("<"), &[TokenEnum::OpCode("<".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("="), &[TokenEnum::Equal, TokenEnum::Eof]);
        assert_eq!(lex_to_tokens(">"), &[TokenEnum::OpCode(">".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("?"), &[TokenEnum::OpCode("?".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("@"), &[TokenEnum::OpCode("@".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("\\"), &[TokenEnum::OpCode("\\".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("^"), &[TokenEnum::OpCode("^".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("|"), &[TokenEnum::Pipe, TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("-"), &[TokenEnum::OpCode("-".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("~"), &[TokenEnum::OpCode("~".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("->"), &[TokenEnum::Arrow, TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("!!"), &[TokenEnum::OpCode("!!".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens(">>="), &[TokenEnum::OpCode(">>=".to_string()), TokenEnum::Eof]);
        assert_eq!(lex_to_tokens(":"), &[TokenEnum::Colon, TokenEnum::Eof]);
        assert_eq!(lex_to_tokens(","), &[TokenEnum::Comma, TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("'"), &[TokenEnum::SingleQuote, TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("&'"), &[TokenEnum::Ampersand, TokenEnum::SingleQuote, TokenEnum::Eof]);
    }

    #[test]
    fn test_lex_paren() {
        assert_eq!(lex_to_tokens("("), &[TokenEnum::LParen, TokenEnum::Eof]);
        assert_eq!(lex_to_tokens(")"), &[TokenEnum::RParen, TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("{"), &[TokenEnum::LBrace, TokenEnum::Eof]);
        assert_eq!(lex_to_tokens("}"), &[TokenEnum::RBrace, TokenEnum::Eof]);
    }

    #[test]
    fn test_lex_tokens() {
        assert_eq!(lex_to_tokens("fn a = 0;"), &[
            TokenEnum::Fn,
            TokenEnum::LowerIdent("a".to_string()),
            TokenEnum::Equal,
            TokenEnum::IntNum("0".to_string()),
            TokenEnum::Semicolon,
            TokenEnum::Eof,
        ]);
    }

    #[test]
    fn test_lex_returns_spans() {
        let s = "fn a = 0;";
        assert!(lex_to_spans("fn a = 0;").into_iter().zip(&[
            CodeSpan::new(1, 1, &s[0..2], s),
            CodeSpan::new(1, 4, &s[3..4], s),
            CodeSpan::new(1, 6, &s[5..6], s),
            CodeSpan::new(1, 8, &s[7..8], s),
            CodeSpan::new(1, 9, &s[8..9], s),
            CodeSpan::eof(),
        ]).all(|(actual, expected)| actual.strict_eq(expected)));
    }
}
