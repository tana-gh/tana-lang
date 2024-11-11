use crate::data::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Token<'input>(
    pub TokenEnum,
    pub CodeSpan<'input>,
);

#[derive(Clone, Debug, PartialEq)]
pub enum TokenEnum {
    Eof,
    Semicolon,
    UpperIdent(String),
    LowerIdent(String),
    IntNum(String),
    RealNum(String),
    OpCode(String),
    Data,
    NewTy,
    Ty,
    Fn,
    Arrow,
    Equal,
    Colon,
    Dot,
    Comma,
    Pipe,
    Ampersand,
    SingleQuote,
    LParen,
    RParen,
    LBrace,
    RBrace,
}
