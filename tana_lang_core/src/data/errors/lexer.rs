use thiserror::Error;
use crate::{
    impl_display,
    impl_new,
    data::*,
};

#[derive(Clone, Debug, Error)]
pub enum LexerError {
    #[error("{0}")]
    InvalidToken(InvalidToken),
}

#[derive(Clone, Debug)]
pub struct InvalidToken {
    error_header: String,
    error_footer: String,
    c: char,
}

impl_new!(LexerError, InvalidToken, invalid_token, c: char);
impl_display!(InvalidToken, "{}Invalid token found: `{}`{}", c);
