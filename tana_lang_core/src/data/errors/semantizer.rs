use thiserror::Error;
use crate::{
    impl_display,
    impl_new,
    data::*,
};

#[derive(Clone, Debug, Error)]
pub enum SemantizerError {
    #[error("{0}")]
    InvalidSemantics(InvalidSemantics),
    #[error("{0}")]
    OtherSemanticError(OtherSemanticError),
}

#[derive(Clone, Debug)]
pub struct InvalidSemantics {
    error_header: String,
    error_footer: String,
    message: String,
    name: String,
}

impl_new!(SemantizerError, InvalidSemantics, invalid_semantics, message: String, name: String);
impl_display!(InvalidSemantics, "{}{}: `{}`{}", message, name);

#[derive(Clone, Debug)]
pub struct OtherSemanticError {
    error_header: String,
    error_footer: String,
    message: String,
}

impl_new!(SemantizerError, OtherSemanticError, other_semantic_error, message: String);
impl_display!(OtherSemanticError, "{}{}{}", message);
