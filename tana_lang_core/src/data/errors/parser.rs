use thiserror::Error;
use crate::{
    impl_display,
    impl_new,
    data::*,
};

#[derive(Clone, Debug, Error)]
pub enum ParserError {
    #[error("{0}")]
    InvalidSyntax(InvalidSyntax),
    #[error("{0}")]
    NodeRequired(NodeRequired),
}

#[derive(Clone, Debug)]
pub struct InvalidSyntax {
    error_header: String,
    error_footer: String,
    syntax: String,
}

impl_new!(ParserError, InvalidSyntax, invalid_syntax, syntax: String);
impl_display!(InvalidSyntax, "{}Invalid {}:{}", syntax);

#[derive(Clone, Debug)]
pub struct NodeRequired {
    error_header: String,
    error_footer: String,
    node: String,
}

impl_new!(ParserError, NodeRequired, node_required, node: String);
impl_display!(NodeRequired, "{}{} required:{}", node);
