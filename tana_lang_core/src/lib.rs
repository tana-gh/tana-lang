pub mod data;
pub mod lexer;
pub mod parser;
pub mod semantizer;

use anyhow::Result;
use data::*;

pub fn semantize() -> Result<SemantizerContext> {
    unimplemented!()
}
