mod lexer;
mod macros;
mod parser;
mod semantizer;

pub use lexer::*;
pub use parser::*;
pub use semantizer::*;

use std::fmt::{
    Display,
    Formatter,
    Result,
};
use anyhow::Error;
use thiserror::Error;

#[derive(Debug, Error)]
pub struct Errors(pub Vec<Error>);

impl Display for Errors {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for error in &self.0 {
            writeln!(f, "{}", error)?;
        }
        let error_count = self.0.len();
        if error_count == 1 {
            writeln!(f, "{} error found.", error_count)?;
        }
        else if error_count > 1 {
            writeln!(f, "{} errors found.", error_count)?;
        }
        Ok(())
    }
}
