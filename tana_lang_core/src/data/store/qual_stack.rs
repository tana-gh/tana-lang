use std::rc::Rc;
use anyhow::{
    bail,
    Result,
};
use crate::data::*;

#[derive(Debug)]
pub struct QualStack {
    stack: Vec<QualKey>,
    top_qual: QualKey,
}

impl QualStack {
    pub fn new(scope_name: String) -> Self {
        let top_qual = QualKey::new(vec![Scope::Mod(scope_name)]);
        Self {
            stack: vec![top_qual.clone()],
            top_qual,
        }
    }

    pub fn push(&mut self, qual: &Rc<Qual>) -> QualKey {
        let key = qual.to_key();
        self.stack.push(key.clone());
        key
    }

    pub fn peek(&self) -> QualKey {
        self.stack.last().unwrap().clone()
    }

    pub fn pop(&mut self) -> Result<QualKey> {
        let qual = self.stack.pop();
        if qual.is_none() || qual.clone().unwrap() == self.top_qual {
            bail!("Top scope has been popped");
        }
        Ok(qual.unwrap())
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = QualKey> + 'a {
        self.stack.iter().rev().cloned()
    }
}
