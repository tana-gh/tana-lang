use crate::data::*;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Scope {
    Mod(String),
    Abs(usize),
    NewTy(usize),
}

impl Construct for Scope {
    fn logical_name(&self) -> String {
        match self {
            Self::Mod(name) =>
                name.clone(),
            Self::Abs(id) =>
                format!("fn_{}", id),
            Self::NewTy(id) =>
                format!("newty_{}", id),
        }
    }

    fn description(&self) -> String {
        match self {
            Self::Mod(name) =>
                name.clone(),
            Self::Abs(id) =>
                format!("fn[{}]", id),
            Self::NewTy(id) =>
                format!("newty[{}]", id),
        }
    }
}
