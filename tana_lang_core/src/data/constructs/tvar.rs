use std::rc::Rc;
use anyhow::Result;
use crate::{
    impl_construct_val,
    impl_construct_key,
    data::*,
};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum TVarKind {
    Type,
    Lifetime,
}

#[derive(Clone, Debug)]
pub struct TVar {
    pub id: usize,
    pub qual: Rc<Qual>,
    pub name: String,
    pub tvar_kind: TVarKind,
    pub kn: Rc<Kn>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TVarKey {
    pub qual: QualKey,
    pub name: String,
    pub tvar_kind: TVarKind,
}

impl_construct_val!(TVar);

impl ConstructVal for TVar {
    type Key = TVarKey;

    fn to_key(&self) -> Self::Key {
        Self::Key {
            qual: self.qual.to_key(),
            name: self.name.clone(),
            tvar_kind: self.tvar_kind,
        }
    }
}

impl_construct_key!(TVarKey, TVar, tvar_store);

impl Construct for TVarKey {
    fn logical_name(&self) -> String {
        format!(
            "{}{}",
            self.qual.qualify_logical_name("___"),
            self.name.logical_name()
        )
    }

    fn description(&self) -> String {
        format!(
            "{}{}",
            self.qual.qualify_description("."),
            self.name.description()
        )
    }
}

impl TVar {
    pub fn new(ctx: &mut SemantizerContext, qual: Rc<Qual>, name: String, tvar_kind: TVarKind, kn: Rc<Kn>) -> Result<Rc<Self>> {
        let val = Rc::new(Self {
            id: ctx.tvar_store.next_id(),
            qual,
            name,
            tvar_kind,
            kn,
        });
        let key = val.to_key();
        ctx.tvar_store.insert(key, val)
    }

    pub fn new_or_get(ctx: &mut SemantizerContext, qual: Rc<Qual>, name: String, tvar_kind: TVarKind, kn: Rc<Kn>) -> Rc<Self> {
        let val = Rc::new(Self {
            id: ctx.tvar_store.next_id(),
            qual,
            name,
            tvar_kind,
            kn,
        });
        let key = val.to_key();
        ctx.tvar_store.insert_or_get(key, val)
    }
}

impl TVarKey {
    pub fn new(qual: QualKey, name: String, tvar_kind: TVarKind) -> Self {
        Self { qual, name, tvar_kind }
    }
}
