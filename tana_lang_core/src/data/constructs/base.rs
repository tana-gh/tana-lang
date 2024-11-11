use std::rc::Rc;
use anyhow::Result;
use crate::{
    impl_construct_val,
    impl_construct_key,
    data::*,
};

#[derive(Clone, Debug)]
pub struct Base {
    pub id: usize,
    pub qual: Rc<Qual>,
    pub name: String,
    pub kn: Rc<Kn>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct BaseKey {
    pub qual: QualKey,
    pub name: String,
}

impl_construct_val!(Base);

impl ConstructVal for Base {
    type Key = BaseKey;

    fn to_key(&self) -> Self::Key {
        Self::Key {
            qual: self.qual.to_key(),
            name: self.name.clone(),
        }
    }
}

impl_construct_key!(BaseKey, Base, base_store);

impl Construct for BaseKey {
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

impl Base {
    pub fn new(ctx: &mut SemantizerContext, qual: Rc<Qual>, name: String, kn: Rc<Kn>) -> Result<Rc<Self>> {
        let val = Rc::new(Self {
            id: ctx.base_store.next_id(),
            qual,
            name,
            kn,
        });
        let key = val.to_key();
        ctx.base_store.insert(key, val)
    }

    pub fn new_or_get(ctx: &mut SemantizerContext, qual: Rc<Qual>, name: String, kn: Rc<Kn>) -> Rc<Self> {
        let val = Rc::new(Self {
            id: ctx.base_store.next_id(),
            qual,
            name,
            kn,
        });
        let key = val.to_key();
        ctx.base_store.insert_or_get(key, val)
    }
}

impl BaseKey {
    pub fn new(qual: QualKey, name: String) -> Self {
        Self { qual, name }
    }
}
