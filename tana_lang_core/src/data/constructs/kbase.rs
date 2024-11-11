use std::rc::Rc;
use crate::{
    impl_construct_val,
    impl_construct_key,
    data::*,
};

#[derive(Clone, Debug)]
pub struct KBase {
    pub id: usize,
    pub name: String,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct KBaseKey {
    pub name: String,
}

impl_construct_val!(KBase);

impl ConstructVal for KBase {
    type Key = KBaseKey;

    fn to_key(&self) -> Self::Key {
        Self::Key {
            name: self.name.clone(),
        }
    }
}

impl_construct_key!(KBaseKey, KBase, kbase_store);

impl Construct for KBaseKey {
    fn logical_name(&self) -> String {
        self.name.logical_name()
    }

    fn description(&self) -> String {
        self.name.description()
    }
}

impl KBase {
    pub fn new_or_get(ctx: &mut SemantizerContext, name: String) -> Rc<Self> {
        let val = Rc::new(Self {
            id: ctx.kbase_store.next_id(),
            name,
        });
        let key = val.to_key();
        ctx.kbase_store.insert_or_get(key, val)
    }
}

impl KBaseKey {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}
