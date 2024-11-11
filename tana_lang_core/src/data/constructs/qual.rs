use std::rc::Rc;
use crate::{
    impl_construct_val,
    impl_construct_key,
    data::*,
};

#[derive(Clone, Debug)]
pub struct Qual {
    pub id: usize,
    pub scopes: Vec<Scope>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct QualKey {
    pub scopes: Vec<Scope>,
}

impl_construct_val!(Qual);

impl ConstructVal for Qual {
    type Key = QualKey;

    fn to_key(&self) -> Self::Key {
        Self::Key {
            scopes: self.scopes.clone(),
        }
    }
}

impl_construct_key!(QualKey, Qual, qual_store);

impl Construct for QualKey {
    fn logical_name(&self) -> String {
        self.qualify_logical_name_self("___")
    }

    fn description(&self) -> String {
        self.qualify_description_self(".")
    }
}

impl Qual {
    pub fn top(ctx: &SemantizerContext) -> Rc<Self> {
        QualKey::top(ctx).get_val(ctx).unwrap()
    }

    fn new_or_get_one(ctx: &mut SemantizerContext, scopes: Vec<Scope>) -> Rc<Self> {
        let value = Rc::new(Self {
            id: ctx.qual_store.next_id(),
            scopes,
        });
        let key = value.to_key();
        ctx.qual_store.insert_or_get(key, value)
    }

    pub fn new_or_get(ctx: &mut SemantizerContext, key: &QualKey) -> Rc<Self> {
        for n in 1..key.scopes.len() {
            Self::new_or_get_one(ctx, key.scopes.iter().take(n).cloned().collect());
        }
        Self::new_or_get_one(ctx, key.scopes.clone())
    }

    pub fn find_scope<T>(&self, pred: impl Fn(&Scope) -> Option<T>) -> Option<T> {
        for scope in self.scopes.iter().rev() {
            if let Some(t) = pred(scope) {
                return Some(t);
            }
        }
        None
    }
}

impl QualKey {
    pub fn top(ctx: &SemantizerContext) -> Self {
        Self::new(vec![ctx.top_scope.clone()])
    }

    pub fn new(scopes: Vec<Scope>) -> Self {
        Self { scopes }
    }

    pub fn pushed(&self, scope: Scope) -> Self {
        let mut cloned = self.scopes.clone();
        cloned.push(scope);
        Self::new(cloned)
    }

    pub fn qualify_logical_name_self(&self, sep: &str) -> String {
        self.scopes.iter().map(|x| x.logical_name()).collect::<Vec<_>>().join(sep)
    }

    pub fn qualify_logical_name(&self, sep: &str) -> String {
        let q = self.qualify_logical_name_self(sep);
        if q.len() == 0 { q } else { q + sep }
    }

    pub fn qualify_description_self(&self, sep: &str) -> String {
        self.scopes.iter().map(|x| x.description()).collect::<Vec<_>>().join(sep)
    }

    pub fn qualify_description(&self, sep: &str) -> String {
        let q = self.qualify_description_self(sep);
        if q.len() == 0 { q } else { q + sep }
    }
}
