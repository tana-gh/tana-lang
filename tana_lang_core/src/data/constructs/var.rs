use std::{
    cell::RefCell,
    rc::Rc,
};
use anyhow::Result;
use crate::{
    impl_construct_val,
    impl_construct_key,
    data::*,
};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum VarKind {
    Normal,
    Arg,
    DataCtor,
    DataField(usize),
    NewtyCtor,
    NewtyField,
}

#[derive(Clone, Debug)]
pub struct Var {
    pub id: usize,
    pub qual: Rc<Qual>,
    pub name: String,
    pub var_kind: VarKind,
    pub ty: Rc<Ty>,
    pub abs: RefCell<Option<Rc<Abs>>>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct VarKey {
    pub qual: QualKey,
    pub name: String,
}

impl_construct_val!(Var);

impl ConstructVal for Var {
    type Key = VarKey;

    fn to_key(&self) -> Self::Key {
        Self::Key {
            qual: self.qual.to_key(),
            name: self.name.clone(),
        }
    }
}

impl_construct_key!(VarKey, Var, var_store);

impl Construct for VarKey {
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

impl Var {
    pub fn new(ctx: &mut SemantizerContext, qual: Rc<Qual>, name: String, var_kind: VarKind, ty: Rc<Ty>) -> Result<Rc<Self>> {
        let val = Rc::new(Self {
            id: ctx.var_store.next_id(),
            qual,
            name,
            var_kind,
            ty,
            abs: RefCell::new(None),
        });
        let key = val.to_key();
        ctx.var_store.insert(key, val)
    }

    pub fn new_or_get(ctx: &mut SemantizerContext, qual: Rc<Qual>, name: String, var_kind: VarKind, ty: Rc<Ty>) -> Rc<Self> {
        let val = Rc::new(Self {
            id: ctx.var_store.next_id(),
            qual,
            name,
            var_kind,
            ty,
            abs: RefCell::new(None),
        });
        let key = val.to_key();
        ctx.var_store.insert_or_get(key, val)
    }

    pub fn clone_with_ty_env(&self, ctx: &mut SemantizerContext, ty_env: Rc<RefCell<TyEnv>>) -> Rc<Self> {
        let ty = ty_env.borrow().apply_env(ctx, self.ty.clone());
        Self::new_or_get(ctx, self.qual.clone(), self.name.clone(), self.var_kind, ty)
    }
}

impl VarKey {
    pub fn new(qual: QualKey, name: String) -> Self {
        Self { qual, name }
    }
}
