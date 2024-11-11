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

#[derive(Clone, Debug)]
pub struct Access {
    pub id: usize,
    pub expr: Rc<Expr>,
    pub field: Rc<Var>,
    pub ty: Rc<Ty>,
    pub ty_env: Rc<RefCell<TyEnv>>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct AccessKey {
    pub id: usize,
}

impl_construct_val!(Access);

impl ConstructVal for Access {
    type Key = AccessKey;

    fn to_key(&self) -> Self::Key {
        Self::Key {
            id: self.id.clone(),
        }
    }
}

impl_construct_key!(AccessKey, Access, access_store);

impl Construct for AccessKey {
    fn logical_name(&self) -> String {
        format!("access_{}", self.id)
    }

    fn description(&self) -> String {
        format!("access[{}]", self.id)
    }
}

impl Access {
    pub fn new(ctx: &mut SemantizerContext, expr: Rc<Expr>, field: Rc<Var>, ty: Rc<Ty>, ty_env: Rc<RefCell<TyEnv>>) -> Rc<Self> {
        let val = Rc::new(Self {
            id: ctx.access_store.next_id(),
            expr,
            field,
            ty,
            ty_env,
        });
        let key = val.to_key();
        ctx.access_store.insert(key, val).unwrap()
    }

    pub fn clone_with_ty_env(&self, ctx: &mut SemantizerContext, qual: Rc<Qual>, ty_env: Rc<RefCell<TyEnv>>) -> Result<Rc<Self>> {
        let expr = self.expr.clone_with_ty_env(ctx, qual.clone(), ty_env.clone());
        let field = self.field.clone_with_ty_env(ctx, ty_env.clone());
        let ty = ty_env.borrow().apply_env(ctx, self.ty.clone());
        let abs = field.abs.borrow();
        let abs = abs.as_ref().unwrap();
        let fn_ty = abs.ty.clone();
        let in_ty = expr.applied_ty(ctx);
        let in_ty = ty_env.borrow().apply_env(ctx, in_ty);
        let ty_env = Self::assign_into_ty_env(ctx, abs.ty_env.clone(), fn_ty, in_ty, qual)?;
        if !ty_env.borrow().apply_env(ctx, abs.ty.clone()).is_nondeterministic(ctx) {
            abs.add_child_with_ty_env(ctx, abs.qual.clone(), ty_env.clone());
        }
        Ok(Self::new(ctx, expr, field.clone(), ty, ty_env))
    }

    fn assign_into_ty_env(ctx: &mut SemantizerContext, ty_env: Rc<RefCell<TyEnv>>, mut fn_ty: Rc<Ty>, in_ty: Rc<Ty>, qual: Rc<Qual>) -> Result<Rc<RefCell<TyEnv>>> {
        let ty_env = Rc::new(RefCell::new(ty_env.borrow().clone()));
        {
            let mut ty_env = ty_env.borrow_mut();
            fn_ty = ty_env.apply_tys(ctx, fn_ty, in_ty.clone(), qual.clone())?;
            ty_env.apply_qual(ctx, fn_ty.clone(), qual.clone())?;
        }
        Ok(ty_env)
    }
}

impl AccessKey {
    pub fn new(id: usize) -> Self {
        Self { id }
    }
}
