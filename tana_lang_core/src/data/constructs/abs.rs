use std::{
    cell::RefCell,
    rc::Rc,
};
use crate::{
    impl_id,
    data::*,
};

#[derive(Clone, Debug)]
pub struct Abs {
    pub id: usize,
    pub args: Vec<Rc<Var>>,
    pub expr: Option<Rc<Expr>>,
    pub qual: Rc<Qual>,
    pub ty: Rc<Ty>,
    pub ty_env: Rc<RefCell<TyEnv>>,
    pub children: Rc<RefCell<Vec<Rc<ChildAbs>>>>,
}

#[derive(Clone, Debug)]
pub struct ChildAbs {
    pub args: Vec<Rc<Var>>,
    pub expr: Option<Rc<Expr>>,
    pub qual: Rc<Qual>,
    pub ty: Rc<Ty>,
    pub ty_env: Rc<RefCell<TyEnv>>,
}

impl_id!(Abs);

impl Construct for Abs {
    fn logical_name(&self) -> String {
        format!("fn_{}", self.id)
    }

    fn description(&self) -> String {
        format!("fn[{}]", self.id)
    }
}

impl Abs {
    pub fn new_with_id(ctx: &mut SemantizerContext, id: usize, args: Vec<Rc<Var>>, expr: Rc<Expr>, qual: Rc<Qual>, var: Rc<Var>) -> Rc<Self> {
        let in_tys = args.iter().map(|arg| arg.ty.clone()).collect();
        let out_ty = expr.ty();
        let ty = Ty::new_or_get_as_fn_ty(ctx, in_tys, out_ty);
        let tvars = ty.get_tvars();
        let tvars = tvars.into_iter().map(|tvar| tvar.to_key()).collect();
        let ty_env = TyEnv::new(ctx, tvars);
        let val = Rc::new(Self {
            id,
            args: args.clone(),
            expr: Some(expr.clone()),
            qual,
            ty,
            ty_env,
            children: Rc::new(RefCell::new(Vec::new())),
        });
        var.abs.replace(Some(val.clone()));
        ctx.abs_store.insert(id, val).unwrap()
    }

    pub fn new_without_expr(ctx: &mut SemantizerContext, var: Rc<Var>) -> Rc<Self> {
        let id = ctx.abs_id.next_id();
        ctx.abs_id.increment();
        let qual = ctx.push_scope_into_qual_stack(Scope::Abs(id)).get_val(ctx).unwrap();
        let ty = var.ty.clone();
        let tvars = ty.get_tvars();
        let tvars = tvars.into_iter().map(|tvar| tvar.to_key()).collect();
        let ty_env = TyEnv::new(ctx, tvars);
        let val = Rc::new(Self {
            id,
            args: Vec::new(),
            expr: None,
            qual,
            ty,
            ty_env,
            children: Rc::new(RefCell::new(Vec::new())),
        });
        ctx.qual_stack.pop().unwrap();
        var.abs.replace(Some(val.clone()));
        ctx.abs_store.insert(id, val).unwrap()
    }

    pub fn add_child_with_ty_env(&self, ctx: &mut SemantizerContext, qual: Rc<Qual>, ty_env: Rc<RefCell<TyEnv>>) {
        if self.children.borrow().iter().any(|child| child.ty_env == ty_env) {
            return;
        }
        let args = self.args.iter().map(|arg| arg.clone_with_ty_env(ctx, ty_env.clone())).collect();
        let expr = self.expr.as_ref().map(|expr| expr.clone_with_ty_env(ctx, qual.clone(), ty_env.clone()));
        let ty = ty_env.borrow().apply_env(ctx, self.ty.clone());
        let child = Rc::new(ChildAbs {
            args,
            expr,
            qual,
            ty,
            ty_env,
        });
        self.children.borrow_mut().push(child);
    }
}
