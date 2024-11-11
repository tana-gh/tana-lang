use std::{
    cell::RefCell,
    rc::Rc
};
use anyhow::{
    bail,
    Result,
};
use crate::{
    impl_construct_key,
    impl_construct_val,
    data::*,
};

#[derive(Clone, Debug)]
pub struct App {
    pub id: usize,
    pub fn_expr: Rc<Expr>,
    pub arg_expr: Option<Rc<Expr>>,
    pub ty: Rc<Ty>,
    pub ty_env: Rc<RefCell<TyEnv>>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct AppKey {
    pub id: usize,
}

impl_construct_val!(App);

impl ConstructVal for App {
    type Key = AppKey;

    fn to_key(&self) -> Self::Key {
        Self::Key {
            id: self.id,
        }
    }
}

impl_construct_key!(AppKey, App, app_store);

impl Construct for AppKey {
    fn logical_name(&self) -> String {
        format!("app_{}", self.id)
    }

    fn description(&self) -> String {
        format!("app[{}]", self.id)
    }
}

impl App {
    pub fn new(ctx: &mut SemantizerContext, fn_expr: Rc<Expr>, arg_expr: Rc<Expr>, ty: Rc<Ty>, ty_env: Rc<RefCell<TyEnv>>) -> Rc<Self> {
        let val = Rc::new(Self {
            id: ctx.app_store.next_id(),
            fn_expr,
            arg_expr: Some(arg_expr),
            ty,
            ty_env,
        });
        let key = val.to_key();
        ctx.app_store.insert(key.clone(), val.clone()).unwrap()
    }

    pub fn new_as_unary(ctx: &mut SemantizerContext, fn_expr: Rc<Expr>, ty: Rc<Ty>, ty_env: Rc<RefCell<TyEnv>>) -> Rc<Self> {
        let val = Rc::new(Self {
            id: ctx.app_store.next_id(),
            fn_expr,
            arg_expr: None,
            ty,
            ty_env,
        });
        let key = val.to_key();
        ctx.app_store.insert(key.clone(), val.clone()).unwrap()
    }

    pub fn unapply(self: &Rc<Self>) -> Result<(Rc<Expr>, Vec<Rc<Expr>>)> {
        let mut app = self.clone();
        let mut args = Vec::new();
        while let Some(arg_expr) = &app.arg_expr {
            args.push(arg_expr.clone());
            if let Expr::App(fn_app) = app.fn_expr.as_ref() {
                app = fn_app.clone();
            }
            else {
                unreachable!();
            }
        }
        args.reverse();
        let fn_expr = app.fn_expr.clone();
        Ok((fn_expr, args))
    }

    pub fn unapply_with_ty_env(self: &Rc<Self>, ctx: &mut SemantizerContext, qual: Rc<Qual>, ty_env: Rc<RefCell<TyEnv>>) -> Result<(Rc<Expr>, Vec<Rc<Expr>>, Rc<Ty>)> {
        let mut app = self.clone();
        let mut args = Vec::new();
        while let Some(arg_expr) = &app.arg_expr {
            args.push(arg_expr.clone_with_ty_env(ctx, qual.clone(), ty_env.clone()));
            if let Expr::App(fn_app) = app.fn_expr.as_ref() {
                app = fn_app.clone();
            }
            else {
                unreachable!();
            }
        }
        let ty = ty_env.borrow().apply_env(ctx, app.ty.clone());
        args.reverse();
        let fn_expr = app.fn_expr.clone_with_ty_env(ctx, qual, ty_env);
        Ok((fn_expr, args, ty))
    }

    pub fn clone_with_ty_env(self: &Rc<Self>, ctx: &mut SemantizerContext, qual: Rc<Qual>, ty_env: Rc<RefCell<TyEnv>>) -> Result<Rc<Self>> {
        let (fn_expr, args, ty) = self.unapply_with_ty_env(ctx, qual.clone(), ty_env.clone())?;
        match fn_expr.as_ref() {
            Expr::Access(access) =>
                Ok(Self::new_as_unary(ctx, fn_expr.clone(), access.ty.clone(), TyEnv::new_empty())),
            Expr::Cn(cn) =>
                Ok(Self::new_as_unary(ctx, fn_expr.clone(), cn.ty.clone(), TyEnv::new_empty())),
            Expr::Var(var) => {
                let abs = var.abs.borrow();
                if let Some(abs) = abs.as_ref() {
                    let fn_ty = abs.ty.clone();
                    let in_tys =
                        args.iter().map(|arg| {
                            let ty = arg.applied_ty(ctx);
                            ty_env.borrow().apply_env(ctx, ty)
                        }).collect::<Vec<_>>();
                    let ty_env = Self::assign_into_ty_env(ctx, abs.ty_env.clone(), fn_ty, &in_tys, qual)?;
                    if !ty_env.borrow().apply_env(ctx, abs.ty.clone()).is_nondeterministic(ctx) {
                        abs.add_child_with_ty_env(ctx, abs.qual.clone(), ty_env.clone());
                    }
                    Ok(Self::reconstruct_app(ctx, ty_env, ty, fn_expr.clone(), &args))
                }
                else {
                    let in_tys =
                        args.iter().map(|arg| {
                            let ty = arg.applied_ty(ctx);
                            ty_env.borrow().apply_env(ctx, ty)
                        }).collect::<Vec<_>>();
                    let ty_env = Self::assign_into_ty_env(ctx, TyEnv::new_empty(), ty.clone(), &in_tys, qual)?;
                    Ok(Self::reconstruct_app(ctx, ty_env, ty, fn_expr.clone(), &args))
                }
            },
            _ => bail!("Lambda expression not supported yet"),
        }
    }

    fn assign_into_ty_env(ctx: &mut SemantizerContext, ty_env: Rc<RefCell<TyEnv>>, mut fn_ty: Rc<Ty>, in_tys: &Vec<Rc<Ty>>, qual: Rc<Qual>) -> Result<Rc<RefCell<TyEnv>>> {
        let ty_env = Rc::new(RefCell::new(ty_env.borrow().clone()));
        {
            let mut ty_env = ty_env.borrow_mut();
            for in_ty in in_tys {
                fn_ty = ty_env.apply_tys(ctx, fn_ty, in_ty.clone(), qual.clone())?;
            }
            ty_env.apply_qual(ctx, fn_ty.clone(), qual.clone())?;
        }
        Ok(ty_env)
    }

    pub fn reconstruct_app(ctx: &mut SemantizerContext, ty_env: Rc<RefCell<TyEnv>>, ty: Rc<Ty>, fn_expr: Rc<Expr>, args: &Vec<Rc<Expr>>) -> Rc<App> {
        let mut ty = ty_env.borrow().apply_env(ctx, ty);
        let mut app = Self::new_as_unary(ctx, fn_expr.clone(), ty.clone(), ty_env.clone());
        for arg in args {
            ty = ty.to_out_ty();
            app = Self::new(ctx, Expr::new_with_app(app), arg.clone(), ty.clone(), ty_env.clone());
        }
        app
    }
}

impl AppKey {
    pub fn new(id: usize) -> Self {
        Self {
            id,
        }
    }
}
