use std::{
    cell::RefCell,
    hash::{
        Hash,
        Hasher,
    },
    rc::Rc,
};
use anyhow::Result;
use crate::data::*;

#[derive(Clone, Debug)]
pub enum Expr {
    Var(Rc<Var>),
    Cn(Rc<Cn>),
    Abs(Rc<Abs>),
    App(Rc<App>),
    Access(Rc<Access>),
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Var(var), Self::Var(other_var)) =>
                var == other_var,
            (Self::Cn(cn), Self::Cn(other_cn)) =>
                cn == other_cn,
            (Self::Abs(abs), Self::Abs(other_abs)) =>
                abs == other_abs,
            (Self::App(app), Self::App(other_app)) =>
                app == other_app,
            (Self::Access(access), Self::Access(other_access)) =>
                access == other_access,
            _ => false,
        }
    }
}

impl Eq for Expr {}

impl Hash for Expr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Var(var) =>
                [0, var.id].hash(state),
            Self::Cn(cn) =>
                [1, cn.id].hash(state),
            Self::Abs(abs) =>
                [2, abs.id].hash(state),
            Self::App(app) =>
                [3, app.id].hash(state),
            Self::Access(access) =>
                [4, access.id].hash(state),
        };
    }
}

impl Construct for Expr {
    fn logical_name(&self) -> String {
        match self {
            Self::Var(var) =>
                var.logical_name(),
            Self::Cn(cn) =>
                cn.logical_name(),
            Self::Abs(abs) =>
                abs.logical_name(),
            Self::App(app) =>
                app.logical_name(),
            Self::Access(access) =>
                access.logical_name(),
        }
    }

    fn description(&self) -> String {
        match self {
            Self::Var(var) =>
                var.description(),
            Self::Cn(cn) =>
                cn.description(),
            Self::Abs(abs) =>
                abs.description(),
            Self::App(app) =>
                app.description(),
            Self::Access(access) =>
                access.description(),
        }
    }
}

impl Expr {
    pub fn new_with_var(var: Rc<Var>) -> Rc<Self> {
        Rc::new(Self::Var(var))
    }

    pub fn new_with_cn(cn: Rc<Cn>) -> Rc<Self> {
        Rc::new(Self::Cn(cn))
    }

    pub fn new_with_abs(abs: Rc<Abs>) -> Rc<Self> {
        Rc::new(Self::Abs(abs))
    }

    pub fn new_with_app(app: Rc<App>) -> Rc<Self> {
        Rc::new(Self::App(app))
    }

    pub fn new_with_access(access: Rc<Access>) -> Rc<Self> {
        Rc::new(Self::Access(access))
    }

    pub fn new_as_var(ctx: &mut SemantizerContext, qual: Rc<Qual>, name: String, var_kind: VarKind, ty: Rc<Ty>) -> Result<Rc<Self>> {
        Ok(Self::new_with_var(Var::new(ctx, qual, name, var_kind, ty)?))
    }

    pub fn new_or_get_as_var(ctx: &mut SemantizerContext, qual: Rc<Qual>, name: String, var_kind: VarKind, ty: Rc<Ty>) -> Rc<Self> {
        Self::new_with_var(Var::new_or_get(ctx, qual, name, var_kind, ty))
    }

    pub fn new_as_cn_as_i64(ctx: &mut SemantizerContext, name: String) -> Result<Rc<Self>> {
        Ok(Self::new_with_cn(Cn::new_as_i64(ctx, name)?))
    }

    pub fn new_as_cn_as_f64(ctx: &mut SemantizerContext, name: String) -> Result<Rc<Self>> {
        Ok(Self::new_with_cn(Cn::new_as_f64(ctx, name)?))
    }

    pub fn new_or_get_as_cn_as_i64(ctx: &mut SemantizerContext, name: String) -> Rc<Self> {
        Self::new_with_cn(Cn::new_or_get_as_i64(ctx, name))
    }

    pub fn new_or_get_as_cn_as_f64(ctx: &mut SemantizerContext, name: String) -> Rc<Self> {
        Self::new_with_cn(Cn::new_or_get_as_f64(ctx, name))
    }

    pub fn new_as_app(ctx: &mut SemantizerContext, fn_expr: Rc<Expr>, arg_expr: Rc<Expr>, ty: Rc<Ty>, ty_env: Rc<RefCell<TyEnv>>) -> Rc<Self> {
        Self::new_with_app(App::new(ctx, fn_expr, arg_expr, ty, ty_env))
    }

    pub fn new_as_unary_app(ctx: &mut SemantizerContext, fn_expr: Rc<Self>, ty_env: Rc<RefCell<TyEnv>>) -> Rc<Self> {
        let ty = fn_expr.ty();
        Self::new_with_app(App::new_as_unary(ctx, fn_expr, ty, ty_env))
    }

    pub fn new_as_access(ctx: &mut SemantizerContext, expr: Rc<Expr>, field: Rc<Var>, ty: Rc<Ty>, ty_env: Rc<RefCell<TyEnv>>) -> Rc<Self> {
        Self::new_with_access(Access::new(ctx, expr, field, ty, ty_env))
    }

    pub fn unwrap_var(&self) -> Rc<Var> {
        match self {
            Self::Var(var) =>
                var.clone(),
            _ => panic!("Unwrapping non-Var Expr: `{}`", self.description()),
        }
    }

    pub fn unwrap_cn(&self) -> Rc<Cn> {
        match self {
            Self::Cn(cn) =>
                cn.clone(),
            _ => panic!("Unwrapping non-Cn Expr: `{}`", self.description()),
        }
    }

    pub fn unwrap_abs(&self) -> Rc<Abs> {
        match self {
            Self::Abs(abs) =>
                abs.clone(),
            _ => panic!("Unwrapping non-Abs Expr: `{}`", self.description()),
        }
    }

    pub fn unwrap_app(&self) -> Rc<App> {
        match self {
            Self::App(app) =>
                app.clone(),
            _ => panic!("Unwrapping non-App Expr: `{}`", self.description()),
        }
    }

    pub fn unwrap_unary_app(&self) -> Rc<Self> {
        match self {
            Self::App(app) if app.arg_expr.is_none() =>
                app.fn_expr.clone(),
            _ => panic!("Unwrapping non-unary App Expr: `{}`", self.description()),
        }
    }

    pub fn unwrap_unary_app_var(&self) -> Rc<Var> {
        match self {
            Self::App(app) if app.arg_expr.is_none() =>
                app.fn_expr.unwrap_var(),
            _ => panic!("Unwrapping non-Var Expr: `{}`", self.description()),
        }
    }

    pub fn unwrap_unary_app_cn(&self) -> Rc<Cn> {
        match self {
            Self::App(app) if app.arg_expr.is_none() =>
                app.fn_expr.unwrap_cn(),
            _ => panic!("Unwrapping non-Cn Expr: `{}`", self.description()),
        }
    }

    pub fn unwrap_access(&self) -> Rc<Access> {
        match self {
            Self::Access(access) =>
                access.clone(),
            _ => panic!("Unwrapping non-Access Expr: `{}`", self.description()),
        }
    }

    pub fn ty(&self) -> Rc<Ty> {
        match self {
            Self::Var(var) =>
                var.ty.clone(),
            Self::Cn(cn) =>
                cn.ty.clone(),
            Self::Abs(abs) =>
                abs.ty.clone(),
            Self::App(app) =>
                app.ty.clone(),
            Self::Access(access) =>
                access.ty.clone(),
        }
    }

    pub fn applied_ty(&self, ctx: &mut SemantizerContext) -> Rc<Ty> {
        match self {
            Self::App(app) =>
                app.ty_env.borrow().apply_env(ctx, app.ty.clone()),
            Self::Access(access) =>
                access.ty_env.borrow().apply_env(ctx, access.ty.clone()),
            _ => unimplemented!()
        }
    }

    pub fn clone_with_ty_env(self: &Rc<Self>, ctx: &mut SemantizerContext, qual: Rc<Qual>, ty_env: Rc<RefCell<TyEnv>>) -> Rc<Self> {
        match self.as_ref() {
            Self::Var(var) =>
                Self::new_with_var(var.clone_with_ty_env(ctx, ty_env)),
            Self::Cn(_) =>
                self.clone(),
            Self::Abs(_) =>
                unimplemented!(),
            Self::App(app) =>
                Self::new_with_app(app.clone_with_ty_env(ctx, qual, ty_env).unwrap()),
            Self::Access(access) =>
                Self::new_with_access(access.clone_with_ty_env(ctx, qual, ty_env).unwrap()),
        }
    }
}
