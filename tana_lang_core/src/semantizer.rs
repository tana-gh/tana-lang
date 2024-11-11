use std::{
    cell::RefCell,
    rc::Rc,
};
use anyhow::{
    anyhow,
    bail,
    Error,
    Result,
};
use crate::data::*;

macro_rules! anyhow_with_errors {
    ($self:ident, $ast:expr, $msg:expr) => {
        {
            let span = &$ast.span;
            $self.errs.push(anyhow!(SemantizerError::other_semantic_error(span, $msg.to_owned())));
            ()
        }
    };

    ($self:ident, $ast:expr, $msg:expr, $name:expr) => {
        {
            let span = &$ast.span;
            $self.errs.push(anyhow!(SemantizerError::invalid_semantics(span, $msg.to_owned(), $name.to_owned())));
            ()
        }
    };
}

macro_rules! bail_with_errors {
    ($self:ident, $ast:expr, $msg:expr) => {
        {
            return Err(anyhow_with_errors!($self, $ast, $msg));
        }
    };

    ($self:ident, $ast:expr, $msg:expr, $name:expr) => {
        {
            return Err(anyhow_with_errors!($self, $ast, $msg, $name));
        }
    };
}

macro_rules! into_with_errors {
    ($result:expr, $self:ident, $ast:expr) => {
        match $result {
            Ok(val) => Ok(val),
            Err(e) =>
                Err(anyhow_with_errors!($self, $ast, e.to_string())),
        }
    };

    ($result:expr, $self:ident, $ast:expr, $msg:expr) => {
        match $result {
            Ok(val) => Ok(val),
            Err(_) =>
                Err(anyhow_with_errors!($self, $ast, $msg)),
        }
    };

    ($result:expr, $self:ident, $ast:expr, $msg:expr, $name:expr) => {
        match $result {
            Ok(val) => Ok(val),
            Err(_) =>
                Err(anyhow_with_errors!($self, $ast, $msg, $name)),
        }
    };
}

pub fn semantize(ctx: &mut SemantizerContext, top_def_enums: &[TopDefEnum]) -> Result<()> {
    let mut semantizer =
        Semantizer {
            ctx,
            errs: Vec::new(),
        };
    for top_def_enum in top_def_enums {
        semantizer.visit_top_def(top_def_enum).ok();
        if semantizer.errs.len() != 0 {
            break;
        }
    }
    if semantizer.errs.len() == 0 {
        Ok(())
    }
    else {
        bail!(Errors(semantizer.errs));
    }
}

struct Semantizer<'ctx> {
    ctx: &'ctx mut SemantizerContext,
    errs: Vec<Error>,
}

impl<'ctx> Semantizer<'ctx> {
    fn visit_top_def(&mut self, top_def_enum: &TopDefEnum) -> Result<(), ()> {
        match top_def_enum {
            TopDefEnum::DataDef(data_def_ast) => {
                self.visit_data_def(data_def_ast)?;
            },
            TopDefEnum::NewTyDef(newty_def_ast) => {
                self.visit_newty_def(newty_def_ast)?;
            },
            TopDefEnum::FnDef(fn_def_ast) => {
                self.visit_fn_def(fn_def_ast)?;
            },
        }
        Ok(())
    }

    fn visit_data_def(&mut self, data_def_ast: &DataDefAst) -> Result<Rc<Data>, ()> {
        let data_id = self.ctx.data_id.next_id();
        self.ctx.data_id.increment();
        let parent_qual = into_with_errors!(self.ctx.qual_stack.peek().get_val(self.ctx), self, data_def_ast.left_ty_def)?;
        let qual = into_with_errors!(self.ctx.push_scope_into_qual_stack(Scope::NewTy(data_id)).get_val(self.ctx), self, data_def_ast.left_ty_def)?;
        let poly_aster = self.poly_aster(&data_def_ast.left_ty_def.args);
        let base = self.visit_left_ty_def_base(&data_def_ast.left_ty_def, parent_qual.clone(), poly_aster)?;
        let args = self.visit_left_ty_def_args(&data_def_ast.left_ty_def.args, qual.clone())?;
        let data_ty = Ty::new_or_get_with_base(self.ctx, base.clone());
        let lifetime_name = format!("'{}", data_def_ast.left_ty_def.name);
        let lifetime_kn = Kn::lifetime(self.ctx);
        let lifetime = into_with_errors!(TVar::new(self.ctx, qual.clone(), lifetime_name, TVarKind::Lifetime, lifetime_kn.clone()), self, data_def_ast.left_ty_def)?;
        let lifetime = Ty::new_or_get_with_tvar(self.ctx, lifetime);
        let lifetime = Ty::new_or_get_as_unary_tapp(self.ctx, lifetime, RefAttr::Unconstrained);
        let tapp_ty = Ty::new_or_get_as_unary_tapp(self.ctx, data_ty.clone(), RefAttr::Ref(lifetime.clone()));
        let tapp_ty = Ty::new_or_get_as_tapp_ty(self.ctx, tapp_ty, args, RefAttr::Ref(lifetime));
        let ctor_and_fields_and_tys =
            data_def_ast.ctors.iter()
            .zip(data_def_ast.fieldss.iter())
            .zip(data_def_ast.tyss.iter());
        let mut ctors = Vec::new();
        let mut fieldss = Vec::new();
        let mut tyss = Vec::new();
        for ((ctor_name, field_names), ty_asts) in ctor_and_fields_and_tys {
            let mut tys = Vec::new();
            for ty_ast in ty_asts {
                tys.push(self.visit_ty(&ty_ast)?);
            }
            tyss.push(tys.clone());
            let ctor_ty = Ty::new_or_get_as_fn_ty(self.ctx, tys.clone(), tapp_ty.clone());
            let ctor = self.visit_ty_def_var(&data_def_ast.left_ty_def, parent_qual.clone(), ctor_name.clone(), VarKind::DataCtor, ctor_ty)?;
            ctors.push(ctor);
            let fields =
                match field_names {
                    Some(field_names) =>
                        Some(
                            field_names.iter()
                            .zip(tys.iter())
                            .enumerate()
                            .map(|(index, (field_name, ty))| {
                                let field_ty = Ty::new_or_get_as_fn_ty(self.ctx, vec![tapp_ty.clone()], ty.clone());
                                self.visit_ty_def_var(&data_def_ast.left_ty_def, parent_qual.clone(), field_name.clone(), VarKind::DataField(index), field_ty.clone())
                            })
                            .collect::<Result<Vec<_>, _>>()?
                        ),
                    None => None,
                };
            fieldss.push(fields);
        }
        let name_and_tys =
            ctors.iter()
            .zip(tyss.iter())
            .map(|(ctor, tys)| (ctor.name.clone(), tys.clone()))
            .collect();
        let data = Data::new_with_id(self.ctx, data_id, tapp_ty.clone(), ctors, fieldss);
        into_with_errors!(self.ctx.ty_data_store.insert(data_ty.to_key(), data.clone()), self, data_def_ast.left_ty_def)?;
        into_with_errors!(self.ctx.data_tyss_store.insert(tapp_ty.to_key(), name_and_tys), self, data_def_ast.left_ty_def)?;
        into_with_errors!(self.ctx.qual_stack.pop(), self, data_def_ast.left_ty_def)?;
        Ok(data)
    }

    fn visit_newty_def(&mut self, newty_def_ast: &NewTyDefAst) -> Result<Rc<NewTy>, ()> {
        let newty_id = self.ctx.newty_id.next_id();
        self.ctx.newty_id.increment();
        let parent_qual = into_with_errors!(self.ctx.qual_stack.peek().get_val(self.ctx), self, newty_def_ast.left_ty_def)?;
        let qual = into_with_errors!(self.ctx.push_scope_into_qual_stack(Scope::NewTy(newty_id)).get_val(self.ctx), self, newty_def_ast.left_ty_def)?;
        let poly_aster = self.poly_aster(&newty_def_ast.left_ty_def.args);
        let base = self.visit_left_ty_def_base(&newty_def_ast.left_ty_def, parent_qual.clone(), poly_aster)?;
        let args = self.visit_left_ty_def_args(&newty_def_ast.left_ty_def.args, qual.clone())?;
        let newty_ty = Ty::new_or_get_with_base(self.ctx, base.clone());
        let tapp_ty = Ty::new_or_get_as_unary_tapp(self.ctx, newty_ty.clone(), RefAttr::Unconstrained);
        let tapp_ty = Ty::new_or_get_as_tapp_ty(self.ctx, tapp_ty.clone(), args, RefAttr::Unconstrained);
        let ty = self.visit_ty(&newty_def_ast.ty)?;
        let ctor_ty = Ty::new_or_get_as_fn_ty(self.ctx, vec![ty.clone()], tapp_ty.clone());
        let ctor = self.visit_ty_def_var(&newty_def_ast.left_ty_def, parent_qual.clone(), newty_def_ast.ctor.clone(), VarKind::NewtyCtor, ctor_ty)?;
        let field_ty = Ty::new_or_get_as_fn_ty(self.ctx, vec![tapp_ty.clone()], ty.clone());
        let field =
            match &newty_def_ast.field {
                Some(field_name) =>
                    Some(self.visit_ty_def_var(&newty_def_ast.left_ty_def, parent_qual.clone(), field_name.clone(), VarKind::NewtyField, field_ty)?),
                None => None,
            };
        let newty = NewTy::new_with_id(self.ctx, newty_id, tapp_ty.clone(), ty.clone(), ctor, field);
        into_with_errors!(self.ctx.ty_newty_store.insert(newty_ty.to_key(), newty.clone()), self, newty_def_ast.left_ty_def)?;
        into_with_errors!(self.ctx.newty_ty_store.insert(tapp_ty.to_key(), ty.clone()), self, newty_def_ast.left_ty_def)?;
        into_with_errors!(self.ctx.qual_stack.pop(), self, newty_def_ast.left_ty_def)?;
        Ok(newty)
    }

    fn poly_aster(&mut self, arg_asts: &[Box<TyAst>]) -> Rc<Kn> {
        let aster = Kn::aster(self.ctx);
        let lifetime_kn = Kn::lifetime(self.ctx);
        arg_asts.iter()
        .map(|arg_ast|
            match &arg_ast.ty_enum {
                TyEnum::Lifetime(_) => lifetime_kn.clone(),
                _ => aster.clone(),
            })
        .rfold(aster.clone(), |acc, aster| Kn::new_or_get_as_karrow(self.ctx, aster, acc))
    }

    fn visit_left_ty_def_base(&mut self, left_ty_def_ast: &LeftTyDefAst, qual: Rc<Qual>, kn: Rc<Kn>) -> Result<Rc<Base>, ()> {
        let base = Base::new(self.ctx, qual, left_ty_def_ast.name.clone(), kn);
        into_with_errors!(base, self, left_ty_def_ast, "Duplicate type definitions", left_ty_def_ast.name.clone())
    }

    fn visit_left_ty_def_args(&mut self, arg_asts: &Vec<Box<TyAst>>, qual: Rc<Qual>) -> Result<Vec<Rc<Ty>>, ()> {
        arg_asts.iter()
        .map(|arg_ast| self.visit_left_ty_def_arg(arg_ast, qual.clone()))
        .collect()
    }

    fn visit_left_ty_def_arg(&mut self, arg_ast: &TyAst, qual: Rc<Qual>) -> Result<Rc<Ty>, ()> {
        let aster = Kn::aster(self.ctx);
        let lifetime_kn = Kn::lifetime(self.ctx);
        let (arg, ref_attr) =
            match &arg_ast.ty_enum {
                TyEnum::TApp(tapp) => {
                    let arg =
                        match &tapp.ty_fn.ty_enum {
                            TyEnum::TVar(tvar) => {
                                let tvar =
                                    into_with_errors!(TVar::new(self.ctx, qual.clone(), tvar.name.clone(), TVarKind::Type, aster), self, arg_ast)?;
                                Ty::new_or_get_with_tvar(self.ctx, tvar)
                            },
                            TyEnum::Lifetime(lifetime) => {
                                let lifetime =
                                    into_with_errors!(TVar::new(self.ctx, qual.clone(), lifetime.name.clone(), TVarKind::Type, lifetime_kn), self, arg_ast)?;
                                Ty::new_or_get_with_tvar(self.ctx, lifetime)
                            },
                            _ => bail_with_errors!(self, arg_ast, "Non type variable argument not supported yet"),
                        };
                    let ref_attr =
                        match &tapp.ref_attr {
                            RefAttrAst::Unconstrained =>
                                RefAttr::Unconstrained,
                            RefAttrAst::Ref(Some(lifetime)) => {
                                let lifetime = self.visit_lifetime(&lifetime)?;
                                let lifetime = Ty::new_or_get_with_tvar(self.ctx, lifetime);
                                let lifetime = Ty::new_or_get_as_unary_tapp(self.ctx, lifetime, RefAttr::Unconstrained);
                                RefAttr::Ref(lifetime)
                            },
                            RefAttrAst::Ref(None) =>
                                bail_with_errors!(self, arg_ast, "No lifetime not supported yet"),
                        };
                    (arg, ref_attr)
                },
                _ => unreachable!(),
            };
        Ok(Ty::new_or_get_as_unary_tapp(self.ctx, arg, ref_attr))
    }

    fn visit_ty_def_var(&mut self, left_ty_def_ast: &LeftTyDefAst, qual: Rc<Qual>, name: String, var_kind: VarKind, ty: Rc<Ty>) -> Result<Rc<Var>, ()> {
        let var = Var::new(self.ctx, qual, name.clone(), var_kind, ty);
        let var = into_with_errors!(var, self, left_ty_def_ast, "Duplicate type definitions", name)?;
        Abs::new_without_expr(self.ctx, var.clone());
        Ok(var)
    }

    fn visit_fn_def(&mut self, fn_def_ast: &FnDefAst) -> Result<Rc<Abs>, ()> {
        let abs_id = self.ctx.abs_id.next_id();
        self.ctx.abs_id.increment();
        let name = &fn_def_ast.left_fn_def.name;
        let arg_asts = &fn_def_ast.left_fn_def.args;
        let parent_qual = into_with_errors!(self.ctx.qual_stack.peek().get_val(self.ctx), self, fn_def_ast.left_fn_def)?;
        let qual = into_with_errors!(self.ctx.push_scope_into_qual_stack(Scope::Abs(abs_id)).get_val(self.ctx), self, fn_def_ast.left_fn_def)?;
        let fn_ty =
            if let Some(ty_annot) = &fn_def_ast.ty_annot {
                self.visit_ty(ty_annot)?
            }
            else {
                let top = Qual::top(self.ctx);
                let i64_ty = Ty::unary_tapp_base(self.ctx, top, "I64".to_owned());
                let fn_in_tys = vec![i64_ty.clone(); arg_asts.len()];
                let fn_out_ty = i64_ty;
                Ty::new_or_get_as_fn_ty(self.ctx, fn_in_tys, fn_out_ty)
            };
        let var = self.visit_left_fn_def_var(&fn_def_ast.left_fn_def, parent_qual, fn_ty.clone())?;
        let (arg_tys, ret_ty) = fn_ty.to_arg_and_ret_tys();
        if arg_tys.len() != arg_asts.len() {
            bail_with_errors!(self, fn_def_ast.left_fn_def, "Defferent argument count between type annotation and function definition", name);
        }
        let args = self.visit_left_fn_def_args(arg_asts, qual.clone(), &arg_tys)?;
        let expr = self.visit_expr(&fn_def_ast.expr)?;
        if ret_ty.assign_from(expr.ty()).is_err() {
            bail_with_errors!(self, fn_def_ast.left_fn_def, "Defferent type between type annotation and function body", name);
        }
        let abs = Abs::new_with_id(self.ctx, abs_id, args, expr, qual, var);
        into_with_errors!(self.ctx.qual_stack.pop(), self, fn_def_ast.left_fn_def)?;
        Ok(abs)
    }

    fn visit_left_fn_def_var(&mut self, left_fn_def_ast: &LeftFnDefAst, qual: Rc<Qual>, ty: Rc<Ty>) -> Result<Rc<Var>, ()> {
        let var = Var::new(self.ctx, qual, left_fn_def_ast.name.clone(), VarKind::Normal, ty);
        into_with_errors!(var, self, left_fn_def_ast, "Duplicate function definitions", left_fn_def_ast.name.clone())
    }

    fn visit_left_fn_def_args(&mut self, arg_asts: &Vec<Box<ExprAst>>, qual: Rc<Qual>, arg_tys: &Vec<Rc<Ty>>) -> Result<Vec<Rc<Var>>, ()> {
        arg_asts.iter()
        .zip(arg_tys.iter())
        .map(|(arg_ast, arg_ty)| self.visit_left_fn_def_arg(arg_ast, qual.clone(), arg_ty.clone()))
        .collect()
    }

    fn visit_left_fn_def_arg(&mut self, arg_ast: &ExprAst, qual: Rc<Qual>, ty: Rc<Ty>) -> Result<Rc<Var>, ()> {
        match &arg_ast.expr_enum {
            ExprEnum::App(app) if app.arg_expr.is_none() =>
                match &app.fn_expr.expr_enum {
                    ExprEnum::Var(var) =>
                        into_with_errors!(Var::new(self.ctx, qual, var.name.clone(), VarKind::Arg, ty), self, arg_ast),
                    _ => bail_with_errors!(self, arg_ast, "Non variable argument not supported yet"),
                },
            _ => unreachable!(),
        }
    }

    fn visit_ty(&mut self, ty_ast: &TyAst) -> Result<Rc<Ty>, ()> {
        Ok(match &ty_ast.ty_enum {
            TyEnum::Arrow(arrow) => {
                let arrow = self.visit_arrow(arrow)?;
                Ty::new_or_get_with_arrow(self.ctx, arrow)
            },
            TyEnum::TApp(tapp) => {
                let tapp = self.visit_tapp(tapp)?;
                Ty::new_or_get_with_tapp(self.ctx, tapp.clone())
            },
            TyEnum::TVar(tvar) => {
                let tvar = self.visit_tvar(tvar)?;
                Ty::new_or_get_with_tvar(self.ctx, tvar)
            },
            TyEnum::Lifetime(lifetime) => {
                let lifetime = self.visit_lifetime(lifetime)?;
                Ty::new_or_get_with_tvar(self.ctx, lifetime)
            },
            TyEnum::Base(base) => {
                let base = self.visit_base(base)?;
                Ty::new_or_get_with_base(self.ctx, base)
            },
        })
    }

    fn visit_arrow(&mut self, arrow_ast: &ArrowAst) -> Result<Rc<Arrow>, ()> {
        let in_ty = self.visit_ty(&arrow_ast.lhs)?;
        let out_ty = self.visit_ty(&arrow_ast.rhs)?;
        Ok(Arrow::new_or_get(self.ctx, in_ty, out_ty))
    }

    fn visit_tapp(&mut self, tapp_ast: &TAppAst) -> Result<Rc<TApp>, ()> {
        let ref_attr =
            match &tapp_ast.ref_attr {
                RefAttrAst::Unconstrained =>
                    RefAttr::Unconstrained,
                RefAttrAst::Ref(Some(lifetime)) => {
                    let lifetime = self.visit_lifetime(&lifetime)?;
                    let lifetime = Ty::new_or_get_with_tvar(self.ctx, lifetime);
                    let lifetime = Ty::new_or_get_as_unary_tapp(self.ctx, lifetime, RefAttr::Unconstrained);
                    RefAttr::Ref(lifetime)
                },
                RefAttrAst::Ref(None) =>
                    bail_with_errors!(self, tapp_ast, "No lifetime not supported yet"),
            };
        let ty_fn = self.visit_ty(&tapp_ast.ty_fn)?;
        let ty_arg =
            match tapp_ast.ty_arg.as_ref() {
                Some(ty_arg) => Some(self.visit_ty(ty_arg)?),
                None => None,
            };
        if let Some(ty_arg) = ty_arg {
            if ty_fn.kn().rank() == 0 {
                bail_with_errors!(self, tapp_ast, "Defferent argument count between kind and type arguments", ty_fn.description());
            }
            Ok(TApp::new_or_get(self.ctx, ty_fn.clone(), ty_arg, ref_attr))
        }
        else {
            Ok(TApp::new_or_get_as_unary(self.ctx, ty_fn.clone(), ref_attr))
        }
    }

    fn visit_tvar(&mut self, tvar_ast: &TVarAst) -> Result<Rc<TVar>, ()> {
        let qual = into_with_errors!(self.ctx.qual_stack.peek().get_val(self.ctx), self, tvar_ast)?;
        let aster = Kn::aster(self.ctx);
        Ok(TVar::new_or_get(self.ctx, qual, tvar_ast.name.clone(), TVarKind::Type, aster))
    }

    fn visit_lifetime(&mut self, lifetime_ast: &LifetimeAst) -> Result<Rc<TVar>, ()> {
        let qual = into_with_errors!(self.ctx.qual_stack.peek().get_val(self.ctx), self, lifetime_ast)?;
        let lifetime_kn = Kn::lifetime(self.ctx);
        Ok(TVar::new_or_get(self.ctx, qual, lifetime_ast.name.clone(), TVarKind::Lifetime, lifetime_kn))
    }

    fn visit_base(&mut self, base_ast: &BaseAst) -> Result<Rc<Base>, ()> {
        self.ctx.find_with_qual(|ctx, qual|
            BaseKey::new(qual.to_key(), base_ast.name.clone()).get_val(ctx).ok()
        )
        .ok_or_else(|| {
            anyhow_with_errors!(self, base_ast, "Unknown type", base_ast.name)
        })
    }

    fn visit_expr(&mut self, expr_ast: &ExprAst) -> Result<Rc<Expr>, ()> {
        Ok(match &expr_ast.expr_enum {
            ExprEnum::App(app_ast) =>
                Expr::new_with_app(self.visit_app(app_ast)?),
            ExprEnum::Access(access_ast) =>
                Expr::new_with_access(self.visit_access(access_ast)?),
            ExprEnum::Var(var_ast) =>
                Expr::new_with_var(self.visit_var(var_ast)?),
            ExprEnum::IntNum(int_num_ast) =>
                Expr::new_with_cn(self.visit_int_num(int_num_ast)?),
            ExprEnum::RealNum(real_num_ast) =>
                Expr::new_with_cn(self.visit_real_num(real_num_ast)?),
        })
    }

    fn visit_app(&mut self, app_ast: &AppAst) -> Result<Rc<App>, ()> {
        let (fn_expr, args) = self.unapply_app(app_ast)?;
        match fn_expr.as_ref() { 
            Expr::Access(access) =>
                Ok(App::new_as_unary(self.ctx, fn_expr.clone(), access.ty.clone(), TyEnv::new_empty())),
            Expr::Cn(cn) =>
                Ok(App::new_as_unary(self.ctx, fn_expr.clone(), cn.ty.clone(), TyEnv::new_empty())),
            Expr::Var(var) if var.var_kind == VarKind::Arg =>
                Ok(App::new_as_unary(self.ctx, fn_expr.clone(), var.ty.clone(), TyEnv::new_empty())),
            Expr::Var(var) => {
                let abs = var.abs.borrow();
                let abs = abs.as_ref().unwrap();
                let qual = into_with_errors!(self.ctx.qual_stack.peek().get_val(self.ctx), self, app_ast)?;
                let ty_env = self.assign_app_into_ty_env(app_ast, abs.clone(), &args, qual.clone())?;
                if !ty_env.borrow().apply_env(self.ctx, abs.ty.clone()).is_nondeterministic(self.ctx) {
                    abs.add_child_with_ty_env(self.ctx, abs.qual.clone(), ty_env.clone());
                }
                Ok(App::reconstruct_app(self.ctx, ty_env, abs.ty.clone(), fn_expr.clone(), &args))
            },
            _ => bail_with_errors!(self, app_ast, "Lambda expression not supported yet"),
        }
    }

    fn unapply_app(&mut self, mut app_ast: &AppAst) -> Result<(Rc<Expr>, Vec<Rc<Expr>>), ()> {
        let mut args = Vec::new();
        while let Some(arg_expr) = &app_ast.arg_expr {
            args.push(self.visit_expr(&arg_expr)?);
            if let ExprAst { expr_enum: ExprEnum::App(fn_app_ast), .. } = app_ast.fn_expr.as_ref() {
                app_ast = fn_app_ast;
            }
            else {
                unreachable!();
            }
        }
        args.reverse();
        let fn_expr = self.visit_expr(&app_ast.fn_expr)?;
        Ok((fn_expr, args))
    }

    fn assign_app_into_ty_env(&mut self, app_ast: &AppAst, abs: Rc<Abs>, args: &Vec<Rc<Expr>>, qual: Rc<Qual>) -> Result<Rc<RefCell<TyEnv>>, ()> {
        let mut fn_ty = abs.ty.clone();
        let in_tys = args.iter().map(|arg| arg.ty()).collect::<Vec<_>>();
        let ty_env = Rc::new(RefCell::new(abs.ty_env.borrow().clone()));
        {
            let mut ty_env = ty_env.borrow_mut();
            for in_ty in in_tys {
                fn_ty = into_with_errors!(ty_env.apply_tys(self.ctx, fn_ty, in_ty.clone(), qual.clone()), self, app_ast)?;
            }
            into_with_errors!(ty_env.apply_qual(self.ctx, fn_ty.clone(), qual.clone()), self, app_ast)?;
        }
        Ok(ty_env)
    }

    fn visit_access(&mut self, access_ast: &AccessAst) -> Result<Rc<Access>, ()> {
        let expr = self.visit_expr(&access_ast.expr)?;
        let ty = expr.ty();
        let field =
            if let Ok(data) = self.ctx.ty_data_store.get(&ty.to_first_ty_fn().to_key()) {
                if data.fieldss.len() == 1 {
                    if let Some(fields) = &data.fieldss[0] {
                        fields.iter()
                        .find(|field| field.name == access_ast.field)
                        .unwrap()
                        .clone()
                    }
                    else {
                        bail_with_errors!(self, access_ast, "Accessing data type with no fields", ty.description());
                    }
                }
                else {
                    bail_with_errors!(self, access_ast, "Accessing data type with multiple constructors", ty.description());
                }
            }
            else if let Ok(newty) = self.ctx.ty_newty_store.get(&ty.to_first_ty_fn().to_key()) {
                if let Some(field) = &newty.field {
                    field.clone()
                }
                else {
                    bail_with_errors!(self, access_ast, "Accessing newty type with no field", ty.description());
                }
            }
            else {
                bail_with_errors!(self, access_ast, "Accessing non data or newty type", ty.description());
            };
        let abs = field.abs.borrow();
        let abs = abs.as_ref().unwrap();
        let qual = into_with_errors!(self.ctx.qual_stack.peek().get_val(self.ctx), self, access_ast)?;
        let ty_env = self.assign_access_into_ty_env(access_ast, abs.clone(), expr.clone(), qual)?;
        if !ty_env.borrow().apply_env(self.ctx, abs.ty.clone()).is_nondeterministic(self.ctx) {
            abs.add_child_with_ty_env(self.ctx, abs.qual.clone(), ty_env.clone());
        }
        let ty = ty_env.borrow().apply_env(self.ctx, field.ty.to_out_ty().clone());
        Ok(Access::new(self.ctx, expr, field.clone(), ty, ty_env))
    }

    fn assign_access_into_ty_env(&mut self, access_ast: &AccessAst, abs: Rc<Abs>, arg: Rc<Expr>, qual: Rc<Qual>) -> Result<Rc<RefCell<TyEnv>>, ()> {
        let mut fn_ty = abs.ty.clone();
        let in_ty = arg.ty();
        let ty_env = Rc::new(RefCell::new(abs.ty_env.borrow().clone()));
        {
            let mut ty_env = ty_env.borrow_mut();
            fn_ty = into_with_errors!(ty_env.apply_tys(self.ctx, fn_ty, in_ty.clone(), qual.clone()), self, access_ast)?;
            into_with_errors!(ty_env.apply_qual(self.ctx, fn_ty.clone(), qual.clone()), self, access_ast)?;
        }
        Ok(ty_env)
    }

    fn visit_var(&mut self, var_ast: &VarAst) -> Result<Rc<Var>, ()> {
        self.ctx.find_with_qual(|ctx, qual| {
            let var = VarKey::new(qual.to_key(), var_ast.name.clone()).get_val(ctx).ok();
            if let Some(var) = var.as_ref() {
                if let VarKind::DataField(_) | VarKind::NewtyField = var.var_kind {
                    None
                }
                else {
                    Some(var.clone())
                }
            }
            else {
                None
            }
        })
        .ok_or_else(|| {
            anyhow_with_errors!(self, var_ast, "Unknown variable", var_ast.name)
        })
    }

    fn visit_int_num(&mut self, int_num_ast: &IntNumAst) -> Result<Rc<Cn>, ()> {
        Ok(Cn::new_or_get_as_i64(self.ctx, int_num_ast.value.clone()))
    }

    fn visit_real_num(&mut self, real_num_ast: &RealNumAst) -> Result<Rc<Cn>, ()> {
        Ok(Cn::new_or_get_as_f64(self.ctx, real_num_ast.value.clone()))
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use anyhow::Result;
    use crate::{
        data::*,
        lexer,
        parser,
    };

    fn semantize(s: &str) -> Result<SemantizerContext> {
        let parsed = parser::parse(lexer::lex(s).unwrap()).unwrap();
        let mut ctx = SemantizerContext::new("test".to_owned());
        super::semantize(&mut ctx, &parsed)
        .map(|_| ctx)
    }

    #[test]
    fn test_semantize_id() {
        let mut ctx = semantize("fn id x = x").unwrap();
        let top = QualKey::top(&ctx);
        let id = ctx.var_store.get(&VarKey::new(top.clone(), "id".to_owned())).unwrap();
        let i64_ty = TyKey::unary_tapp_base(top.clone(), "I64".to_owned()).get_val(&ctx).unwrap();
        assert_eq!(id.name, "id");
        assert_eq!(id.ty, Ty::new_or_get_as_fn_ty(&mut ctx, vec![i64_ty.clone()], i64_ty.clone()));
        let abs = id.abs.borrow();
        let abs = abs.as_ref().unwrap();
        let x_qual = top.pushed(Scope::Abs(abs.id));
        let x = ctx.var_store.get(&VarKey::new(x_qual, "x".to_owned())).unwrap();
        assert_eq!(x.name, "x");
        assert_eq!(x.ty, i64_ty.clone());
        assert_eq!(abs.args.len(), 1);
        assert_eq!(abs.args[0], x);
        let app =
            Expr::App(
                Rc::new(App {
                id: ctx.app_store.next_id() - 1,
                fn_expr: Rc::new(Expr::Var(x)),
                arg_expr: None,
                ty: i64_ty.clone(),
                ty_env: TyEnv::new_empty(),
            }));
        assert_eq!(abs.expr.as_ref().unwrap().as_ref(), &app);
        assert_eq!(abs.expr.as_ref().unwrap().ty(), i64_ty);
    }

    #[test]
    fn test_semantize_zero() {
        let mut ctx = semantize("fn zero = 0").unwrap();
        let top = QualKey::top(&ctx);
        let zero = ctx.var_store.get(&VarKey::new(top.clone(), "zero".to_owned())).unwrap();
        let i64_ty = TyKey::unary_tapp_base(top, "I64".to_owned()).get_val(&ctx).unwrap();
        assert_eq!(zero.name, "zero");
        assert_eq!(zero.ty, i64_ty.clone());
        let abs = zero.abs.borrow();
        let abs = abs.as_ref().unwrap();
        let cn = Cn::new_or_get_as_i64(&mut ctx, "0".to_owned());
        assert_eq!(abs.args.len(), 0);
        assert_eq!(abs.expr.as_ref().unwrap().as_ref().unwrap_unary_app_cn(), cn);
        assert!(abs.expr.as_ref().unwrap().ty().assign_from(i64_ty).is_ok());
    }
}
