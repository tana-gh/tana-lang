use crate::data::*;

pub fn init_prelude(ctx: &mut SemantizerContext) {
    init_quals(ctx);
    init_kns(ctx);
    init_tys(ctx);
    init_vars(ctx);
}

fn init_quals(ctx: &mut SemantizerContext) {
    Qual::new_or_get(ctx, &QualKey::top(ctx));
}

fn init_kns(ctx: &mut SemantizerContext) {
    init_kn(ctx, "*".to_owned());
    init_kn(ctx, "Lifetime".to_owned());
}

fn init_kn(ctx: &mut SemantizerContext, name: String) {
    Kn::new_or_get_as_kbase(ctx, name.to_owned());
}

fn init_tys(ctx: &mut SemantizerContext) {
    init_base(ctx, "Unknown".to_owned(), RefAttr::Unconstrained);
    init_base(ctx, "Bottom".to_owned(), RefAttr::Unconstrained);
    init_base(ctx, "I64".to_owned(), RefAttr::Unconstrained);
    init_base(ctx, "F64".to_owned(), RefAttr::Unconstrained);
}

fn init_base(ctx: &mut SemantizerContext, name: String, ref_attr: RefAttr) {
    let top = Qual::top(ctx);
    let aster = Kn::aster(ctx);
    let base = Ty::new_or_get_as_base(ctx, top, name.to_owned(), aster);
    Ty::new_or_get_as_unary_tapp(ctx, base, ref_attr);
}

fn init_vars(ctx: &mut SemantizerContext) {
    init_op(ctx, "add".to_owned());
    init_op(ctx, "sub".to_owned());
    init_op(ctx, "mul".to_owned());
    init_op(ctx, "div".to_owned());
}

fn init_op(ctx: &mut SemantizerContext, name: String) {
    let top = ctx.qual_stack.peek().get_val(ctx).unwrap();
    let id = ctx.abs_id.next_id();
    ctx.abs_id.increment();
    let qual = ctx.push_scope_into_qual_stack(Scope::Abs(id)).get_val(ctx).unwrap();
    let aster = Kn::aster(ctx);
    let a_ty = Ty::new_or_get_as_tvar(ctx, qual.clone(), "a".to_owned(), TVarKind::Type, aster.clone());
    let a_ty = Ty::new_or_get_as_unary_tapp(ctx, a_ty.clone(), RefAttr::Unconstrained);
    let a_fn_ty = Ty::new_or_get_as_fn_ty(ctx, vec![a_ty.clone()], a_ty.clone());
    let var_ty = Ty::new_or_get_as_fn_ty(ctx, vec![a_ty.clone(), a_ty.clone()], a_ty.clone());
    let tvar = a_ty.unwrap_unary_tapp_tvar().to_key();
    let ty_env = TyEnv::new(ctx, vec![tvar]);
    let var = Expr::new_or_get_as_var(ctx, top.clone(), name.clone(), VarKind::Normal, var_ty.clone());
    let x = Expr::new_or_get_as_var(ctx, qual.clone(), "x".to_owned(), VarKind::Arg, a_ty.clone());
    let y = Expr::new_or_get_as_var(ctx, qual.clone(), "y".to_owned(), VarKind::Arg, a_ty.clone());
    let x_expr = Expr::new_as_unary_app(ctx, x.clone(), ty_env.clone());
    let y_expr = Expr::new_as_unary_app(ctx, y.clone(), ty_env.clone());
    let inner_var_name = format!("_{}", name);
    let inner_var = Expr::new_or_get_as_var(ctx, top.clone(), inner_var_name, VarKind::Normal, var_ty.clone());
    let expr = Expr::new_as_unary_app(ctx, inner_var, ty_env.clone());
    let expr = Expr::new_as_app(ctx, expr, x_expr, a_fn_ty, ty_env.clone());
    let expr = Expr::new_as_app(ctx, expr, y_expr, a_ty, ty_env);
    Abs::new_with_id(ctx, id, vec![x.unwrap_var(), y.unwrap_var()], expr, qual, var.unwrap_var());
    ctx.qual_stack.pop().unwrap();
}
