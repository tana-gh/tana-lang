use crate::data::*;

pub fn top_data_def_ast<'input>(data_def_ast: DataDefAst<'input>) -> TopDefEnum<'input> {
    TopDefEnum::DataDef(data_def_ast)
}

pub fn top_newty_def_ast<'input>(newty_def_ast: NewTyDefAst<'input>) -> TopDefEnum<'input> {
    TopDefEnum::NewTyDef(newty_def_ast)
}

pub fn top_fn_def_ast<'input>(fn_def_ast: FnDefAst<'input>) -> TopDefEnum<'input> {
    TopDefEnum::FnDef(fn_def_ast)
}

pub fn data_def_ast<'input>(left_ty_def: LeftTyDefAst<'input>, ctors: Vec<String>, fieldss: Vec<Option<Vec<String>>>, tyss: Vec<Vec<Box<TyAst<'input>>>>, span: CodeSpan<'input>) -> DataDefAst<'input> {
    DataDefAst { left_ty_def, ctors, fieldss, tyss, span }
}

pub fn newty_def_ast<'input>(left_ty_def: LeftTyDefAst<'input>, ctor: String, ty: Box<TyAst<'input>>, span: CodeSpan<'input>) -> NewTyDefAst<'input> {
    NewTyDefAst { left_ty_def, ctor, field: None, ty, span }
}

pub fn newty_rec_def_ast<'input>(left_ty_def: LeftTyDefAst<'input>, ctor: String, field: String, ty: Box<TyAst<'input>>, span: CodeSpan<'input>) -> NewTyDefAst<'input> {
    NewTyDefAst { left_ty_def, ctor, field: Some(field), ty, span }
}

pub fn left_ty_def_ast<'input>(name: String, args: Vec<Box<TyAst<'input>>>, span: CodeSpan<'input>) -> LeftTyDefAst<'input> {
    LeftTyDefAst { name, args, span }
}

pub fn fn_def_ast<'input>(ty_annot: Option<Box<TyAst<'input>>>, left_fn_def: LeftFnDefAst<'input>, expr: Box<ExprAst<'input>>, span: CodeSpan<'input>) -> FnDefAst<'input> {
    FnDefAst { ty_annot, left_fn_def, expr, span }
}

pub fn arrow_ty_ast<'input>(arrow: ArrowAst<'input>, span: CodeSpan<'input>) -> Box<TyAst<'input>> {
    Box::new(TyAst { ty_enum: TyEnum::Arrow(arrow), span })
}

pub fn tapp_ty_ast<'input>(tapp: TAppAst<'input>, span: CodeSpan<'input>) -> Box<TyAst<'input>> {
    Box::new(TyAst { ty_enum: TyEnum::TApp(tapp), span })
}

pub fn tvar_ty_ast<'input>(tvar: TVarAst<'input>, span: CodeSpan<'input>) -> Box<TyAst<'input>> {
    Box::new(TyAst { ty_enum: TyEnum::TVar(tvar), span })
}

pub fn lifetime_ty_ast<'input>(lifetime: LifetimeAst<'input>, span: CodeSpan<'input>) -> Box<TyAst<'input>> {
    Box::new(TyAst { ty_enum: TyEnum::Lifetime(lifetime), span })
}

pub fn base_ty_ast<'input>(base: BaseAst<'input>, span: CodeSpan<'input>) -> Box<TyAst<'input>> {
    Box::new(TyAst { ty_enum: TyEnum::Base(base), span })
}

pub fn arrow_ast<'input>(lhs: Box<TyAst<'input>>, rhs: Box<TyAst<'input>>, span: CodeSpan<'input>) -> ArrowAst<'input> {
    ArrowAst { lhs, rhs, span }
}

pub fn tapp_ast<'input>(ty_fn: Box<TyAst<'input>>, ty_arg: Box<TyAst<'input>>, ref_attr: RefAttrAst<'input>, span: CodeSpan<'input>) -> TAppAst<'input> {
    TAppAst { ty_fn, ty_arg: Some(ty_arg), ref_attr, span }
}

pub fn unary_tapp_ast<'input>(ty_fn: Box<TyAst<'input>>, ref_attr: RefAttrAst<'input>, span: CodeSpan<'input>) -> TAppAst<'input> {
    TAppAst { ty_fn, ty_arg: None, ref_attr, span }
}

pub fn tvar_ast<'input>(name: String, span: CodeSpan<'input>) -> TVarAst<'input> {
    TVarAst { name, span }
}

pub fn lifetime_ast<'input>(name: String, span: CodeSpan<'input>) -> LifetimeAst<'input> {
    LifetimeAst { name, span }
}

pub fn base_ast<'input>(name: String, span: CodeSpan<'input>) -> BaseAst<'input> {
    BaseAst { name, span }
}

pub fn left_fn_def_ast<'input>(name: String, args: Vec<Box<ExprAst<'input>>>, span: CodeSpan<'input>) -> LeftFnDefAst<'input> {
    LeftFnDefAst { name, args, span }
}

pub fn app_expr_ast<'input>(app_ast: AppAst<'input>, span: CodeSpan<'input>) -> Box<ExprAst<'input>> {
    Box::new(ExprAst { expr_enum: ExprEnum::App(app_ast), span })
}

pub fn access_expr_ast<'input>(access_ast: AccessAst<'input>, span: CodeSpan<'input>) -> Box<ExprAst<'input>> {
    Box::new(ExprAst { expr_enum: ExprEnum::Access(access_ast), span })
}

pub fn var_expr_ast<'input>(var_ast: VarAst<'input>, span: CodeSpan<'input>) -> Box<ExprAst<'input>> {
    Box::new(ExprAst { expr_enum: ExprEnum::Var(var_ast), span })
}

pub fn int_num_expr_ast<'input>(int_num_ast: IntNumAst<'input>, span: CodeSpan<'input>) -> Box<ExprAst<'input>> {
    Box::new(ExprAst { expr_enum: ExprEnum::IntNum(int_num_ast), span })
}

pub fn real_num_expr_ast<'input>(real_num_ast: RealNumAst<'input>, span: CodeSpan<'input>) -> Box<ExprAst<'input>> {
    Box::new(ExprAst { expr_enum: ExprEnum::RealNum(real_num_ast), span })
}

pub fn app_ast<'input>(fn_expr: Box<ExprAst<'input>>, arg_expr: Box<ExprAst<'input>>, span: CodeSpan<'input>) -> AppAst<'input> {
    AppAst { fn_expr, arg_expr: Some(arg_expr), span }
}

pub fn unary_app_ast<'input>(fn_expr: Box<ExprAst<'input>>, span: CodeSpan<'input>) -> AppAst<'input> {
    AppAst { fn_expr, arg_expr: None, span }
}

pub fn prefix_op_ast<'input>(op_code: String, rhs: Box<ExprAst<'input>>, span: CodeSpan<'input>, op_code_span: CodeSpan<'input>) -> AppAst<'input> {
    app_ast(
        app_expr_ast(
            unary_app_ast(
                var_expr_ast(var_ast(op_code.clone(), op_code_span.clone()), op_code_span.clone()),
                op_code_span.clone(),
            ),
            op_code_span,
        ),
        rhs,
        span,
    )
}

pub fn infix_op_ast<'input>(op_code: String, lhs: Box<ExprAst<'input>>, rhs: Box<ExprAst<'input>>, span: CodeSpan<'input>, op_code_span: CodeSpan<'input>, lhs_span: CodeSpan<'input>) -> AppAst<'input> {
    app_ast(
        app_expr_ast(
            app_ast(
                app_expr_ast(
                    unary_app_ast(
                        var_expr_ast(var_ast(op_code, op_code_span.clone()), op_code_span.clone()),
                        op_code_span.clone(),
                    ),
                    op_code_span,
                ),
                lhs,
                lhs_span.clone(),
            ),
            lhs_span,
        ),
        rhs,
        span,
    )
}

pub fn access_ast<'input>(expr: Box<ExprAst<'input>>, field: String, span: CodeSpan<'input>) -> AccessAst<'input> {
    AccessAst { expr, field, span }
}

pub fn var_ast<'input>(name: String, span: CodeSpan<'input>) -> VarAst<'input> {
    VarAst { name, span }
}

pub fn int_num_ast<'input>(value: String, span: CodeSpan<'input>) -> IntNumAst<'input> {
    IntNumAst { value, span }
}

pub fn real_num_ast<'input>(value: String, span: CodeSpan<'input>) -> RealNumAst<'input> {
    RealNumAst { value, span }
}
