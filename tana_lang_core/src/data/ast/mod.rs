mod ctor;

pub use ctor::*;

use crate::data::*;

pub trait Ast<'input> {
    fn span(&self) -> CodeSpan<'input>;
}

macro_rules! impl_struct_asts {
    ($($ast_ty:ident),*) => {
        $(
            impl<'input> Ast<'input> for $ast_ty<'input> {
                fn span(&self) -> CodeSpan<'input> {
                    self.span.clone()
                }
            }
        )*
    };
}

macro_rules! impl_enum_asts {
    ($($ast_ty:ident, [$($enum:ident),*]),*) => {
        $(
            impl<'input> Ast<'input> for $ast_ty<'input> {
                fn span(&self) -> CodeSpan<'input> {
                    match self {
                        $(
                            $ast_ty::$enum(ast) => ast.span.clone(),
                        )*
                    }
                }
            }
        )*
    };
}

impl_struct_asts!(
    DataDefAst,
    NewTyDefAst,
    LeftTyDefAst,
    FnDefAst,
    TyAst,
    ArrowAst,
    TAppAst,
    TVarAst,
    LifetimeAst,
    BaseAst,
    LeftFnDefAst,
    ExprAst,
    AppAst,
    AccessAst,
    VarAst,
    IntNumAst,
    RealNumAst
);

impl_enum_asts!(
    TopDefEnum, [DataDef, NewTyDef, FnDef],
    TyEnum, [Arrow, TApp, TVar, Lifetime, Base],
    ExprEnum, [App, Access, Var, IntNum, RealNum]
);

#[derive(Clone, Debug, PartialEq)]
pub enum TopDefEnum<'input> {
    DataDef(DataDefAst<'input>),
    NewTyDef(NewTyDefAst<'input>),
    FnDef(FnDefAst<'input>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct DataDefAst<'input> {
    pub left_ty_def: LeftTyDefAst<'input>,
    pub ctors: Vec<String>,
    pub fieldss: Vec<Option<Vec<String>>>,
    pub tyss: Vec<Vec<Box<TyAst<'input>>>>,
    pub span: CodeSpan<'input>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct NewTyDefAst<'input> {
    pub left_ty_def: LeftTyDefAst<'input>,
    pub ctor: String,
    pub field: Option<String>,
    pub ty: Box<TyAst<'input>>,
    pub span: CodeSpan<'input>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LeftTyDefAst<'input> {
    pub name: String,
    pub args: Vec<Box<TyAst<'input>>>,
    pub span: CodeSpan<'input>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnDefAst<'input> {
    pub ty_annot: Option<Box<TyAst<'input>>>,
    pub left_fn_def: LeftFnDefAst<'input>,
    pub expr: Box<ExprAst<'input>>,
    pub span: CodeSpan<'input>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TyAst<'input> {
    pub ty_enum: TyEnum<'input>,
    pub span: CodeSpan<'input>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TyEnum<'input> {
    Arrow(ArrowAst<'input>),
    TApp(TAppAst<'input>),
    TVar(TVarAst<'input>),
    Lifetime(LifetimeAst<'input>),
    Base(BaseAst<'input>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArrowAst<'input> {
    pub lhs: Box<TyAst<'input>>,
    pub rhs: Box<TyAst<'input>>,
    pub span: CodeSpan<'input>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TAppAst<'input> {
    pub ty_fn: Box<TyAst<'input>>,
    pub ty_arg: Option<Box<TyAst<'input>>>,
    pub ref_attr: RefAttrAst<'input>,
    pub span: CodeSpan<'input>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum RefAttrAst<'input> {
    Unconstrained,
    Ref(Option<LifetimeAst<'input>>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct TVarAst<'input> {
    pub name: String,
    pub span: CodeSpan<'input>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LifetimeAst<'input> {
    pub name: String,
    pub span: CodeSpan<'input>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BaseAst<'input> {
    pub name: String,
    pub span: CodeSpan<'input>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LeftFnDefAst<'input> {
    pub name: String,
    pub args: Vec<Box<ExprAst<'input>>>,
    pub span: CodeSpan<'input>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprAst<'input> {
    pub expr_enum: ExprEnum<'input>,
    pub span: CodeSpan<'input>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprEnum<'input> {
    App(AppAst<'input>),
    Access(AccessAst<'input>),
    Var(VarAst<'input>),
    IntNum(IntNumAst<'input>),
    RealNum(RealNumAst<'input>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct AppAst<'input> {
    pub fn_expr: Box<ExprAst<'input>>,
    pub arg_expr: Option<Box<ExprAst<'input>>>,
    pub span: CodeSpan<'input>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AccessAst<'input> {
    pub expr: Box<ExprAst<'input>>,
    pub field: String,
    pub span: CodeSpan<'input>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarAst<'input> {
    pub name: String,
    pub span: CodeSpan<'input>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IntNumAst<'input> {
    pub value: String,
    pub span: CodeSpan<'input>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RealNumAst<'input> {
    pub value: String,
    pub span: CodeSpan<'input>,
}
