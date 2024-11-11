use std::iter::Peekable;
use anyhow::{
    bail,
    Result,
};
use crate::data::*;

#[derive(Clone, Copy, Debug, PartialEq)]
enum Assoc {
    L,
    R,
}

macro_rules! bail_invalid_syntax {
    ($tokens:expr, $syntax:expr) => {
        {
            let span = &$tokens.peek().unwrap().1;
            bail!(ParserError::invalid_syntax(span, $syntax.to_owned()));
        }
    };
}

macro_rules! bail_node_required {
    ($tokens:expr, $node:expr) => {
        {
            let span = &$tokens.peek().unwrap().1;
            bail!(ParserError::node_required(span, $node.to_owned()));
        }
    };
}

pub fn parse<'input>(input: Vec<Token<'input>>) -> Result<Vec<TopDefEnum<'input>>> {
    let mut asts = Vec::new();
    let mut errs = Vec::new();
    let mut parser =
        Parser {
            tokens: input.into_iter().peekable(),
        };
    loop {
        if parser.tokens.peek().is_none() {
            break;
        }
        match parser.assume() {
            Ok(Some(ast)) =>
                asts.push(ast.clone()),
            Ok(None) =>
                break,
            Err(e) => {
                errs.push(e);
                parser.tokens.next();
            },
        }
    }
    if errs.len() == 0 {
        Ok(asts)
    }
    else {
        bail!(Errors(errs));
    }
}

struct Parser<'input, Iter>
where
    Iter: Iterator<Item = Token<'input>>,
{
    tokens: Peekable<Iter>,
}

impl<'input, Iter> Parser<'input, Iter>
where
    Iter: Iterator<Item = Token<'input>>,
{
    fn assume(&mut self) -> Result<Option<TopDefEnum<'input>>> {
        if let Some(_) = self.assume_eof()? {
            Ok(None)
        }
        else if let Some(ast) = self.assume_top_def_delim()? {
            Ok(Some(ast))
        }
        else {
            bail_invalid_syntax!(self.tokens, "top-level definition");
        }
    }

    fn assume_eof(&mut self) -> Result<Option<()>> {
        if let Some(_) = self.assume_simple_token(TokenEnum::Eof)? {
            Ok(Some(()))
        }
        else {
            Ok(None)
        }
    }

    fn assume_top_def_delim(&mut self) -> Result<Option<TopDefEnum<'input>>> {
        if let Some(ast) = self.assume_top_def()? {
            if let Some(_) = self.assume_simple_token(TokenEnum::Eof)? {
                Ok(Some(ast))
            }
            else if let Some(_) = self.assume_simple_token(TokenEnum::Semicolon)? {
                Ok(Some(ast))
            }
            else {
                bail_node_required!(self.tokens, "`;`");
            }
        }
        else {
            Ok(None)
        }
    }

    fn assume_top_def(&mut self) -> Result<Option<TopDefEnum<'input>>> {
        if let Some(data_def) = self.assume_data_def()? {
            Ok(Some(top_data_def_ast(data_def)))
        }
        else if let Some(newty_def) = self.assume_newty_def()? {
            Ok(Some(top_newty_def_ast(newty_def)))
        }
        else if let Some(fn_def) = self.assume_fn_def()? {
            Ok(Some(top_fn_def_ast(fn_def)))
        }
        else {
            Ok(None)
        }
    }

    fn assume_data_def(&mut self) -> Result<Option<DataDefAst<'input>>> {
        if let Some((_, data_span)) = self.assume_simple_token(TokenEnum::Data)? {
            if let Some(left_ty_def) = self.assume_left_ty_def()? {
                if let Some(_) = self.assume_simple_token(TokenEnum::Equal)? {
                    let mut ctors = Vec::new();
                    let mut fieldss = Vec::new();
                    let mut tyss = Vec::new();
                    let mut span;
                    loop {
                        if let Some((ctor, _)) = self.assume_upper_ident()? {
                            if let Some(tys) = self.assume_data_right_def()? {
                                span = tys.last().unwrap().span.clone();
                                ctors.push(ctor);
                                fieldss.push(None);
                                tyss.push(tys);
                            }
                            else if let Some((fields, tys, rec_span)) = self.assume_data_right_rec_def()? {
                                span = rec_span;
                                ctors.push(ctor);
                                fieldss.push(Some(fields));
                                tyss.push(tys);
                            }
                            else {
                                bail_node_required!(self.tokens, "data definition body");
                            }
                            if let Some(_) = self.assume_simple_token(TokenEnum::Pipe)? {
                                continue;
                            }
                            break;
                        }
                        bail_node_required!(self.tokens, "constructor");
                    }
                    let extended = data_span.extend(&span);
                    return Ok(Some(data_def_ast(left_ty_def, ctors, fieldss, tyss, extended)));
                }
                bail_node_required!(self.tokens, "`=`");
            }
            bail_node_required!(self.tokens, "left type definition");
        }
        else {
            Ok(None)
        }
    }

    fn assume_data_right_def(&mut self) -> Result<Option<Vec<Box<TyAst<'input>>>>> {
        if let Some(ty) = self.assume_wrapped_ty_factor()? {
            let mut tys = vec![ty];
            while let Some(ty) = self.assume_wrapped_ty_factor()? {
                tys.push(ty);
            }
            Ok(Some(tys))
        }
        else {
            Ok(None)
        }
    }

    fn assume_data_right_rec_def(&mut self) -> Result<Option<(Vec<String>, Vec<Box<TyAst<'input>>>, CodeSpan<'input>)>> {
        if let Some(_) = self.assume_simple_token(TokenEnum::LBrace)? {
            let mut fields = Vec::new();
            let mut tys = Vec::new();
            loop {
                if let Some((field, _)) = self.assume_lower_ident()? {
                    if let Some(_) = self.assume_simple_token(TokenEnum::Colon)? {
                        if let Some(ty) = self.assume_ty()? {
                            fields.push(field);
                            tys.push(ty);
                            if let Some(_) = self.assume_simple_token(TokenEnum::Comma)? {
                                continue;
                            }
                            break;
                        }
                        bail_node_required!(self.tokens, "type");
                    }
                    bail_node_required!(self.tokens, "`:`");
                }
                bail_node_required!(self.tokens, "field name");
            }
            if let Some((_, r_brace_span)) = self.assume_simple_token(TokenEnum::RBrace)? {
                return Ok(Some((fields, tys, r_brace_span)));
            }
            bail_node_required!(self.tokens, "`}`");
        }
        else {
            Ok(None)
        }
    }

    fn assume_newty_def(&mut self) -> Result<Option<NewTyDefAst<'input>>> {
        if let Some((_, newty_span)) = self.assume_simple_token(TokenEnum::NewTy)? {
            if let Some(left_ty_def) = self.assume_left_ty_def()? {
                if let Some(_) = self.assume_simple_token(TokenEnum::Equal)? {
                    if let Some((ctor, _)) = self.assume_upper_ident()? {
                        if let Some(ty) = self.assume_newty_right_def()? {
                            let extended = newty_span.extend(&ty.span);
                            return Ok(Some(newty_def_ast(left_ty_def, ctor, ty, extended)));
                        }
                        else if let Some((field, ty, rec_span)) = self.assume_newty_right_rec_def()? {
                            let extended = newty_span.extend(&rec_span);
                            return Ok(Some(newty_rec_def_ast(left_ty_def, ctor, field, ty, extended)));
                        }
                        bail_node_required!(self.tokens, "newtype definition body");
                    }
                    bail_node_required!(self.tokens, "constructor");
                }
                bail_node_required!(self.tokens, "`=`");
            }
            bail_node_required!(self.tokens, "left type definition");
        }
        else {
            Ok(None)
        }
    }

    fn assume_newty_right_def(&mut self) -> Result<Option<Box<TyAst<'input>>>> {
        if let Some(ty) = self.assume_wrapped_ty_factor()? {
            return Ok(Some(ty));
        }
        else {
            Ok(None)
        }
    }

    fn assume_newty_right_rec_def(&mut self) -> Result<Option<(String, Box<TyAst<'input>>, CodeSpan<'input>)>> {
        if let Some(_) = self.assume_simple_token(TokenEnum::LBrace)? {
            if let Some((field, _)) = self.assume_lower_ident()? {
                if let Some(_) = self.assume_simple_token(TokenEnum::Colon)? {
                    if let Some(ty) = self.assume_ty()? {
                        if let Some((_, r_brace_span)) = self.assume_simple_token(TokenEnum::RBrace)? {
                            return Ok(Some((field, ty, r_brace_span)));
                        }
                        bail_node_required!(self.tokens, "`}`");
                    }
                    bail_node_required!(self.tokens, "type");
                }
                bail_node_required!(self.tokens, "`:`");
            }
            bail_node_required!(self.tokens, "field name");
        }
        else {
            Ok(None)
        }
    }

    fn assume_left_ty_def(&mut self) -> Result<Option<LeftTyDefAst<'input>>> {
        if let Some((name, span)) = self.assume_upper_ident()? {
            let mut args = Vec::new();
            let mut arg_span = None;
            while let Some(arg) = self.assume_wrapped_ty_factor()? {
                args.push(arg.clone());
                arg_span = Some(arg.span);
            }
            let extended =
                if let Some(arg_span) = arg_span {
                    span.extend(&arg_span)
                }
                else {
                    span
                };
            Ok(Some(left_ty_def_ast(name, args, extended)))
        }
        else {
            Ok(None)
        }
    }

    fn assume_fn_def(&mut self) -> Result<Option<FnDefAst<'input>>> {
        let mut ty_annot = None;
        let mut ty_span = None;
        if let Some(span) = self.assume_simple_token(TokenEnum::Ty)? {
            ty_span = Some(span);
            if let Some(ty) = self.assume_ty()? {
                ty_annot = Some(ty);
            }
        }
        if let Some((_, fn_span)) = self.assume_simple_token(TokenEnum::Fn)? {
            if let Some(left_fn_def) = self.assume_left_fn_def()? {
                if let Some(_) = self.assume_simple_token(TokenEnum::Equal)? {
                    if let Some(expr) = self.assume_expr()? {
                        let extended =
                            if let Some((_, ty_span)) = ty_span {
                                ty_span.extend(&expr.span)
                            }
                            else {
                                fn_span.extend(&expr.span)
                            };
                        return Ok(Some(fn_def_ast(ty_annot, left_fn_def, expr, extended)));
                    }
                    bail_node_required!(self.tokens, "expression");
                }
                bail_node_required!(self.tokens, "`=`");
            }
            bail_node_required!(self.tokens, "left function definition");
        }
        else {
            Ok(None)
        }
    }

    fn assume_ty(&mut self) -> Result<Option<Box<TyAst<'input>>>> {
        let mut tys = Vec::new();
        if let Some(lhs) = self.assume_ty_lhs()? {
            tys.push(lhs);
            while let Some(rhs) = self.assume_ty_rhs()? {
                tys.push(rhs);
            }
            let mut ty_iter = tys.into_iter().rev();
            let mut rhs = ty_iter.next().unwrap();
            for lhs in ty_iter {
                let extended = lhs.span.extend(&rhs.span);
                rhs = arrow_ty_ast(arrow_ast(lhs, rhs, extended.clone()), extended);
            }
            Ok(Some(rhs))
        }
        else {
            Ok(None)
        }
    }

    fn assume_ty_lhs(&mut self) -> Result<Option<Box<TyAst<'input>>>> {
        if let Some(term) = self.assume_ty_term()? {
            Ok(Some(term))
        }
        else {
            Ok(None)
        }
    }

    fn assume_ty_rhs(&mut self) -> Result<Option<Box<TyAst<'input>>>> {
        if let Some(_) = self.assume_simple_token(TokenEnum::Arrow)? {
            if let Some(term) = self.assume_ty_term()? {
                return Ok(Some(term));
            }
            bail_node_required!(self.tokens, "type term");
        }
        else {
            Ok(None)
        }
    }

    fn assume_ty_term(&mut self) -> Result<Option<Box<TyAst<'input>>>> {
        let (ref_attr, ref_span) =
            if let Some((_, span)) = self.assume_simple_token(TokenEnum::Ampersand)? {
                if let Some(lifetime) = self.assume_lifetime()? {
                    (RefAttrAst::Ref(Some(lifetime)), Some(span))
                }
                else {
                    (RefAttrAst::Ref(None), None)
                }
            }
            else {
                (RefAttrAst::Unconstrained, None)
            };
        if let Some(mut term) = self.assume_ty_factor()? {
            if let TyAst { ty_enum: TyEnum::TVar(_) | TyEnum::Lifetime(_) | TyEnum::Base(_), span } = term.as_ref() {
                term = tapp_ty_ast(unary_tapp_ast(term.clone(), ref_attr.clone(), span.clone()), span.clone())
            }
            while let Some(mut factor) = self.assume_ty_factor()? {
                if let TyAst { ty_enum: TyEnum::TVar(_) | TyEnum::Lifetime(_) | TyEnum::Base(_), span } = factor.as_ref() {
                    factor = tapp_ty_ast(unary_tapp_ast(factor.clone(), ref_attr.clone(), span.clone()), span.clone())
                }
                let extended = ref_span.clone().unwrap_or(term.span.clone()).extend(&factor.span);
                term = tapp_ty_ast(tapp_ast(term, factor, ref_attr.clone(), extended.clone()), extended);
            }
            Ok(Some(term))
        }
        else {
            Ok(None)
        }
    }

    fn assume_wrapped_ty_factor(&mut self) -> Result<Option<Box<TyAst<'input>>>> {
        if let Some(factor) = self.assume_ty_factor()? {
            if let TyAst { ty_enum: TyEnum::Arrow(_) | TyEnum::TApp(_), .. } = factor.as_ref() {
                Ok(Some(factor))
            }
            else {
                Ok(Some(tapp_ty_ast(unary_tapp_ast(factor.clone(), RefAttrAst::Unconstrained, factor.span.clone()), factor.span.clone())))
            }
        }
        else {
            Ok(None)
        }
    }

    fn assume_ty_factor(&mut self) -> Result<Option<Box<TyAst<'input>>>> {
        if let Some(ty) = self.assume_ty_paren()? {
            Ok(Some(ty))
        }
        else if let Some(tvar) = self.assume_tvar()? {
            Ok(Some(tvar_ty_ast(tvar.clone(), tvar.span.clone())))
        }
        else if let Some(lifetime) = self.assume_lifetime()? {
            Ok(Some(lifetime_ty_ast(lifetime.clone(), lifetime.span.clone())))
        }
        else if let Some(base) = self.assume_base()? {
            Ok(Some(base_ty_ast(base.clone(), base.span.clone())))
        }
        else {
            Ok(None)
        }
    }

    fn assume_ty_paren(&mut self) -> Result<Option<Box<TyAst<'input>>>>  {
        if let Some(_) = self.assume_simple_token(TokenEnum::LParen)? {
            if let Some(ty) = self.assume_ty()? {
                if let Some(_) = self.assume_simple_token(TokenEnum::RParen)? {
                    return Ok(Some(ty))
                }
                bail_node_required!(self.tokens, "`)`")
            }
            bail_node_required!(self.tokens, "type expression")
        }
        else {
            Ok(None)
        }
    }

    fn assume_tvar(&mut self) -> Result<Option<TVarAst<'input>>> {
        if let Some((name, span)) = self.assume_lower_ident()? {
            Ok(Some(tvar_ast(name, span)))
        }
        else {
            Ok(None)
        }
    }

    fn assume_lifetime(&mut self) -> Result<Option<LifetimeAst<'input>>> {
        if let Some((_, quote_span)) = self.assume_simple_token(TokenEnum::SingleQuote)? {
            if let Some((name, ident_span)) = self.assume_lower_ident()? {
                let extended = quote_span.extend(&ident_span);
                return Ok(Some(lifetime_ast(format!("'{}", name), extended)))
            }
            bail_node_required!(self.tokens, "lifetime identifier")
        }
        else {
            Ok(None)
        }
    }

    fn assume_base(&mut self) -> Result<Option<BaseAst<'input>>> {
        if let Some((name, span)) = self.assume_upper_ident()? {
            Ok(Some(base_ast(name, span)))
        }
        else {
            Ok(None)
        }
    }

    fn assume_left_fn_def(&mut self) -> Result<Option<LeftFnDefAst<'input>>> {
        if let Some((name, span)) = self.assume_lower_ident()? {
            let mut args = Vec::new();
            let mut arg_span = None;
            while let Some(arg) = self.assume_wrapped_factor()? {
                args.push(arg.clone());
                arg_span = Some(arg.span);
            }
            let extended =
                if let Some(arg_span) = arg_span {
                    span.extend(&arg_span)
                }
                else {
                    span
                };
            Ok(Some(left_fn_def_ast(name, args, extended)))
        }
        else {
            Ok(None)
        }
    }

    fn assume_expr(&mut self) -> Result<Option<Box<ExprAst<'input>>>> {
        if let Some(lhs) = self.assume_prefix_op_lhs()? {
            let expr = self.assume_infix_op_rhs(0, lhs)?;
            Ok(Some(expr))
        }
        else {
            Ok(None)
        }
    }

    fn assume_prefix_op_lhs(&mut self) -> Result<Option<Box<ExprAst<'input>>>> {
        if let Some(Token(TokenEnum::OpCode(op_code), span)) = self.tokens.peek() {
            let op_code = op_code.clone();
            let span = span.clone();
            self.tokens.next();
            if let Some(term) = self.assume_term()? {
                let name = self.prefix_op_name(&op_code)?;
                let extended = span.extend(&term.span);
                return Ok(Some(app_expr_ast(prefix_op_ast(name, term, extended.clone(), span), extended)));
            }
            bail_node_required!(self.tokens, "term");
        }
        else if let Some(term) = self.assume_term()? {
            Ok(Some(term))
        }
        else {
            Ok(None)
        }
    }

    fn assume_infix_op_rhs(&mut self, expr_prec: usize, mut lhs: Box<ExprAst<'input>>) -> Result<Box<ExprAst<'input>>> {
        while let Some(Token(TokenEnum::OpCode(op_code), span)) = self.tokens.peek() {
            let op_code = op_code.clone();
            let span = span.clone();
            let (prec, assoc) = self.op_code_precedence(&op_code)?;
            if prec < expr_prec {
                return Ok(lhs);
            }
            self.tokens.next();
            if let Some(rhs) = self.assume_term()? {
                let mut rhs = rhs.clone();
                if let Some(Token(TokenEnum::OpCode(next_op_code), _)) = self.tokens.peek() {
                    let next_op_code = next_op_code.clone();
                    let (next_prec, _) = self.op_code_precedence(&next_op_code)?;
                    if assoc == Assoc::L && prec < next_prec {
                        rhs = self.assume_infix_op_rhs(prec + 1, rhs)?;
                    }
                    else if assoc == Assoc::R && prec <= next_prec {
                        rhs = self.assume_infix_op_rhs(prec, rhs)?;
                    }
                }
                if op_code == "<|" {
                    let extended = lhs.span.extend(&rhs.span);
                    lhs = app_expr_ast(app_ast(lhs, rhs, extended.clone()), extended);
                }
                else {
                    let name = self.infix_op_name(&op_code)?;
                    let extended = lhs.span.extend(&rhs.span);
                    let infix_op = infix_op_ast(name, lhs.clone(), rhs, extended.clone(), span, lhs.span.clone());
                    lhs = app_expr_ast(infix_op, extended);
                }
            }
            else {
                bail_node_required!(self.tokens, "term");
            }
        }
        Ok(lhs)
    }

    fn assume_term(&mut self) -> Result<Option<Box<ExprAst<'input>>>> {
        if let Some(mut term) = self.assume_access()? {
            if let ExprAst { expr_enum: ExprEnum::Access(_) | ExprEnum::Var(_) | ExprEnum::IntNum(_) | ExprEnum::RealNum(_), span } = term.as_ref() {
                term = app_expr_ast(unary_app_ast(term.clone(), span.clone()), span.clone());
            }
            while let Some(mut factor) = self.assume_access()? {
                if let ExprAst { expr_enum: ExprEnum::Access(_) | ExprEnum::Var(_) | ExprEnum::IntNum(_) | ExprEnum::RealNum(_), span } = factor.as_ref() {
                    factor = app_expr_ast(unary_app_ast(factor.clone(), span.clone()), span.clone())
                }
                let extended = term.span.extend(&factor.span);
                term = app_expr_ast(app_ast(term, factor, extended.clone()), extended);
            }
            Ok(Some(term))
        }
        else {
            Ok(None)
        }
    }

    fn assume_access(&mut self) -> Result<Option<Box<ExprAst<'input>>>> {
        if let Some(factor) = self.assume_factor()? {
            if let Some(_) = self.assume_simple_token(TokenEnum::Dot)? {
                if let Some((field, span)) = self.assume_lower_ident()? {
                    let extended = factor.span.extend(&span);
                    let access = access_expr_ast(access_ast(factor, field, extended.clone()), extended);
                    return Ok(Some(access));
                }
                bail_node_required!(self.tokens, "field name");
            }
            Ok(Some(factor))
        }
        else {
            Ok(None)
        }
    }

    fn assume_wrapped_factor(&mut self) -> Result<Option<Box<ExprAst<'input>>>> {
        if let Some(factor) = self.assume_factor()? {
            if let ExprAst { expr_enum: ExprEnum::App(_), .. } = factor.as_ref() {
                Ok(Some(factor))
            }
            else {
                Ok(Some(app_expr_ast(unary_app_ast(factor.clone(), factor.span.clone()), factor.span.clone())))
            }
        }
        else {
            Ok(None)
        }
    }

    fn assume_factor(&mut self) -> Result<Option<Box<ExprAst<'input>>>> {
        if let Some(expr) = self.assume_paren()? {
            Ok(Some(expr))
        }
        else if let Some(var) = self.assume_var()? {
            Ok(Some(var_expr_ast(var.clone(), var.span)))
        }
        else if let Some(int_num) = self.assume_int_num()? {
            Ok(Some(int_num_expr_ast(int_num.clone(), int_num.span)))
        }
        else if let Some(real_num) = self.assume_real_num()? {
            Ok(Some(real_num_expr_ast(real_num.clone(), real_num.span)))
        }
        else {
            Ok(None)
        }
    }

    fn assume_paren(&mut self) -> Result<Option<Box<ExprAst<'input>>>>  {
        if let Some(_) = self.assume_simple_token(TokenEnum::LParen)? {
            if let Some(expr) = self.assume_expr()? {
                if let Some(_) = self.assume_simple_token(TokenEnum::RParen)? {
                    return Ok(Some(expr))
                }
                bail_node_required!(self.tokens, "`)`")
            }
            bail_node_required!(self.tokens, "expression")
        }
        else {
            Ok(None)
        }
    }

    fn assume_var(&mut self) -> Result<Option<VarAst<'input>>> {
        if let Some((name, span)) = self.assume_ident()? {
            Ok(Some(var_ast(name, span)))
        }
        else {
            Ok(None)
        }
    }

    fn assume_int_num(&mut self) -> Result<Option<IntNumAst<'input>>> {
        if let Some(Token(TokenEnum::IntNum(value), span)) = self.tokens.peek() {
            let value = value.clone();
            let span = span.clone();
            self.tokens.next();
            Ok(Some(int_num_ast(value, span)))
        }
        else {
            Ok(None)
        }
    }

    fn assume_real_num(&mut self) -> Result<Option<RealNumAst<'input>>> {
        if let Some(Token(TokenEnum::RealNum(value), span)) = self.tokens.peek() {
            let value = value.clone();
            let span = span.clone();
            self.tokens.next();
            Ok(Some(real_num_ast(value, span)))
        }
        else {
            Ok(None)
        }
    }

    fn assume_upper_ident(&mut self) -> Result<Option<(String, CodeSpan<'input>)>> {
        if let Some(Token(TokenEnum::UpperIdent(name), span)) = self.tokens.peek() {
            let name = name.clone();
            let span = span.clone();
            self.tokens.next();
            Ok(Some((name, span)))
        }
        else {
            Ok(None)
        }
    }

    fn assume_lower_ident(&mut self) -> Result<Option<(String, CodeSpan<'input>)>> {
        if let Some(Token(TokenEnum::LowerIdent(name), span)) = self.tokens.peek() {
            let name = name.clone();
            let span = span.clone();
            self.tokens.next();
            Ok(Some((name, span)))
        }
        else {
            Ok(None)
        }
    }

    fn assume_ident(&mut self) -> Result<Option<(String, CodeSpan<'input>)>> {
        if let Some(Token(TokenEnum::UpperIdent(name), span)) = self.tokens.peek() {
            let name = name.clone();
            let span = span.clone();
            self.tokens.next();
            Ok(Some((name, span)))
        }
        else if let Some(Token(TokenEnum::LowerIdent(name), span)) = self.tokens.peek() {
            let name = name.clone();
            let span = span.clone();
            self.tokens.next();
            Ok(Some((name, span)))
        }
        else {
            Ok(None)
        }
    }

    fn assume_simple_token(&mut self, assumed: TokenEnum) -> Result<Option<(TokenEnum, CodeSpan<'input>)>> {
        if let Some(Token(token, span)) = self.tokens.peek() {
            let token = token.clone();
            if token == assumed {
                let span = span.clone();
                self.tokens.next();
                Ok(Some((token, span)))
            }
            else {
                Ok(None)
            }
        }
        else {
            Ok(None)
        }
    }

    fn prefix_op_name(&mut self, op_code: &str) -> Result<String> {
        match op_code {
            "-" => Ok("negate".to_string()),
            _ => bail_invalid_syntax!(self.tokens, "prefix operator"),
        }
    }

    fn infix_op_name(&mut self, op_code: &str) -> Result<String> {
        match op_code {
            "+" => Ok("add".to_string()),
            "-" => Ok("sub".to_string()),
            "*" => Ok("mul".to_string()),
            "/" => Ok("div".to_string()),
            _ => bail_invalid_syntax!(self.tokens, "infix operator"),
        }
    }

    fn op_code_precedence(&mut self, op_code: &str) -> Result<(usize, Assoc)> {
        match op_code {
            "." => Ok((10, Assoc::L)),
            "*" | "/" => Ok((7, Assoc::L)),
            "+" | "-" => Ok((6, Assoc::L)),
            "<|" => Ok((1, Assoc::R)),
            _ => bail_invalid_syntax!(self.tokens, "infix operator"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        data::{
            self,
            *,
        },
        lexer,
    };

    fn parse<'input>(s: &'input str) -> Vec<TopDefEnum<'input>> {
        super::parse(lexer::lex(s).unwrap()).unwrap()
    }

    fn top_data_def_ast<'input>(data_def_ast: DataDefAst<'input>) -> TopDefEnum<'input> {
        data::top_data_def_ast(data_def_ast)
    }

    fn top_newty_def_ast<'input>(newty_def_ast: NewTyDefAst<'input>) -> TopDefEnum<'input> {
        data::top_newty_def_ast(newty_def_ast)
    }

    fn top_fn_def_ast<'input>(fn_def_ast: FnDefAst<'input>) -> TopDefEnum<'input> {
        data::top_fn_def_ast(fn_def_ast)
    }

    fn data_def_ast<'input>(left_ty_def: LeftTyDefAst<'input>, ctors: &[&'input str], fieldss: &[Option<&[&'input str]>], tyss: &[&[Box<TyAst<'input>>]]) -> DataDefAst<'input> {
        let ctors = ctors.to_owned().into_iter().map(|s| s.to_owned()).collect();
        let fieldss = fieldss.to_owned().into_iter().map(|opt| opt.map(|fs| fs.to_owned().into_iter().map(|s| s.to_owned()).collect())).collect();
        let tyss = tyss.to_owned().into_iter().map(|ts| ts.to_owned().into_iter().collect()).collect();
        data::data_def_ast(left_ty_def, ctors, fieldss, tyss, dummy_span())
    }

    fn newty_def_ast<'input>(left_ty_def: LeftTyDefAst<'input>, ctor: &'input str, ty: Box<TyAst<'input>>) -> NewTyDefAst<'input> {
        let ctor = ctor.to_owned();
        data::newty_def_ast(left_ty_def, ctor, ty, dummy_span())
    }

    fn newty_rec_def_ast<'input>(left_ty_def: LeftTyDefAst<'input>, ctor: &'input str, field: &'input str, ty: Box<TyAst<'input>>) -> NewTyDefAst<'input> {
        let ctor = ctor.to_owned();
        let field = field.to_owned();
        data::newty_rec_def_ast(left_ty_def, ctor, field, ty, dummy_span())
    }

    fn left_ty_def_ast<'input>(name: &'input str, args: &[Box<TyAst<'input>>]) -> LeftTyDefAst<'input> {
        let name = name.to_owned();
        let args = args.to_owned().into_iter().collect();
        data::left_ty_def_ast(name, args, dummy_span())
    }

    fn arrow_ty_ast<'input>(arrow: ArrowAst<'input>) -> Box<TyAst<'input>> {
        data::arrow_ty_ast(arrow, dummy_span())
    }

    fn tapp_ty_ast<'input>(tapp: TAppAst<'input>) -> Box<TyAst<'input>> {
        data::tapp_ty_ast(tapp, dummy_span())
    }

    fn tvar_ty_ast<'input>(tvar: TVarAst<'input>) -> Box<TyAst<'input>> {
        data::tvar_ty_ast(tvar, dummy_span())
    }

    fn lifetime_ty_ast<'input>(lifetime: LifetimeAst<'input>) -> Box<TyAst<'input>> {
        data::lifetime_ty_ast(lifetime, dummy_span())
    }

    fn base_ty_ast<'input>(base: BaseAst<'input>) -> Box<TyAst<'input>> {
        data::base_ty_ast(base, dummy_span())
    }

    fn arrow_ast<'input>(lhs: Box<TyAst<'input>>, rhs: Box<TyAst<'input>>) -> ArrowAst<'input> {
        data::arrow_ast(lhs, rhs, dummy_span())
    }

    fn tapp_ast<'input>(ty_fn: Box<TyAst<'input>>, ty_arg: Box<TyAst<'input>>, ref_attr: RefAttrAst<'input>) -> TAppAst<'input> {
        data::tapp_ast(ty_fn, ty_arg, ref_attr, dummy_span())
    }

    fn unary_tapp_ast<'input>(ty_fn: Box<TyAst<'input>>, ref_attr: RefAttrAst<'input>) -> TAppAst<'input> {
        data::unary_tapp_ast(ty_fn, ref_attr, dummy_span())
    }

    fn tvar_ast<'input>(name: &'input str) -> TVarAst<'input> {
        let name = name.to_owned();
        data::tvar_ast(name, dummy_span())
    }

    fn lifetime_ast<'input>(name: &'input str) -> LifetimeAst<'input> {
        let name = name.to_owned();
        data::lifetime_ast(name, dummy_span())
    }

    fn base_ast<'input>(name: &'input str) -> BaseAst<'input> {
        let name = name.to_owned();
        data::base_ast(name, dummy_span())
    }

    fn ty_fn_def_ast<'input>(ty_annot: Box<TyAst<'input>>, left_fn_def: LeftFnDefAst<'input>, expr: Box<ExprAst<'input>>) -> FnDefAst<'input> {
        data::fn_def_ast(Some(ty_annot), left_fn_def, expr, dummy_span())
    }

    fn fn_def_ast<'input>(left_fn_def: LeftFnDefAst<'input>, expr: Box<ExprAst<'input>>) -> FnDefAst<'input> {
        data::fn_def_ast(None, left_fn_def, expr, dummy_span())
    }

    fn left_fn_def_ast<'input>(name: &'input str, args: &[Box<ExprAst<'input>>]) -> LeftFnDefAst<'input> {
        let name = name.to_owned();
        let args = args.to_owned().into_iter().collect();
        data::left_fn_def_ast(name, args, dummy_span())
    }

    fn app_expr_ast<'input>(app_ast: AppAst<'input>) -> Box<ExprAst<'input>> {
        data::app_expr_ast(app_ast, dummy_span())
    }

    fn var_expr_ast<'input>(var_ast: VarAst<'input>) -> Box<ExprAst<'input>> {
        data::var_expr_ast(var_ast, dummy_span())
    }

    fn int_num_expr_ast<'input>(int_num_ast: IntNumAst<'input>) -> Box<ExprAst<'input>> {
        data::int_num_expr_ast(int_num_ast, dummy_span())
    }

    fn real_num_expr_ast<'input>(real_num_ast: RealNumAst<'input>) -> Box<ExprAst<'input>> {
        data::real_num_expr_ast(real_num_ast, dummy_span())
    }

    fn app_ast<'input>(fn_expr: Box<ExprAst<'input>>, arg_expr: Box<ExprAst<'input>>) -> AppAst<'input> {
        data::app_ast(fn_expr, arg_expr, dummy_span())
    }

    fn unary_app_ast<'input>(fn_expr: Box<ExprAst<'input>>) -> AppAst<'input> {
        data::unary_app_ast(fn_expr, dummy_span())
    }

    fn prefix_op_ast<'input>(op_code: &'input str, rhs: Box<ExprAst<'input>>) -> AppAst<'input> {
        let op_code = op_code.to_owned();
        data::prefix_op_ast(op_code, rhs, dummy_span(), dummy_span())
    }

    fn infix_op_ast<'input>(op_code: &'input str, lhs: Box<ExprAst<'input>>, rhs: Box<ExprAst<'input>>) -> AppAst<'input> {
        let op_code = op_code.to_owned();
        data::infix_op_ast(op_code, lhs, rhs, dummy_span(), dummy_span(), dummy_span())
    }

    fn var_ast<'input>(name: &'input str) -> VarAst<'input> {
        let name = name.to_owned();
        data::var_ast(name, dummy_span())
    }

    fn int_num_ast<'input>(value: &'input str) -> IntNumAst<'input> {
        let value = value.to_owned();
        data::int_num_ast(value, dummy_span())
    }

    fn real_num_ast<'input>(value: &'input str) -> RealNumAst<'input> {
        let value = value.to_owned();
        data::real_num_ast(value, dummy_span())
    }

    fn dummy_span<'a>() -> CodeSpan<'a> {
        CodeSpan::eof()
    }

    #[test]
    fn test_parse_empty() {
        assert_eq!(parse(""), &[]);
    }

    #[test]
    fn test_parse_arg() {
        assert_eq!(parse("fn f a = 0"), &[
            top_fn_def_ast(
                fn_def_ast(
                    left_fn_def_ast("f", &[
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("a")),
                        )),
                    ]),
                    app_expr_ast(unary_app_ast(
                        int_num_expr_ast(int_num_ast("0")),
                    )),
                ),
            ),
        ]);
        assert_eq!(parse("fn f a b = 0"), &[
            top_fn_def_ast(
                fn_def_ast(
                    left_fn_def_ast("f", &[
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("a")),
                        )),
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("b")),
                        )),
                    ]),
                    app_expr_ast(unary_app_ast(
                        int_num_expr_ast(int_num_ast("0")),
                    )),
                ),
            ),
        ]);
    }

    #[test]
    fn test_parse_ident() {
        assert_eq!(parse("fn f a = a"), &[
            top_fn_def_ast(
                fn_def_ast(
                    left_fn_def_ast("f", &[
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("a")),
                        )),
                    ]),
                    app_expr_ast(unary_app_ast(
                        var_expr_ast(var_ast("a")),
                    )),
                ),
            ),
        ]);
        assert_eq!(parse("fn f = f"), &[
            top_fn_def_ast(
                fn_def_ast(
                    left_fn_def_ast("f", &[]),
                    app_expr_ast(unary_app_ast(
                        var_expr_ast(var_ast("f")),
                    )),
                ),
            ),
        ]);
    }

    #[test]
    fn test_parse_int_num() {
        assert_eq!(parse("fn f = 0"), &[
            top_fn_def_ast(
                fn_def_ast(
                    left_fn_def_ast("f", &[]),
                    app_expr_ast(unary_app_ast(
                        int_num_expr_ast(int_num_ast("0")),
                    )),
                ),
            ),
        ]);
        assert_eq!(parse("fn f = 123"), &[
            top_fn_def_ast(
                fn_def_ast(
                    left_fn_def_ast("f", &[]),
                    app_expr_ast(unary_app_ast(
                        int_num_expr_ast(int_num_ast("123")),
                    ))
                ),
            ),
        ]);
    }

    #[test]
    fn test_parse_real_num() {
        assert_eq!(parse("fn f = 0.0"), &[
            top_fn_def_ast(
                fn_def_ast(
                    left_fn_def_ast("f", &[]),
                    app_expr_ast(unary_app_ast(
                        real_num_expr_ast(real_num_ast("0.0")),
                    )),
                ),
            ),
        ]);
        assert_eq!(parse("fn f = 125.125"), &[
            top_fn_def_ast(
                fn_def_ast(
                    left_fn_def_ast("f", &[]),
                    app_expr_ast(unary_app_ast(
                        real_num_expr_ast(real_num_ast("125.125")),
                    )),
                ),
            ),
        ]);
    }

    #[test]
    fn test_parse_fn() {
        assert_eq!(parse("fn f g a = g a"), &[
            top_fn_def_ast(
                fn_def_ast(
                    left_fn_def_ast("f", &[
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("g")),
                        )),
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("a")),
                        )),
                    ]),
                    app_expr_ast(
                        app_ast(
                            app_expr_ast(unary_app_ast(
                                var_expr_ast(var_ast("g")),
                            )),
                            app_expr_ast(unary_app_ast(
                                var_expr_ast(var_ast("a")),
                            )),
                        ),
                    ),
                ),
            ),
        ]);
        assert_eq!(parse("fn f g a b = g a b"), &[
            top_fn_def_ast(
                fn_def_ast(
                    left_fn_def_ast("f", &[
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("g")),
                        )),
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("a")),
                        )),
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("b")),
                        )),
                    ]),
                    app_expr_ast(
                        app_ast(
                            app_expr_ast(
                                app_ast(
                                    app_expr_ast(unary_app_ast(
                                        var_expr_ast(var_ast("g")),
                                    )),
                                    app_expr_ast(unary_app_ast(
                                        var_expr_ast(var_ast("a")),
                                    )),
                                ),
                            ),
                            app_expr_ast(unary_app_ast(
                                var_expr_ast(var_ast("b")),
                            )),
                        ),
                    ),
                ),
            ),
        ]);
    }

    #[test]
    fn test_parse_infix_op() {
        assert_eq!(parse("fn f a = a + 1"), &[
            top_fn_def_ast(
                fn_def_ast(
                    left_fn_def_ast("f", &[
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("a")),
                        )),
                    ]),
                    app_expr_ast(
                        infix_op_ast(
                            "add",
                            app_expr_ast(unary_app_ast(
                                var_expr_ast(var_ast("a")),
                            )),
                            app_expr_ast(unary_app_ast(
                                int_num_expr_ast(int_num_ast("1")),
                            )),
                        ),
                    ),
                ),
            ),
        ]);
        assert_eq!(parse("fn f g a b c = g a + g b + c"), &[
            top_fn_def_ast(
                fn_def_ast(
                    left_fn_def_ast("f", &[
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("g")),
                        )),
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("a")),
                        )),
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("b")),
                        )),
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("c")),
                        )),
                    ]),
                    app_expr_ast(
                        infix_op_ast(
                            "add",
                            app_expr_ast(
                                infix_op_ast(
                                    "add",
                                    app_expr_ast(
                                        app_ast(
                                            app_expr_ast(unary_app_ast(
                                                var_expr_ast(var_ast("g")),
                                            )),
                                            app_expr_ast(unary_app_ast(
                                                var_expr_ast(var_ast("a")),
                                            )),
                                        ),
                                    ),
                                    app_expr_ast(
                                        app_ast(
                                            app_expr_ast(unary_app_ast(
                                                var_expr_ast(var_ast("g")),
                                            )),
                                            app_expr_ast(unary_app_ast(
                                                var_expr_ast(var_ast("b")),
                                            )),
                                        ),
                                    ),
                                ),
                            ),
                            app_expr_ast(unary_app_ast(
                                var_expr_ast(var_ast("c")),
                            )),
                        ),
                    ),
                ),
            ),
        ]);
    }

    #[test]
    fn test_parse_infix_op_prec() {
        assert_eq!(parse("fn f a b c = a * b + c"), &[
            top_fn_def_ast(
                fn_def_ast(
                    left_fn_def_ast("f", &[
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("a")),
                        )),
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("b")),
                        )),
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("c")),
                        )),
                    ]),
                    app_expr_ast(
                        infix_op_ast(
                            "add",
                            app_expr_ast(
                                infix_op_ast(
                                    "mul",
                                    app_expr_ast(unary_app_ast(
                                        var_expr_ast(var_ast("a")),
                                    )),
                                    app_expr_ast(unary_app_ast(
                                        var_expr_ast(var_ast("b")),
                                    )),
                                ),
                            ),
                            app_expr_ast(unary_app_ast(
                                var_expr_ast(var_ast("c")),
                            )),
                        ),
                    ),
                ),
            ),
        ]);
        assert_eq!(parse("fn f a b c = a + b * c"), &[
            top_fn_def_ast(
                fn_def_ast(
                    left_fn_def_ast("f", &[
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("a")),
                        )),
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("b")),
                        )),
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("c")),
                        )),
                    ]),
                    app_expr_ast(
                        infix_op_ast(
                            "add",
                            app_expr_ast(unary_app_ast(
                                var_expr_ast(var_ast("a")),
                            )),
                            app_expr_ast(
                                infix_op_ast(
                                    "mul",
                                    app_expr_ast(unary_app_ast(
                                        var_expr_ast(var_ast("b")),
                                    )),
                                    app_expr_ast(unary_app_ast(
                                        var_expr_ast(var_ast("c")),
                                    )),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ]);
    }

    #[test]
    fn test_parse_infix_op_right_assoc() {
        assert_eq!(parse("fn f a b c = a <| b <| c"), &[
            top_fn_def_ast(
                fn_def_ast(
                    left_fn_def_ast("f", &[
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("a")),
                        )),
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("b")),
                        )),
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("c")),
                        )),
                    ]),
                    app_expr_ast(
                        app_ast(
                            app_expr_ast(unary_app_ast(
                                var_expr_ast(var_ast("a")),
                            )),
                            app_expr_ast(
                                app_ast(
                                    app_expr_ast(unary_app_ast(
                                        var_expr_ast(var_ast("b")),
                                    )),
                                    app_expr_ast(unary_app_ast(
                                        var_expr_ast(var_ast("c")),
                                    )),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ]);
    }

    #[test]
    fn test_parse_paren() {
        assert_eq!(parse("fn f a b c = (a + b) + c"), &[
            top_fn_def_ast(
                fn_def_ast(
                    left_fn_def_ast("f", &[
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("a")),
                        )),
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("b")),
                        )),
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("c")),
                        )),
                    ]),
                    app_expr_ast(
                        infix_op_ast(
                            "add",
                            app_expr_ast(
                                infix_op_ast(
                                    "add",
                                    app_expr_ast(unary_app_ast(
                                        var_expr_ast(var_ast("a")),
                                    )),
                                    app_expr_ast(unary_app_ast(
                                        var_expr_ast(var_ast("b")),
                                    )),
                                ),
                            ),
                            app_expr_ast(unary_app_ast(
                                var_expr_ast(var_ast("c")),
                            )),
                        ),
                    ),
                ),
            ),
        ]);
        assert_eq!(parse("fn f = a + (b + c)"), &[
            top_fn_def_ast(
                fn_def_ast(
                    left_fn_def_ast("f", &[]),
                    app_expr_ast(
                        infix_op_ast(
                            "add",
                            app_expr_ast(unary_app_ast(
                                var_expr_ast(var_ast("a")),
                            )),
                            app_expr_ast(
                                infix_op_ast(
                                    "add",
                                    app_expr_ast(unary_app_ast(
                                        var_expr_ast(var_ast("b")),
                                    )),
                                    app_expr_ast(unary_app_ast(
                                        var_expr_ast(var_ast("c")),
                                    )),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ]);
    }

    #[test]
    fn test_parse_prefix_op() {
        assert_eq!(parse("fn f = -1"), &[
            top_fn_def_ast(
                fn_def_ast(
                    left_fn_def_ast("f", &[]),
                    app_expr_ast(
                        prefix_op_ast(
                            "negate",
                            app_expr_ast(unary_app_ast(
                                int_num_expr_ast(int_num_ast("1")),
                            )),
                        ),
                    ),
                ),
            ),
        ]);
        assert_eq!(parse("fn f = -a + 1"), &[
            top_fn_def_ast(
                fn_def_ast(
                    left_fn_def_ast("f", &[]),
                    app_expr_ast(
                        infix_op_ast(
                            "add",
                            app_expr_ast(
                                prefix_op_ast(
                                    "negate",
                                    app_expr_ast(unary_app_ast(
                                        var_expr_ast(var_ast("a")),
                                    )),
                                ),
                            ),
                            app_expr_ast(unary_app_ast(
                                int_num_expr_ast(int_num_ast("1")),
                            )),
                        ),
                    ),
                ),
            ),
        ]);
    }

    #[test]
    fn test_parse_ty_annot() {
        assert_eq!(parse("ty I64 -> I64 fn f a = 0"), &[
            top_fn_def_ast(
                ty_fn_def_ast(
                    arrow_ty_ast(
                        arrow_ast(
                            tapp_ty_ast(unary_tapp_ast(
                                base_ty_ast(base_ast("I64")),
                                RefAttrAst::Unconstrained,
                            )),
                            tapp_ty_ast(unary_tapp_ast(
                                base_ty_ast(base_ast("I64")),
                                RefAttrAst::Unconstrained,
                            )),
                        ),
                    ),
                    left_fn_def_ast("f", &[
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("a")),
                        )),
                    ]),
                    app_expr_ast(unary_app_ast(
                        int_num_expr_ast(int_num_ast("0")),
                    )),
                ),
            ),
        ]);
        assert_eq!(parse("ty (I64 -> a) -> I64 -> a fn f a b = a b"), &[
            top_fn_def_ast(
                ty_fn_def_ast(
                    arrow_ty_ast(
                        arrow_ast(
                            arrow_ty_ast(
                                arrow_ast(
                                    tapp_ty_ast(unary_tapp_ast(
                                        base_ty_ast(base_ast("I64")),
                                        RefAttrAst::Unconstrained,
                                    )),
                                    tapp_ty_ast(unary_tapp_ast(
                                        tvar_ty_ast(tvar_ast("a")),
                                        RefAttrAst::Unconstrained,
                                    )),
                                ),
                            ),
                            arrow_ty_ast(
                                arrow_ast(
                                    tapp_ty_ast(unary_tapp_ast(
                                        base_ty_ast(base_ast("I64")),
                                        RefAttrAst::Unconstrained,
                                    )),
                                    tapp_ty_ast(unary_tapp_ast(
                                        tvar_ty_ast(tvar_ast("a")),
                                        RefAttrAst::Unconstrained,
                                    )),
                                ),
                            ),
                        ),
                    ),
                    left_fn_def_ast("f", &[
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("a")),
                        )),
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("b")),
                        )),
                    ]),
                    app_expr_ast(
                        app_ast(
                            app_expr_ast(unary_app_ast(
                                var_expr_ast(var_ast("a")),
                            )),
                            app_expr_ast(unary_app_ast(
                                var_expr_ast(var_ast("b")),
                            )),
                        ),
                    ),
                ),
            ),
        ]);
    }

    #[test]
    fn test_parse_ref_ty() {
        assert_eq!(parse("ty &MyData -> &MyData fn f x = x"), &[
            top_fn_def_ast(
                ty_fn_def_ast(
                    arrow_ty_ast(
                        arrow_ast(
                            tapp_ty_ast(unary_tapp_ast(
                                base_ty_ast(base_ast("MyData")),
                                RefAttrAst::Ref(None),
                            )),
                            tapp_ty_ast(unary_tapp_ast(
                                base_ty_ast(base_ast("MyData")),
                                RefAttrAst::Ref(None),
                            )),
                        ),
                    ),
                    left_fn_def_ast("f", &[
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("x")),
                        )),
                    ]),
                    app_expr_ast(unary_app_ast(
                        var_expr_ast(var_ast("x")),
                    )),
                ),
            ),
        ]);
        assert_eq!(parse("ty &'a MyData -> &'a MyData fn f x = x"), &[
            top_fn_def_ast(
                ty_fn_def_ast(
                    arrow_ty_ast(
                        arrow_ast(
                            tapp_ty_ast(unary_tapp_ast(
                                base_ty_ast(base_ast("MyData")),
                                RefAttrAst::Ref(Some(lifetime_ast("'a"))),
                            )),
                            tapp_ty_ast(unary_tapp_ast(
                                base_ty_ast(base_ast("MyData")),
                                RefAttrAst::Ref(Some(lifetime_ast("'a"))),
                            )),
                        ),
                    ),
                    left_fn_def_ast("f", &[
                        app_expr_ast(unary_app_ast(
                            var_expr_ast(var_ast("x")),
                        )),
                    ]),
                    app_expr_ast(unary_app_ast(
                        var_expr_ast(var_ast("x")),
                    )),
                ),
            ),
        ]);
    }

    #[test]
    fn test_parse_data_def() {
        assert_eq!(parse("data MyData = MyData I64 F64"), &[
            top_data_def_ast(
                data_def_ast(
                    left_ty_def_ast("MyData", &[]),
                    &["MyData"],
                    &[None],
                    &[&[
                        tapp_ty_ast(unary_tapp_ast(
                            base_ty_ast(base_ast("I64")),
                            RefAttrAst::Unconstrained,
                        )),
                        tapp_ty_ast(unary_tapp_ast(
                            base_ty_ast(base_ast("F64")),
                            RefAttrAst::Unconstrained,
                        )),
                    ]],
                ),
            ),
        ]);
        assert_eq!(parse("data MyData = MyData1 I64 | MyData2 { unMyI: I64, unMyF: F64 }"), &[
            top_data_def_ast(
                data_def_ast(
                    left_ty_def_ast("MyData", &[]),
                    &["MyData1", "MyData2"],
                    &[None, Some(&["unMyI", "unMyF"])],
                    &[
                        &[
                            tapp_ty_ast(unary_tapp_ast(
                                base_ty_ast(base_ast("I64")),
                                RefAttrAst::Unconstrained,
                            )),
                        ],
                        &[
                            tapp_ty_ast(unary_tapp_ast(
                                base_ty_ast(base_ast("I64")),
                                RefAttrAst::Unconstrained,
                            )),
                            tapp_ty_ast(unary_tapp_ast(
                                base_ty_ast(base_ast("F64")),
                                RefAttrAst::Unconstrained,
                            )),
                        ],
                    ],
                ),
            ),
        ]);
    }

    #[test]
    fn test_parse_newty_def() {
        assert_eq!(parse("newty MyI64 = MyI64 I64"), &[
            top_newty_def_ast(
                newty_def_ast(
                    left_ty_def_ast("MyI64", &[]),
                    "MyI64",
                    tapp_ty_ast(unary_tapp_ast(
                        base_ty_ast(base_ast("I64")),
                        RefAttrAst::Unconstrained,
                    )),
                ),
            ),
        ]);
        assert_eq!(parse("newty MyI64 = MyI64 { unMy: I64 }"), &[
            top_newty_def_ast(
                newty_rec_def_ast(
                    left_ty_def_ast("MyI64", &[]),
                    "MyI64",
                    "unMy",
                    tapp_ty_ast(unary_tapp_ast(
                        base_ty_ast(base_ast("I64")),
                        RefAttrAst::Unconstrained,
                    )),
                ),
            ),
        ]);
    }

    #[test]
    fn test_parse_ty_args() {
        assert_eq!(parse("data MyData a 'a b 'b = MyData (a 'a) (b 'b)"), &[
            top_data_def_ast(
                data_def_ast(
                    left_ty_def_ast("MyData", &[
                        tapp_ty_ast(unary_tapp_ast(
                            tvar_ty_ast(tvar_ast("a")),
                            RefAttrAst::Unconstrained,
                        )),
                        tapp_ty_ast(unary_tapp_ast(
                            lifetime_ty_ast(lifetime_ast("'a")),
                            RefAttrAst::Unconstrained,
                        )),
                        tapp_ty_ast(unary_tapp_ast(
                            tvar_ty_ast(tvar_ast("b")),
                            RefAttrAst::Unconstrained,
                        )),
                        tapp_ty_ast(unary_tapp_ast(
                            lifetime_ty_ast(lifetime_ast("'b")),
                            RefAttrAst::Unconstrained,
                        )),
                    ]),
                    &["MyData"],
                    &[None],
                    &[&[
                        tapp_ty_ast(tapp_ast(
                            tapp_ty_ast(unary_tapp_ast(
                                tvar_ty_ast(tvar_ast("a")),
                                RefAttrAst::Unconstrained,
                            )),
                            tapp_ty_ast(unary_tapp_ast(
                                lifetime_ty_ast(lifetime_ast("'a")),
                                RefAttrAst::Unconstrained,
                            )),
                            RefAttrAst::Unconstrained,
                        )),
                        tapp_ty_ast(tapp_ast(
                            tapp_ty_ast(unary_tapp_ast(
                                tvar_ty_ast(tvar_ast("b")),
                                RefAttrAst::Unconstrained,
                            )),
                            tapp_ty_ast(unary_tapp_ast(
                                lifetime_ty_ast(lifetime_ast("'b")),
                                RefAttrAst::Unconstrained,
                            )),
                            RefAttrAst::Unconstrained,
                        )),
                    ]],
                ),
            ),
        ]);
    }
}
