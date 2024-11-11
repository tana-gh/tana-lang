use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
};
use anyhow::{
    bail,
    Result,
};
use crate::data::*;

#[derive(Clone, Debug, PartialEq)]
pub struct TyEnv {
    tys: HashMap<TVarKey, Rc<Ty>>,
}

impl TyEnv {
    pub fn new(ctx: &SemantizerContext, tvars: Vec<TVarKey>) -> Rc<RefCell<Self>> {
        let unknown = Ty::unary_tapp_unknown(ctx);
        let tys = tvars.iter().map(|tvar| (tvar.clone(), unknown.clone())).collect();
        Rc::new(RefCell::new(Self {
            tys,
        }))
    }

    pub fn new_empty() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            tys: HashMap::new(),
        }))
    }

    pub fn assign(&mut self, ctx: &mut SemantizerContext, tvar: TVarKey, ty: Rc<Ty>) -> Result<()> {
        let ty = self.apply_env(ctx, ty);
        if let Some(mut_ty) = self.tys.get_mut(&tvar) {
            if mut_ty.is_unknown() {
                *mut_ty = ty.clone();
            } else {
                bail!("Type variable `{}` is already assigned", tvar.description());
            }
        } else {
            self.tys.insert(tvar.clone(), ty.clone());
        }
        let tvar_ty = tvar.get_val(ctx)?;
        let tvar_ty = Ty::new_or_get_with_tvar(ctx, tvar_ty);
        let tvar_ty = Ty::new_or_get_as_unary_tapp(ctx, tvar_ty, RefAttr::Unconstrained);
        let mut key_and_vals = Vec::new();
        for (key, val) in self.tys.iter() {
            key_and_vals.push((key.clone(), val.clone()));
        }
        for (key, val) in key_and_vals {
            if val == tvar_ty {
                let mut_ty = self.tys.get_mut(&key).unwrap();
                *mut_ty = ty.clone();
            }
        }
        Ok(())
    }

    pub fn extend(&mut self, ctx: &mut SemantizerContext, ty_env: Rc<RefCell<Self>>) -> Result<()> {
        let ty_env = ty_env.borrow();
        for (tvar, ty) in ty_env.tys.iter() {
            self.assign(ctx, tvar.clone(), ty.clone())?;
        }
        Ok(())
    }

    pub fn get(&self, tvar: &TVarKey) -> Result<Rc<Ty>> {
        if let Some(ty) = self.tys.get(tvar) {
            Ok(ty.clone())
        } else {
            bail!("Unknown type variable: `{}`", tvar.description());
        }
    }

    pub fn is_generic(&self) -> bool {
        !self.tys.is_empty()
    }

    pub fn is_unknown(&self) -> bool {
        self.tys.values().any(|ty| ty.is_unknown())
    }

    pub fn is_nondeterministic(&self, ctx: &SemantizerContext,) -> bool {
        self.tys.values().any(|ty| ty.is_nondeterministic(ctx))
    }

    pub fn apply_qual(&mut self, ctx: &mut SemantizerContext, applied: Rc<Ty>, qual: Rc<Qual>) -> Result<Rc<Ty>> {
        if applied.rank() == 0 && applied.kn().rank() == 0 {
            if let Ty::TApp(tapp) = applied.as_ref() {
                if let RefAttr::Ref(lifetime) = &tapp.ref_attr {
                    let tvar = lifetime.unwrap_unary_tapp_tvar();
                    let new_lifetime = Ty::new_or_get_as_tvar(ctx, qual, tvar.name.clone(), tvar.tvar_kind, tvar.kn.clone());
                    let new_lifetime = Ty::new_or_get_as_unary_tapp(ctx, new_lifetime, RefAttr::Unconstrained);
                    self.assign(ctx, tvar.to_key(), new_lifetime)?;
                }
            }
        }
        Ok(self.apply_env(ctx, applied))
    }

    pub fn apply_tys(&mut self, ctx: &mut SemantizerContext, applied: Rc<Ty>, applying: Rc<Ty>, qual: Rc<Qual>) -> Result<Rc<Ty>> {
        let tys = applied.apply_from(applying.clone())?;
        for (tvar, ty) in tys {
            self.assign(ctx, tvar, ty)?;
        }
        Ok(self.apply_qual(ctx, applied.to_out_ty(), qual)?)
    }

    pub fn apply_env(&self, ctx: &mut SemantizerContext, ty: Rc<Ty>) -> Rc<Ty> {
        match ty.as_ref() {
            Ty::TVar(tvar) =>
                if let Some(applied) = self.tys.get(&tvar.to_key()) {
                    if applied.is_unknown() {
                        ty
                    }
                    else {
                        applied.clone()
                    }
                }
                else {
                    ty
                },
            Ty::Base(_) =>
                ty.clone(),
            Ty::Arrow(arrow) => {
                let in_ty = self.apply_env(ctx, arrow.in_ty.clone());
                let out_ty = self.apply_env(ctx, arrow.out_ty.clone());
                Ty::new_or_get_as_arrow(ctx, in_ty, out_ty)
            },
            Ty::TApp(tapp) => {
                let ref_attr =
                    match &tapp.ref_attr {
                        RefAttr::Unconstrained =>
                            RefAttr::Unconstrained,
                        RefAttr::Ref(lifetime) =>
                            RefAttr::Ref(self.apply_env(ctx, lifetime.clone())),
                    };
                match &tapp.ty_arg {
                    Some(ty_arg) => {
                        let ty_fn = self.apply_env(ctx, tapp.ty_fn.clone());
                        let ty_arg = self.apply_env(ctx, ty_arg.clone());
                        Ty::new_or_get_as_tapp(ctx, ty_fn, ty_arg, ref_attr)
                    },
                    None =>
                        self.apply_env(ctx, tapp.ty_fn.clone()).assign_ref_attr(ctx, ref_attr),
                }
            },
        }
    }

    pub fn get_generic_name(&self, default_name: &str) -> String {
        if self.is_generic() {
            let generic_name =
                self.tys.values()
                .map(|ty| ty.logical_name())
                .collect::<Vec<_>>()
                .join("____");
            format!("{}____{}", default_name, generic_name)
        }
        else {
            default_name.to_owned()
        }
    }

    pub fn description(&self) -> String {
        self.tys.iter()
        .map(|(tvar, ty)| format!("{}: {}", tvar.description(), ty.description()))
        .collect::<Vec<_>>()
        .join(", ")
    }
}
