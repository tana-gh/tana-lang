use std::rc::Rc;
use anyhow::Result;
use crate::{
    impl_construct_key,
    impl_construct_val,
    data::*,
};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum RefAttr {
    Unconstrained,
    Ref(Rc<Ty>),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum RefAttrKey {
    Unconstrained,
    Ref(TyKey),
}

#[derive(Clone, Debug)]
pub struct TApp {
    pub id: usize,
    pub ty_fn: Rc<Ty>,
    pub ty_arg: Option<Rc<Ty>>,
    pub ref_attr: RefAttr,
    pub kn: Rc<Kn>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TAppKey {
    pub ty_fn: Box<TyKey>,
    pub ty_arg: Option<Box<TyKey>>,
    pub ref_attr: Box<RefAttrKey>,
}

impl_construct_val!(TApp);

impl ConstructVal for TApp {
    type Key = TAppKey;

    fn to_key(&self) -> Self::Key {
        Self::Key {
            ty_fn: Box::new(self.ty_fn.to_key()),
            ty_arg: self.ty_arg.as_ref().map(|ty_arg| Box::new(ty_arg.to_key())),
            ref_attr:
                match &self.ref_attr {
                    RefAttr::Unconstrained => Box::new(RefAttrKey::Unconstrained),
                    RefAttr::Ref(ty) => Box::new(RefAttrKey::Ref(ty.to_key())),
                },
        }
    }
}

impl_construct_key!(TAppKey, TApp, tapp_store);

impl Construct for TAppKey {
    fn logical_name(&self) -> String {
        match &self.ty_arg {
            Some(ref ty_arg) => format!("tapp_{}__{}", self.ty_fn.logical_name(), ty_arg.logical_name()),
            None => format!("tapp_{}", self.ty_fn.logical_name()),
        }
    }

    fn description(&self) -> String {
        let ref_str =
            match self.ref_attr.as_ref() {
                RefAttrKey::Unconstrained => String::new(),
                RefAttrKey::Ref(lifetime) => format!("&{} ", lifetime.description()),
            };
        let ty_str =
            match &self.ty_arg {
                Some(ref ty_arg) => format!("{} {}", self.ty_fn.description(), ty_arg.description()),
                None => format!("{}", self.ty_fn.description()),
            };
        format!("{}{}", ref_str, ty_str)
    }
}

impl TApp {
    pub fn new_or_get(ctx: &mut SemantizerContext, ty_fn: Rc<Ty>, ty_arg: Rc<Ty>, ref_attr: RefAttr) -> Rc<Self> {
        let val = Rc::new(Self {
            id: ctx.tapp_store.next_id(),
            ty_fn: ty_fn.clone(),
            ty_arg: Some(ty_arg),
            ref_attr,
            kn: ty_fn.kn().to_out_kn().clone(),
        });
        let key = val.to_key();
        if ctx.tapp_store.insert(key.clone(), val.clone()).is_ok() {
            Self::insert_into_context(ctx, val.clone());
        }
        val
    }

    pub fn new_or_get_as_unary(ctx: &mut SemantizerContext, ty_fn: Rc<Ty>, ref_attr: RefAttr) -> Rc<Self> {
        let val = Rc::new(Self {
            id: ctx.tapp_store.next_id(),
            ty_fn: ty_fn.clone(),
            ty_arg: None,
            ref_attr,
            kn: ty_fn.kn().clone(),
        });
        let key = val.to_key();
        if ctx.tapp_store.insert(key.clone(), val.clone()).is_ok() {
            Self::insert_into_context(ctx, val.clone());
        }
        val
    }

    fn insert_into_context(ctx: &mut SemantizerContext, tapp: Rc<TApp>) {
        let ty = Ty::new_or_get_with_tapp(ctx, tapp.clone());
        if ty.kn().rank() == 0 {
            if let Ok(data) = ctx.ty_data_store.get(&ty.to_first_ty_fn().to_key()) {
                let ty_env = data.ty.get_ty_env_for_assign(ctx, ty.clone()).unwrap();
                let tyss = ctx.data_tyss_store.get(&data.ty.to_key()).unwrap();
                let tyss =
                    tyss.iter()
                    .map(|(ctor, tys)| {
                        let tys =
                            tys.iter()
                            .map(|ty| ty_env.borrow().apply_env(ctx, ty.clone()))
                            .collect();
                        (ctor.clone(), tys)
                    })
                    .collect();
                ctx.data_tyss_store.insert_or_get(ty.to_key(), tyss);
            }
            else if let Ok(newty) = ctx.ty_newty_store.get(&ty.to_first_ty_fn().to_key()) {
                let ty_env = newty.ty.get_ty_env_for_assign(ctx, ty.clone()).unwrap();
                let oldty = ty_env.borrow().apply_env(ctx, newty.oldty.clone());
                ctx.newty_ty_store.insert_or_get(ty.to_key(), oldty);
            }
        }
    }

    pub fn assign_from_lifetime(self: &Rc<Self>, ctx: &mut SemantizerContext, lifetime: Rc<Ty>) -> Rc<Self> {
        if let RefAttr::Ref(_) = &self.ref_attr {
            if let Some(ty_arg) = &self.ty_arg {
                Self::new_or_get(ctx, self.ty_fn.clone(), ty_arg.clone(), RefAttr::Ref(lifetime))
            }
            else {
                Self::new_or_get_as_unary(ctx, self.ty_fn.clone(), RefAttr::Ref(lifetime))
            }
        }
        else {
            self.clone()
        }
    }

    pub fn unapply(self: &Rc<Self>) -> Result<(Rc<Ty>, Vec<Rc<Ty>>)> {
        let mut tapp = self.clone();
        let mut ty_args = Vec::new();
        while let Some(ty_arg) = &tapp.ty_arg {
            ty_args.push(ty_arg.clone());
            if let Ty::TApp(fn_tapp) = tapp.ty_fn.as_ref() {
                tapp = fn_tapp.clone();
            }
            else {
                unreachable!();
            }
        }
        ty_args.reverse();
        let ty_fn = tapp.ty_fn.clone();
        Ok((ty_fn, ty_args))
    }
}

impl TAppKey {
    pub fn new(ty_fn: TyKey, ty_arg: TyKey, ref_attr: RefAttrKey) -> Self {
        Self {
            ty_fn: Box::new(ty_fn),
            ty_arg: Some(Box::new(ty_arg)),
            ref_attr: Box::new(ref_attr),
        }
    }

    pub fn new_as_unary(ty_fn: TyKey, ref_attr: RefAttrKey) -> Self {
        Self {
            ty_fn: Box::new(ty_fn),
            ty_arg: None,
            ref_attr: Box::new(ref_attr),
        }
    }
}
