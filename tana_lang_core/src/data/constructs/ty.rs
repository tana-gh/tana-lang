use std::{
    cell::RefCell,
    collections::{
        HashMap,
        HashSet,
    },
    hash::{
        Hash,
        Hasher,
    },
    rc::Rc,
};
use anyhow::{
    bail,
    Result,
};
use crate::{
    impl_construct_key,
    data::*,
};

#[derive(Clone, Debug)]
pub enum Ty {
    TVar(Rc<TVar>),
    Base(Rc<Base>),
    Arrow(Rc<Arrow>),
    TApp(Rc<TApp>),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TyKey {
    TVar(TVarKey),
    Base(BaseKey),
    Arrow(ArrowKey),
    TApp(TAppKey),
}

impl PartialEq for Ty {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::TVar(tvar), Self::TVar(other_tvar)) =>
                tvar == other_tvar,
            (Self::Base(base), Self::Base(other_base)) =>
                base == other_base,
            (Self::Arrow(arrow), Self::Arrow(other_arrow)) =>
                arrow == other_arrow,
            (Self::TApp(tapp), Self::TApp(other_tapp)) =>
                tapp == other_tapp,
            _ => false,
        }
    }
}

impl Eq for Ty {}

impl Hash for Ty {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::TVar(tvar) =>
                [0, tvar.id].hash(state),
            Self::Base(base) =>
                [1, base.id].hash(state),
            Self::Arrow(arrow) =>
                [2, arrow.id].hash(state),
            Self::TApp(tapp) =>
                [3, tapp.id].hash(state),
        };
    }
}

impl Construct for Ty {
    fn logical_name(&self) -> String {
        self.to_key().logical_name()
    }

    fn description(&self) -> String {
        self.to_key().description()
    }
}

impl ConstructVal for Ty {
    type Key = TyKey;

    fn to_key(&self) -> Self::Key {
        match self {
            Self::TVar(tvar) =>
                Self::Key::TVar(tvar.to_key()),
            Self::Base(base) =>
                Self::Key::Base(base.to_key()),
            Self::Arrow(arrow) =>
                Self::Key::Arrow(arrow.to_key()),
            Self::TApp(tapp) =>
                Self::Key::TApp(tapp.to_key()),
        }
    }
}

impl_construct_key!(TyKey, Ty, ty_store);

impl Construct for TyKey {
    fn logical_name(&self) -> String {
        match self {
            Self::TVar(tvar) =>
                tvar.logical_name(),
            Self::Base(base) =>
                base.logical_name(),
            Self::Arrow(arrow) =>
                arrow.logical_name(),
            Self::TApp(tapp) =>
                tapp.logical_name(),
        }
    }

    fn description(&self) -> String {
        match self {
            Self::TVar(tvar) =>
                tvar.description(),
            Self::Base(base) =>
                base.description(),
            Self::Arrow(arrow) =>
                arrow.description(),
            Self::TApp(tapp) =>
                tapp.description(),
        }
    }
}

impl Ty {
    pub fn unary_tapp_unknown(ctx: &SemantizerContext) -> Rc<Self> {
        TyKey::unary_tapp_unknown(ctx).get_val(ctx).unwrap()
    }

    pub fn unary_tapp_bottom(ctx: &SemantizerContext) -> Rc<Self> {
        TyKey::unary_tapp_bottom(ctx).get_val(ctx).unwrap()
    }

    pub fn unary_tapp_i64(ctx: &SemantizerContext) -> Rc<Self> {
        TyKey::unary_tapp_i64(ctx).get_val(ctx).unwrap()
    }

    pub fn unary_tapp_f64(ctx: &SemantizerContext) -> Rc<Self> {
        TyKey::unary_tapp_f64(ctx).get_val(ctx).unwrap()
    }

    pub fn unary_tapp_base(ctx: &mut SemantizerContext, qual: Rc<Qual>, name: String) -> Rc<Self> {
        TyKey::unary_tapp_base(qual.to_key(), name).get_val(ctx).unwrap()
    }

    pub fn new_or_get_with_tvar(ctx: &mut SemantizerContext, tvar: Rc<TVar>) -> Rc<Self> {
        let val = Rc::new(Self::TVar(tvar));
        let key = val.to_key();
        ctx.ty_store.insert_or_get(key, val)
    }

    pub fn new_or_get_with_base(ctx: &mut SemantizerContext, base: Rc<Base>) -> Rc<Self> {
        let val = Rc::new(Self::Base(base));
        let key = val.to_key();
        ctx.ty_store.insert_or_get(key, val)
    }

    pub fn new_or_get_with_arrow(ctx: &mut SemantizerContext, arrow: Rc<Arrow>) -> Rc<Self> {
        let val = Rc::new(Self::Arrow(arrow));
        let key = val.to_key();
        ctx.ty_store.insert_or_get(key, val)
    }

    pub fn new_or_get_with_tapp(ctx: &mut SemantizerContext, tapp: Rc<TApp>) -> Rc<Self> {
        let val = Rc::new(Self::TApp(tapp));
        let key = val.to_key();
        ctx.ty_store.insert_or_get(key, val)
    }

    pub fn new_as_tvar(ctx: &mut SemantizerContext, qual: Rc<Qual>, name: String, tvar_kind: TVarKind, kn: Rc<Kn>) -> Result<Rc<Self>> {
        let tvar = TVar::new(ctx, qual, name, tvar_kind, kn)?;
        Ok(Self::new_or_get_with_tvar(ctx, tvar))
    }

    pub fn new_as_base(ctx: &mut SemantizerContext, qual: Rc<Qual>, name: String, kn: Rc<Kn>) -> Result<Rc<Self>> {
        let base = Base::new(ctx, qual, name, kn)?;
        Ok(Self::new_or_get_with_base(ctx, base))
    }

    pub fn new_or_get_as_tvar(ctx: &mut SemantizerContext, qual: Rc<Qual>, name: String, tvar_kind: TVarKind, kn: Rc<Kn>) -> Rc<Self> {
        let tvar = TVar::new_or_get(ctx, qual, name, tvar_kind, kn);
        Self::new_or_get_with_tvar(ctx, tvar)
    }

    pub fn new_or_get_as_base(ctx: &mut SemantizerContext, qual: Rc<Qual>, name: String, kn: Rc<Kn>) -> Rc<Self> {
        let base = Base::new_or_get(ctx, qual, name, kn);
        Self::new_or_get_with_base(ctx, base)
    }

    pub fn new_or_get_as_arrow(ctx: &mut SemantizerContext, in_ty: Rc<Ty>, out_ty: Rc<Ty>) -> Rc<Self> {
        let arrow = Arrow::new_or_get(ctx, in_ty, out_ty);
        Self::new_or_get_with_arrow(ctx, arrow)
    }

    pub fn new_or_get_as_tapp(ctx: &mut SemantizerContext, ty_fn: Rc<Ty>, ty_arg: Rc<Ty>, ref_attr: RefAttr) -> Rc<Self> {
        let tapp = TApp::new_or_get(ctx, ty_fn, ty_arg, ref_attr);
        Self::new_or_get_with_tapp(ctx, tapp)
    }

    pub fn new_or_get_as_unary_tapp(ctx: &mut SemantizerContext, ty_fn: Rc<Ty>, ref_attr: RefAttr) -> Rc<Self> {
        let tapp = TApp::new_or_get_as_unary(ctx, ty_fn, ref_attr);
        Self::new_or_get_with_tapp(ctx, tapp)
    }

    pub fn new_or_get_as_fn_ty(ctx: &mut SemantizerContext, in_tys: Vec<Rc<Ty>>, out_ty: Rc<Ty>) -> Rc<Self> {
        let mut ty = out_ty;
        for in_ty in in_tys.into_iter().rev() {
            ty = Self::new_or_get_as_arrow(ctx, in_ty, ty);
        }
        ty
    }

    pub fn new_or_get_as_tapp_ty(ctx: &mut SemantizerContext, ty_fn: Rc<Ty>, ty_args: Vec<Rc<Ty>>, ref_attr: RefAttr) -> Rc<Self> {
        let mut ty = ty_fn;
        for ty_arg in ty_args {
            ty = Self::new_or_get_as_tapp(ctx, ty, ty_arg, ref_attr.clone());
        }
        ty
    }

    pub fn to_arg_and_ret_tys(self: &Rc<Self>) -> (Vec<Rc<Self>>, Rc<Self>) {
        let mut tys = Vec::new();
        let mut ty = self.clone();
        loop {
            match ty.as_ref() {
                Self::TVar(_) => return (tys, ty.clone()),
                Self::Base(_) => return (tys, ty.clone()),
                Self::Arrow(arrow) => {
                    tys.push(arrow.in_ty.clone());
                    ty = arrow.out_ty.clone();
                },
                Self::TApp(_) => return (tys, ty.clone()),
            }
        }
    }

    pub fn to_in_ty(self: &Rc<Self>) -> Rc<Self> {
        match self.as_ref() {
            Self::TVar(_) => self.clone(),
            Self::Base(_) => self.clone(),
            Self::Arrow(arrow) => arrow.in_ty.clone(),
            Self::TApp(_) => self.clone(),
        }
    }

    pub fn to_out_ty(self: &Rc<Self>) -> Rc<Self> {
        match self.as_ref() {
            Self::TVar(_) => self.clone(),
            Self::Base(_) => self.clone(),
            Self::Arrow(arrow) => arrow.out_ty.clone(),
            Self::TApp(_) => self.clone(),
        }
    }

    pub fn to_last_ty(self: &Rc<Self>) -> Rc<Self> {
        match self.as_ref() {
            Self::TVar(_) => self.clone(),
            Self::Base(_) => self.clone(),
            Self::Arrow(arrow) => arrow.out_ty.to_last_ty(),
            Self::TApp(_) => self.clone(),
        }
    }

    pub fn to_ty_fn(self: &Rc<Self>) -> Rc<Self> {
        match self.as_ref() {
            Self::TVar(_) => self.clone(),
            Self::Base(_) => self.clone(),
            Self::Arrow(_) => self.clone(),
            Self::TApp(tapp) => tapp.ty_fn.clone(),
        }
    }

    pub fn to_first_ty_fn(self: &Rc<Self>) -> Rc<Self> {
        match self.as_ref() {
            Self::TVar(_) => self.clone(),
            Self::Base(_) => self.clone(),
            Self::Arrow(_) => self.clone(),
            Self::TApp(tapp) => tapp.ty_fn.to_first_ty_fn(),
        }
    }

    pub fn assign_ref_attr(self: &Rc<Self>, ctx: &mut SemantizerContext, ref_attr: RefAttr) -> Rc<Self> {
        match self.as_ref() {
            Self::TVar(_) | Self::Base(_) =>
                Self::new_or_get_as_unary_tapp(ctx, self.clone(), ref_attr),
            Self::Arrow(arrow) => {
                let in_ty = arrow.in_ty.assign_ref_attr(ctx, ref_attr.clone());
                let out_ty = arrow.out_ty.assign_ref_attr(ctx, ref_attr.clone());
                Self::new_or_get_as_arrow(ctx, in_ty, out_ty)
            },
            Self::TApp(_) =>
                self.clone(),
        }
    }

    pub fn unwrap_tvar(&self) -> Rc<TVar> {
        match self {
            Self::TVar(tvar) =>
                tvar.clone(),
            _ => panic!("Unwrapping non-TVar Ty: `{}`", self.description()),
        }
    }

    pub fn unwrap_base(&self) -> Rc<Base> {
        match self {
            Self::Base(base) =>
                base.clone(),
            _ => panic!("Unwrapping non-Base Ty: `{}`", self.description()),
        }
    }

    pub fn unwrap_arrow(&self) -> Rc<Arrow> {
        match self {
            Self::Arrow(arrow) =>
                arrow.clone(),
            _ => panic!("Unwrapping non-Arrow Ty: `{}`", self.description()),
        }
    }

    pub fn unwrap_tapp(&self) -> Rc<TApp> {
        match self {
            Self::TApp(tapp) =>
                tapp.clone(),
            _ => panic!("Unwrapping non-TApp Ty: `{}`", self.description()),
        }
    }

    pub fn unwrap_unary_tapp(&self) -> Rc<Self> {
        match self {
            Self::TApp(tapp) if tapp.ty_arg.is_none() =>
                tapp.ty_fn.clone(),
            _ => panic!("Unwrapping non-unary TApp Ty: `{}`", self.description()),
        }
    }

    pub fn unwrap_unary_tapp_tvar(&self) -> Rc<TVar> {
        match self {
            Self::TApp(tapp) if tapp.ty_arg.is_none() =>
                tapp.ty_fn.unwrap_tvar(),
            _ => panic!("Unwrapping non-TVar Ty: `{}`", self.description()),
        }
    }

    pub fn unwrap_unary_tapp_base(&self) -> Rc<Base> {
        match self {
            Self::TApp(tapp) if tapp.ty_arg.is_none() =>
                tapp.ty_fn.unwrap_base(),
            _ => panic!("Unwrapping non-Base Ty: `{}`", self.description()),
        }
    }

    pub fn rank(&self) -> usize {
        match self {
            Self::TVar(_) => 0,
            Self::Base(_) => 0,
            Self::Arrow(arrow) => arrow.rank,
            Self::TApp(_) => 0,
        }
    }

    pub fn is_unknown(&self) -> bool {
        self.to_key().is_unknown()
    }

    pub fn is_bottom(&self) -> bool {
        self.to_key().is_bottom()
    }

    pub fn is_nondeterministic(&self, ctx: &SemantizerContext) -> bool {
        match self {
            Self::TVar(tvar) =>
                tvar.tvar_kind == TVarKind::Type || tvar.tvar_kind == TVarKind::Lifetime && tvar.qual.to_key() == QualKey::top(ctx),
            Self::Base(_) =>
                false,
            Self::Arrow(arrow) =>
                arrow.in_ty.is_nondeterministic(ctx) || arrow.out_ty.is_nondeterministic(ctx),
            Self::TApp(tapp) =>
                tapp.ty_fn.is_nondeterministic(ctx) || tapp.ty_arg.as_ref().map_or(false, |ty| ty.is_nondeterministic(ctx)) ||
                if let RefAttr::Ref(lifetime) = &tapp.ref_attr {
                    lifetime.is_nondeterministic(ctx)
                }
                else {
                    false
                },
        }
    }

    pub fn is_ref(&self) -> bool {
        match self {
            Self::TApp(tapp) =>
                matches!(tapp.ref_attr, RefAttr::Ref(_)),
            _ => false,
        }
    }

    pub fn is_owned_ref(&self, qual: Rc<Qual>) -> bool {
        match self {
            Self::TApp(tapp) =>
                match &tapp.ref_attr {
                    RefAttr::Ref(lifetime) =>
                        lifetime.unwrap_unary_tapp_tvar().qual == qual,
                    _ => false,
                },
            _ => false,
        }
    }

    pub fn is_unowned_ref(&self, qual: Rc<Qual>) -> bool {
        match self {
            Self::TApp(tapp) =>
                match &tapp.ref_attr {
                    RefAttr::Ref(lifetime) =>
                        lifetime.unwrap_unary_tapp_tvar().qual != qual,
                    _ => false,
                },
            _ => false,
        }
    }

    pub fn get_tvars(&self) -> HashSet<Rc<TVar>> {
        match self {
            Self::TVar(tvar) =>
                vec![tvar.clone()].into_iter().collect(),
            Self::Base(_) =>
                HashSet::new(),
            Self::Arrow(arrow) => {
                let mut tvars = arrow.in_ty.get_tvars();
                tvars.extend(arrow.out_ty.get_tvars());
                tvars
            },
            Self::TApp(tapp) => {
                let mut tvars = tapp.ty_fn.get_tvars();
                if let Some(ty_arg) = tapp.ty_arg.as_ref() {
                    tvars.extend(ty_arg.get_tvars());
                }
                if let RefAttr::Ref(lifetime) = &tapp.ref_attr {
                    tvars.extend(lifetime.get_tvars());
                }
                tvars
            },
        }
    }

    pub fn get_ref_attr(&self) -> Option<RefAttr> {
        match self {
            Self::TApp(tapp) =>
                Some(tapp.ref_attr.clone()),
            _ => None,
        }
    }
    
    pub fn apply_from(&self, ty: Rc<Self>) -> Result<HashMap<TVarKey, Rc<Ty>>> {
        match self {
            Self::Arrow(arrow) =>
                arrow.in_ty.assign_from(ty),
            _ => bail!("Cannot apply `{}` to `{}`", ty.description(), self.description()),
        }
    }

    pub fn assign_from(&self, ty: Rc<Self>) -> Result<HashMap<TVarKey, Rc<Ty>>> {
        match (self, ty.as_ref()) {
            (Self::TVar(tvar), Self::TVar(ty_tvar)) =>
                match (&tvar.tvar_kind, &ty_tvar.tvar_kind) {
                    (TVarKind::Type, TVarKind::Type) =>
                        Ok([(tvar.to_key(), ty)].into_iter().collect()),
                    (TVarKind::Lifetime, TVarKind::Lifetime) =>
                        Ok([(tvar.to_key(), ty)].into_iter().collect()),
                    _ => bail!("Cannot assign `{}` to `{}`", ty.description(), self.description()),
                },
            (Self::TVar(tvar), _) =>
                Ok([(tvar.to_key(), ty)].into_iter().collect()),
            (Self::Base(base), Self::Base(ty_base)) if base == ty_base =>
                Ok(HashMap::new()),
            (Self::Arrow(arrow), Self::Arrow(ty_arrow)) => {
                let mut map = arrow.in_ty.assign_from(ty_arrow.in_ty.clone())?;
                map.extend(arrow.out_ty.assign_from(ty_arrow.out_ty.clone())?);
                Ok(map)
            },
            (Self::TApp(tapp), Self::TVar(_) | Self::Base(_)) if tapp.ty_arg.is_none() =>
                tapp.ty_fn.assign_from(ty),
            (Self::Base(_), Self::TApp(ty_tapp)) =>
                self.assign_from(ty_tapp.ty_fn.clone()),
            (Self::TApp(tapp), Self::TApp(ty_tapp)) => {
                match (&tapp.ref_attr, &ty_tapp.ref_attr) {
                    (RefAttr::Unconstrained, _)  if tapp.ty_arg.is_none() =>
                        tapp.ty_fn.assign_from(ty),
                    (RefAttr::Unconstrained, RefAttr::Unconstrained) =>
                        match (tapp.ty_arg.as_ref(), ty_tapp.ty_arg.as_ref()) {
                            (Some(tapp_arg), Some(ty_tapp_arg)) => {
                                let mut map = tapp.ty_fn.assign_from(ty_tapp.ty_fn.clone())?;
                                map.extend(tapp_arg.assign_from(ty_tapp_arg.clone())?);
                                Ok(map)
                            },
                            (None, None) =>
                                tapp.ty_fn.assign_from(ty_tapp.ty_fn.clone()),
                            _ => bail!("Cannot assign `{}` to `{}`", ty.description(), self.description()),
                        },
                    (RefAttr::Ref(lifetime), RefAttr::Ref(ty_lifetime)) =>
                        match (tapp.ty_arg.as_ref(), ty_tapp.ty_arg.as_ref()) {
                            (Some(tapp_arg), Some(ty_tapp_arg)) => {
                                let mut map = tapp.ty_fn.assign_from(ty_tapp.ty_fn.clone())?;
                                map.extend(tapp_arg.assign_from(ty_tapp_arg.clone())?);
                                map.extend(lifetime.assign_from(ty_lifetime.clone())?);
                                Ok(map)
                            },
                            (None, None) => {
                                let mut map = tapp.ty_fn.assign_from(ty_tapp.ty_fn.clone())?;
                                map.extend(lifetime.assign_from(ty_lifetime.clone())?);
                                Ok(map)
                            },
                            _ => bail!("Cannot assign `{}` to `{}`", ty.description(), self.description()),
                        },
                    _ => bail!("Cannot assign `{}` to `{}`", ty.description(), self.description()),
                }
            },
            _ => bail!("Cannot assign `{}` to `{}`", ty.description(), self.description()),
        }
    }

    pub fn get_ty_env_for_assign(&self, ctx: &mut SemantizerContext, ty: Rc<Self>) -> Result<Rc<RefCell<TyEnv>>> {
        let map = self.assign_from(ty.clone())?;
        let tvars = map.keys().cloned().collect();
        let ty_env = TyEnv::new(ctx, tvars);
        {
            let mut ty_env = ty_env.borrow_mut();
            for (tvar, ty) in map {
                ty_env.assign(ctx, tvar, ty)?;
            }
        }
        Ok(ty_env)
    }

    pub fn kn(&self) -> Rc<Kn> {
        match self {
            Self::TVar(tvar) =>
                tvar.kn.clone(),
            Self::Base(base) =>
                base.kn.clone(),
            Self::Arrow(arrow) =>
                arrow.kn.clone(),
            Self::TApp(tapp) =>
                tapp.kn.clone(),
        }
    }
}

impl TyKey {
    pub fn unary_tapp_unknown(ctx: &SemantizerContext) -> Self {
        Self::unary_tapp_base(QualKey::top(ctx), "Unknown".to_owned())
    }

    pub fn unary_tapp_bottom(ctx: &SemantizerContext) -> Self {
        Self::unary_tapp_base(QualKey::top(ctx), "Bottom".to_owned())
    }

    pub fn unary_tapp_i64(ctx: &SemantizerContext) -> Self {
        Self::unary_tapp_base(QualKey::top(ctx), "I64".to_owned())
    }

    pub fn unary_tapp_f64(ctx: &SemantizerContext) -> Self {
        Self::unary_tapp_base(QualKey::top(ctx), "F64".to_owned())
    }

    pub fn unary_tapp_base(qual: QualKey, name: String) -> Self {
        let base = Self::new_as_base(qual, name);
        Self::new_as_unary_tapp(base, RefAttrKey::Unconstrained)
    }

    pub fn new_with_tvar(tvar: TVarKey) -> Self {
        Self::TVar(tvar)
    }

    pub fn new_with_base(base: BaseKey) -> Self {
        Self::Base(base)
    }

    pub fn new_with_arrow(arrow: ArrowKey) -> Self {
        Self::Arrow(arrow)
    }

    pub fn new_with_tapp(tapp: TAppKey) -> Self {
        Self::TApp(tapp)
    }

    pub fn new_as_tvar(qual: QualKey, name: String, tvar_kind: TVarKind) -> Self {
        Self::TVar(TVarKey::new(qual, name, tvar_kind))
    }

    pub fn new_as_base(qual: QualKey, name: String) -> Self {
        Self::Base(BaseKey::new(qual, name))
    }

    pub fn new_as_arrow(in_ty: TyKey, out_ty: TyKey) -> Self {
        Self::Arrow(ArrowKey::new(in_ty, out_ty))
    }

    pub fn new_as_tapp(ty_fn: TyKey, ty_arg: TyKey, ref_attr: RefAttrKey) -> Self {
        Self::TApp(TAppKey::new(ty_fn, ty_arg, ref_attr))
    }

    pub fn new_as_unary_tapp(ty_fn: TyKey, ref_attr: RefAttrKey) -> Self {
        Self::TApp(TAppKey::new_as_unary(ty_fn, ref_attr))
    }

    pub fn to_unary_tapp(&self, ref_attr: RefAttrKey) -> Self {
        match self {
            Self::TVar(_) | Self::Base(_) =>
                Self::new_as_unary_tapp(self.clone(), ref_attr),
            Self::Arrow(_) | Self::TApp(_) =>
                self.clone(),
        }
    }

    pub fn is_unknown(&self) -> bool {
        match self {
            Self::Base(base) => base.name == "Unknown",
            Self::Arrow(arrow) => arrow.in_ty.is_unknown() || arrow.out_ty.is_unknown(),
            Self::TApp(tapp) => tapp.ty_fn.is_unknown() || tapp.ty_arg.as_ref().map_or(false, |ty| ty.is_unknown()),
            _ => false,
        }
    }

    pub fn is_bottom(&self) -> bool {
        match self {
            Self::Base(base) => base.name == "Bottom",
            Self::Arrow(arrow) => arrow.in_ty.is_bottom() || arrow.out_ty.is_bottom(),
            Self::TApp(tapp) => tapp.ty_fn.is_bottom() || tapp.ty_arg.as_ref().map_or(false, |ty| ty.is_bottom()),
            _ => false,
        }
    }
}
