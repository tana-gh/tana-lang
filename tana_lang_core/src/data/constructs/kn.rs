use std::{
    hash::{
        Hash,
        Hasher,
    },
    rc::Rc,
};
use crate::{
    impl_construct_key,
    data::*,
};

#[derive(Clone, Debug)]
pub enum Kn {
    KBase(Rc<KBase>),
    KArrow(Rc<KArrow>),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum KnKey {
    KBase(KBaseKey),
    KArrow(KArrowKey),
}

impl PartialEq for Kn {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Kn::KBase(kbase), Kn::KBase(other_kbase)) =>
                kbase == other_kbase,
            (Kn::KArrow(karrow), Kn::KArrow(other_karrow)) =>
                karrow == other_karrow,
            _ => false,
        }
    }
}

impl Eq for Kn {}

impl Hash for Kn {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Kn::KBase(kbase) =>
                [0, kbase.id].hash(state),
            Kn::KArrow(karrow) =>
                [1, karrow.id].hash(state),
        }
    }
}

impl Construct for Kn {
    fn logical_name(&self) -> String {
        self.to_key().logical_name()
    }

    fn description(&self) -> String {
        self.to_key().description()
    }
}

impl ConstructVal for Kn {
    type Key = KnKey;

    fn to_key(&self) -> Self::Key {
        match self {
            Kn::KBase(kbase) =>
                Self::Key::KBase(kbase.to_key()),
            Kn::KArrow(karrow) =>
                Self::Key::KArrow(karrow.to_key()),
        }
    }
}

impl_construct_key!(KnKey, Kn, kn_store);

impl Construct for KnKey {
    fn logical_name(&self) -> String {
        match self {
            KnKey::KBase(kbase) =>
                kbase.logical_name(),
            KnKey::KArrow(karrow) =>
                karrow.logical_name(),
        }
    }

    fn description(&self) -> String {
        match self {
            KnKey::KBase(kbase) =>
                kbase.description(),
            KnKey::KArrow(karrow) =>
                karrow.description(),
        }
    }
}

impl Kn {
    pub fn aster(ctx: &SemantizerContext) -> Rc<Self> {
        KnKey::aster().get_val(ctx).unwrap()
    }

    pub fn lifetime(ctx: &SemantizerContext) -> Rc<Self> {
        KnKey::lifetime().get_val(ctx).unwrap()
    }

    pub fn new_or_get_with_kbase(ctx: &mut SemantizerContext, kbase: Rc<KBase>) -> Rc<Self> {
        let val = Rc::new(Self::KBase(kbase));
        let key = val.to_key();
        ctx.kn_store.insert_or_get(key, val)
    }

    pub fn new_or_get_with_karrow(ctx: &mut SemantizerContext, karrow: Rc<KArrow>) -> Rc<Self> {
        let val = Rc::new(Self::KArrow(karrow));
        let key = val.to_key();
        ctx.kn_store.insert_or_get(key, val)
    }

    pub fn new_or_get_as_kbase(ctx: &mut SemantizerContext, name: String) -> Rc<Self> {
        let kbase = KBase::new_or_get(ctx, name);
        Self::new_or_get_with_kbase(ctx, kbase)
    }

    pub fn new_or_get_as_karrow(ctx: &mut SemantizerContext, in_kn: Rc<Self>, out_kn: Rc<Self>) -> Rc<Self> {
        let karrow = KArrow::new_or_get(ctx, in_kn, out_kn);
        Self::new_or_get_with_karrow(ctx, karrow)
    }

    pub fn new_or_get_as_fn_kn(ctx: &mut SemantizerContext, in_kns: Vec<Rc<Self>>, out_kn: Rc<Self>) -> Rc<Self> {
        let mut kn = out_kn;
        for in_kn in in_kns.into_iter().rev() {
            kn = Self::new_or_get_as_karrow(ctx, in_kn, kn);
        }
        kn
    }

    pub fn to_arg_and_ret_kns(self: &Rc<Self>) -> (Vec<Rc<Self>>, Rc<Self>) {
        let mut kns = Vec::new();
        let mut kn = self.clone();
        loop {
            match kn.as_ref() {
                Self::KBase(_) => return (kns, kn.clone()),
                Self::KArrow(karrow) => {
                    kns.push(karrow.in_kn.clone());
                    kn = karrow.out_kn.clone();
                },
            }
        }
    }

    pub fn to_out_kn(self: &Rc<Self>) -> Rc<Self> {
        match self.as_ref() {
            Self::KBase(_) =>
                self.clone(),
            Self::KArrow(karrow) =>
                karrow.out_kn.clone(),
        }
    }

    pub fn unwrap_kbase(&self) -> Rc<KBase> {
        match self {
            Self::KBase(kbase) =>
                kbase.clone(),
            _ => panic!("Unwrapping non-KBase Kn: `{}`", self.description()),
        }
    }

    pub fn unwrap_karrow(&self) -> Rc<KArrow> {
        match self {
            Self::KArrow(karrow) =>
                karrow.clone(),
            _ => panic!("Unwrapping non-KArrow Kn: `{}`", self.description()),
        }
    }

    pub fn rank(&self) -> usize {
        match self {
            Kn::KBase(_) => 0,
            Kn::KArrow(karrow) => karrow.rank,
        }
    }
}

impl KnKey {
    pub fn aster() -> Self {
        Self::new_as_kbase("*".to_owned())
    }

    pub fn lifetime() -> Self {
        Self::new_as_kbase("Lifetime".to_owned())
    }

    pub fn new_as_kbase(name: String) -> Self {
        Self::KBase(KBaseKey::new(name))
    }

    pub fn new_as_karrow(in_kn: KnKey, out_kn: KnKey) -> Self {
        Self::KArrow(KArrowKey::new(in_kn, out_kn))
    }
}
