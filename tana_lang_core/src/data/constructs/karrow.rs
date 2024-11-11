use std::rc::Rc;
use crate::{
    impl_construct_val,
    impl_construct_key,
    data::*,
};

#[derive(Clone, Debug)]
pub struct KArrow {
    pub id: usize,
    pub in_kn: Rc<Kn>,
    pub out_kn: Rc<Kn>,
    pub rank: usize,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct KArrowKey {
    pub in_kn: Box<KnKey>,
    pub out_kn: Box<KnKey>,
}

impl_construct_val!(KArrow);

impl ConstructVal for KArrow {
    type Key = KArrowKey;

    fn to_key(&self) -> Self::Key {
        Self::Key {
            in_kn: Box::new(self.in_kn.to_key()),
            out_kn: Box::new(self.out_kn.to_key()),
        }
    }
}

impl_construct_key!(KArrowKey, KArrow, karrow_store);

impl Construct for KArrowKey {
    fn logical_name(&self) -> String {
        format!(
            "{}__{}",
            self.in_kn.logical_name(),
            self.out_kn.logical_name()
        )
    }

    fn description(&self) -> String {
        format!(
            "{} -> {}",
            self.in_kn.description(),
            self.out_kn.description()
        )
    }
}

impl KArrow {
    pub fn new_or_get(ctx: &mut SemantizerContext, in_kn: Rc<Kn>, out_kn: Rc<Kn>) -> Rc<Self> {
        let rank = out_kn.rank() + 1;
        let val = Rc::new(Self {
            id: ctx.karrow_store.next_id(),
            in_kn,
            out_kn,
            rank,
        });
        let key = val.to_key();
        ctx.karrow_store.insert_or_get(key, val)
    }
}

impl KArrowKey {
    pub fn new(in_kn: KnKey, out_kn: KnKey) -> Self {
        Self {
            in_kn: Box::new(in_kn),
            out_kn: Box::new(out_kn),
        }
    }
}
