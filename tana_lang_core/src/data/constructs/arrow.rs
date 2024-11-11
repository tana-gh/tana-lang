use std::rc::Rc;
use crate::{
    impl_construct_val,
    impl_construct_key,
    data::*,
};

#[derive(Clone, Debug)]
pub struct Arrow {
    pub id: usize,
    pub in_ty: Rc<Ty>,
    pub out_ty: Rc<Ty>,
    pub rank: usize,
    pub kn: Rc<Kn>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ArrowKey {
    pub in_ty: Box<TyKey>,
    pub out_ty: Box<TyKey>,
}

impl_construct_val!(Arrow);

impl ConstructVal for Arrow {
    type Key = ArrowKey;

    fn to_key(&self) -> Self::Key {
        Self::Key {
            in_ty: Box::new(self.in_ty.to_key()),
            out_ty: Box::new(self.out_ty.to_key()),
        }
    }
}

impl_construct_key!(ArrowKey, Arrow, arrow_store);

impl Construct for ArrowKey {
    fn logical_name(&self) -> String {
        format!(
            "{}__{}",
            self.in_ty.logical_name(),
            self.out_ty.logical_name()
        )
    }

    fn description(&self) -> String {
        format!(
            "{} -> {}",
            self.in_ty.description(),
            self.out_ty.description()
        )
    }
}

impl Arrow {
    pub fn new_or_get(ctx: &mut SemantizerContext, in_ty: Rc<Ty>, out_ty: Rc<Ty>) -> Rc<Self> {
        let rank = out_ty.rank() + 1;
        let kn = Kn::new_or_get_as_karrow(ctx, in_ty.kn(), out_ty.kn());
        let val = Rc::new(Self {
            id: ctx.arrow_store.next_id(),
            in_ty,
            out_ty,
            rank,
            kn,
        });
        let key = val.to_key();
        ctx.arrow_store.insert_or_get(key, val)
    }
}

impl ArrowKey {
    pub fn new(in_ty: TyKey, out_ty: TyKey) -> Self {
        Self {
            in_ty: Box::new(in_ty),
            out_ty: Box::new(out_ty),
        }
    }
}
