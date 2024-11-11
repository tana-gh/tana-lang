use std::rc::Rc;
use crate::{
    impl_id,
    data::*,
};

#[derive(Clone, Debug)]
pub struct NewTy {
    pub id: usize,
    pub ty: Rc<Ty>,
    pub oldty: Rc<Ty>,
    pub ctor: Rc<Var>,
    pub field: Option<Rc<Var>>,
}

impl_id!(NewTy);

impl Construct for NewTy {
    fn logical_name(&self) -> String {
        format!("newty_{}", self.id)
    }

    fn description(&self) -> String {
        format!("newty[{}]", self.id)
    }
}

impl NewTy {
    pub fn new_with_id(ctx: &mut SemantizerContext, id: usize, ty: Rc<Ty>, oldty: Rc<Ty>, ctor: Rc<Var>, field: Option<Rc<Var>>) -> Rc<Self> {
        let val = Rc::new(Self {
            id,
            ty,
            oldty,
            ctor,
            field,
        });
        ctx.newty_store.insert(id, val).unwrap()
    }
}
