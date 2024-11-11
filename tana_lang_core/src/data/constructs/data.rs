use std::rc::Rc;
use crate::{
    impl_id,
    data::*,
};

#[derive(Clone, Debug)]
pub struct Data {
    pub id: usize,
    pub ty: Rc<Ty>,
    pub ctors: Vec<Rc<Var>>,
    pub fieldss: Vec<Option<Vec<Rc<Var>>>>,
}

impl_id!(Data);

impl Construct for Data {
    fn logical_name(&self) -> String {
        format!("data_{}", self.id)
    }

    fn description(&self) -> String {
        format!("data[{}]", self.id)
    }
}

impl Data {
    pub fn new_with_id(ctx: &mut SemantizerContext, id: usize, ty: Rc<Ty>, ctors: Vec<Rc<Var>>, fieldss: Vec<Option<Vec<Rc<Var>>>>) -> Rc<Self> {
        let val = Rc::new(Self {
            id,
            ty,
            ctors,
            fieldss,
        });
        ctx.data_store.insert(id, val).unwrap()
    }
}
