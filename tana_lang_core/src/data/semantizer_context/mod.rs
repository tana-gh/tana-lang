mod prelude;

use std::rc::Rc;
use crate::data::*;

use self::prelude::init_prelude;

#[derive(Debug)]
pub struct SemantizerContext {
    pub top_scope: Scope,
    pub var_store: IdStore<VarKey, Var>,
    pub cn_store: IdStore<CnKey, Cn>,
    pub abs_store: GenericStore<usize, Rc<Abs>>,
    pub abs_id: IdVal,
    pub app_store: IdStore<AppKey, App>,
    pub access_store: IdStore<AccessKey, Access>,
    pub ty_store: IdStore<TyKey, Ty>,
    pub tvar_store: IdStore<TVarKey, TVar>,
    pub base_store: IdStore<BaseKey, Base>,
    pub arrow_store: IdStore<ArrowKey, Arrow>,
    pub tapp_store: IdStore<TAppKey, TApp>,
    pub data_store: GenericStore<usize, Rc<Data>>,
    pub data_id: IdVal,
    pub newty_store: GenericStore<usize, Rc<NewTy>>,
    pub newty_id: IdVal,
    pub kn_store: IdStore<KnKey, Kn>,
    pub kbase_store: IdStore<KBaseKey, KBase>,
    pub karrow_store: IdStore<KArrowKey, KArrow>,
    pub qual_store: IdStore<QualKey, Qual>,
    pub qual_stack: QualStack,
    pub ty_data_store: GenericStore<TyKey, Rc<Data>>,
    pub data_tyss_store: GenericStore<TyKey, Vec<(String, Vec<Rc<Ty>>)>>,
    pub ty_newty_store: GenericStore<TyKey, Rc<NewTy>>,
    pub newty_ty_store: GenericStore<TyKey, Rc<Ty>>,
}

impl SemantizerContext {
    pub fn new(scope_name: String) -> Self {
        let mut ctx = Self {
            top_scope: Scope::Mod(scope_name.clone()),
            var_store: IdStore::new(),
            cn_store: IdStore::new(),
            abs_store: GenericStore::new(),
            abs_id: IdVal::new(),
            app_store: IdStore::new(),
            access_store: IdStore::new(),
            ty_store: IdStore::new(),
            tvar_store: IdStore::new(),
            base_store: IdStore::new(),
            arrow_store: IdStore::new(),
            tapp_store: IdStore::new(),
            data_store: GenericStore::new(),
            data_id: IdVal::new(),
            newty_store: GenericStore::new(),
            newty_id: IdVal::new(),
            kn_store: IdStore::new(),
            kbase_store: IdStore::new(),
            karrow_store: IdStore::new(),
            qual_store: IdStore::new(),
            qual_stack: QualStack::new(scope_name),
            ty_data_store: GenericStore::new(),
            data_tyss_store: GenericStore::new(),
            ty_newty_store: GenericStore::new(),
            newty_ty_store: GenericStore::new(),
        };
        init_prelude(&mut ctx);
        ctx
    }

    pub fn push_scope_into_qual_stack(&mut self, scope: Scope) -> QualKey {
        let qual_key = self.qual_stack.peek().pushed(scope);
        let qual = Qual::new_or_get(self, &qual_key);
        self.qual_stack.push(&qual)
    }

    pub fn find_with_qual<T>(&self, pred: impl Fn(&Self, Rc<Qual>) -> Option<T>) -> Option<T> {
        for qual in self.qual_stack.iter() {
            if let Some(t) = pred(self, qual.get_val(self).unwrap()) {
                return Some(t);
            }
        }
        None
    }
}
