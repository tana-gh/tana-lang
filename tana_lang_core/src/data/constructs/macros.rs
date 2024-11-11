
#[macro_export]
macro_rules! impl_id {
    (
        $val_ty:ident
    ) => {
        impl PartialEq for $val_ty {
            fn eq(&self, other: &Self) -> bool {
                self.id == other.id
            }
        }

        impl Eq for $val_ty {}

        impl std::hash::Hash for $val_ty {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.id.hash(state);
            }
        }
    }
}

#[macro_export]
macro_rules! impl_construct_val {
    (
        $val_ty:ident
    ) => {
        crate::impl_id!($val_ty);

        impl crate::data::constructs::Construct for $val_ty {
            fn logical_name(&self) -> String {
                crate::data::constructs::ConstructVal::to_key(self).logical_name()
            }

            fn description(&self) -> String {
                crate::data::constructs::ConstructVal::to_key(self).description()
            }
        }
    };
}

#[macro_export]
macro_rules! impl_construct_key {
    (
        $key_ty:ident,
        $val_ty:ident,
        $store_name:ident
    ) => {
        impl crate::data::constructs::ConstructKey for $key_ty {
            type Val = $val_ty;

            fn get_val(
                &self, ctx: &crate::data::semantizer_context::SemantizerContext,
            ) -> anyhow::Result<std::rc::Rc<Self::Val>> {
                ctx.$store_name.get(self)
            }
        }
    };
}
