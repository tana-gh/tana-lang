
#[macro_export]
macro_rules! impl_new {
    ($ty:ident, $inner:ident, $name:ident$(, $field_name:ident: $field_ty:ident)*) => {
        impl $ty {
            pub fn $name(span: &CodeSpan$(, $field_name: $field_ty)*) -> Self {
                Self::$inner($inner {
                    error_header: span.error_header(),
                    error_footer: span.error_footer(),
                    $($field_name,)*
                })
            }
        }
    };
}

#[macro_export]
macro_rules! impl_display {
    ($ty:ident, $msg:expr$(, $field:ident)*) => {
        impl std::fmt::Display for $ty {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, $msg, self.error_header$(, self.$field)*, self.error_footer)
            }
        }
    };
}
