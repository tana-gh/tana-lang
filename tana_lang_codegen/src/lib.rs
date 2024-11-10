use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input,
    LitStr,
};

#[proc_macro]
pub fn tana_lang(input: TokenStream) -> TokenStream {
    let tana_src_path = parse_macro_input!(input as LitStr);
    quote! {
        fn tana_src() -> String {
            use std::fs;
            fs::read_to_string(#tana_src_path).unwrap()
        }
    }.into()
}
