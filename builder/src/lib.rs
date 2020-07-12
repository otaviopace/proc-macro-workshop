extern crate proc_macro;

use quote::quote;
use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let _input = parse_macro_input!(input as DeriveInput);

    let expanded = quote! {

    };

    expanded.into()
}
