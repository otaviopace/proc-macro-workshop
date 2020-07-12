extern crate proc_macro;

use quote::quote;
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use syn::{parse_macro_input, DeriveInput, Ident, Data, Fields};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    
    let struct_name = input.ident;

    let builder_name = Ident::new(&format!("{}Builder", struct_name), Span::call_site());

    let fields_with_option = get_fields_with_option(&input.data);
    let none_fields = get_none_fields(&input.data);

    let expanded = quote! {
        pub struct #builder_name { #fields_with_option }
        
        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #none_fields
                }
            }
        }
    };

    expanded.into()
}

fn get_fields_with_option(data: &Data) -> TokenStream2 {
    match *data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => {
                    fields.named.iter().map(|f| {
                        let name = &f.ident;
                        let ty = &f.ty;
                        quote! {
                            #name: Option<#ty>,
                        }
                    }).collect()
                },
                _ => unimplemented!(),
            }
        },
        _ => unimplemented!(),
    }
}

fn get_none_fields(data: &Data) -> TokenStream2 {
    match *data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => {
                    fields.named.iter().map(|f| {
                        let name = &f.ident;
                        quote! {
                            #name: None,
                        }
                    }).collect()
                },
                _ => unimplemented!(),
            }
        },
        _ => unimplemented!(),
    }
}
