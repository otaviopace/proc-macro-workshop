extern crate proc_macro;

use quote::quote;
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use syn::{parse_macro_input, DeriveInput, Ident, Data, Fields, Type, PathArguments, GenericArgument};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    
    let struct_name = input.ident;

    let builder_name = Ident::new(&format!("{}Builder", struct_name), Span::call_site());

    let fields_with_option = get_fields_with_option(&input.data);
    let none_fields = get_none_fields(&input.data);
    let methods = get_builder_methods(&input.data);
    let struct_fields_from_builder = convert_builder_fields(&input.data);

    let builder_error = quote! {
        #[derive(Debug)]
        pub enum BuilderError {
            NoneField(String),
        }

        impl std::error::Error for BuilderError {}

        impl std::fmt::Display for BuilderError {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    Self::NoneField(s) => write!(f, "BuilderError::NoneField({})", s)
                }
            }
        }
    };

    let expanded = quote! {
        pub struct #builder_name { #fields_with_option }
        
        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #none_fields
                }
            }
        }

        impl #builder_name {
            #methods

            pub fn build(&mut self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                Ok(#struct_name {
                    #struct_fields_from_builder
                })
            }
        }

        #builder_error
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
                        let ty = extract_type_from_option(&ty);
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

fn is_option(ty: &Type) -> bool {
    if let Type::Path(typepath) = ty {
        if let Some(segment) = typepath.path.segments.last() {
            if segment.ident == "Option" {
                if let PathArguments::AngleBracketed(ref generic_args) = segment.arguments {
                    if let Some(generic_arg) = generic_args.args.first() {
                        if let GenericArgument::Type(_) = generic_arg {
                            return true
                        }
                    }
                }
            }
        }
    }

    false
}

fn extract_type_from_option(ty: &Type) -> Type {
    if let Type::Path(typepath) = ty {
        if let Some(segment) = typepath.path.segments.last() {
            if segment.ident == "Option" {
                if let PathArguments::AngleBracketed(ref generic_args) = segment.arguments {
                    if let Some(generic_arg) = generic_args.args.first() {
                        if let GenericArgument::Type(t) = generic_arg {
                            return t.clone()
                        }
                    }
                }
            }
        }
    }

    ty.clone()
}

fn get_builder_methods(data: &Data) -> TokenStream2 {
    match *data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => {
                    fields.named.iter().map(|f| {
                        let name = &f.ident;
                        let ty = &f.ty;

                        let ty = extract_type_from_option(&ty);
                        quote! {
                            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                                self.#name = Some(#name);
                                self
                            }
                        }
                    }).collect()
                },
                _ => unimplemented!(),
            }
        },
        _ => unimplemented!(),
    }
}

fn convert_builder_fields(data: &Data) -> TokenStream2 {
    match *data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => {
                    fields.named.iter().map(|f| {
                        let name = &f.ident;
                        let ty = &f.ty;
                        if is_option(ty) {
                            quote! {
                                #name: self.#name.clone(),
                            }
                        } else {
                            quote! {
                                #name: self.#name
                                .as_ref()
                                    .ok_or_else(|| BuilderError::NoneField(stringify!(#name).to_string()))?
                                    .clone(),
                            }
                        }
                    }).collect()
                },
                _ => unimplemented!(),
            }
        },
        _ => unimplemented!(),
    }
}
