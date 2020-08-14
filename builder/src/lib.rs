extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, token::Comma, Attribute, Data, DeriveInput, Field,
    Fields, GenericArgument, Ident, Lit, Meta, NestedMeta, PathArguments, Type,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let struct_name = input.ident;

    let builder_name = Ident::new(&format!("{}Builder", struct_name), Span::call_site());
    let struct_fields = get_struct_fields(&input.data);

    let fields_with_option = envolve_fields_on_option(struct_fields);
    let none_fields = populate_fields_with_none(struct_fields);

    let builder_struct_definition = create_builder_struct(&builder_name, fields_with_option);
    let impl_builder_method_on_struct =
        impl_builder_method_on_struct(&struct_name, &builder_name, &none_fields);

    let impl_builder_methods = impl_builder_methods(&builder_name, &struct_name, struct_fields);

    let builder_error = create_builder_error();

    let expanded = quote! {
        #builder_struct_definition

        #impl_builder_method_on_struct

        #impl_builder_methods

        #builder_error
    };

    expanded.into()
}

fn get_struct_fields(data: &Data) -> &Punctuated<Field, Comma> {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => &fields.named,
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

fn envolve_fields_on_option(fields: &Punctuated<Field, Comma>) -> TokenStream2 {
    fields
        .iter()
        .map(|f| {
            let name = &f.ident;
            let ty = &f.ty;
            let ty = extract_type_from_option(&ty);
            quote! {
                #name: std::option::Option<#ty>,
            }
        })
        .collect()
}

fn populate_fields_with_none(fields: &Punctuated<Field, Comma>) -> TokenStream2 {
    fields
        .iter()
        .map(|f| {
            let name = &f.ident;
            quote! {
                #name: std::option::Option::None,
            }
        })
        .collect()
}

fn is_vec(ty: &Type) -> bool {
    if let Type::Path(typepath) = ty {
        if let Some(segment) = typepath.path.segments.last() {
            if segment.ident == "Vec" {
                if let PathArguments::AngleBracketed(ref generic_args) = segment.arguments {
                    if let Some(generic_arg) = generic_args.args.first() {
                        if let GenericArgument::Type(_) = generic_arg {
                            return true;
                        }
                    }
                }
            }
        }
    }

    false
}

fn is_option(ty: &Type) -> bool {
    if let Type::Path(typepath) = ty {
        if let Some(segment) = typepath.path.segments.last() {
            if segment.ident == "Option" {
                if let PathArguments::AngleBracketed(ref generic_args) = segment.arguments {
                    if let Some(generic_arg) = generic_args.args.first() {
                        if let GenericArgument::Type(_) = generic_arg {
                            return true;
                        }
                    }
                }
            }
        }
    }

    false
}

fn extract_type_from_option(ty: &Type) -> &Type {
    if let Type::Path(typepath) = ty {
        if let Some(segment) = typepath.path.segments.last() {
            if segment.ident == "Option" {
                if let PathArguments::AngleBracketed(ref generic_args) = segment.arguments {
                    if let Some(generic_arg) = generic_args.args.first() {
                        if let GenericArgument::Type(t) = generic_arg {
                            return t;
                        }
                    }
                }
            }
        }
    }

    ty
}

fn extract_type_from_vec(ty: &Type) -> &Type {
    if let Type::Path(typepath) = ty {
        if let Some(segment) = typepath.path.segments.last() {
            if segment.ident == "Vec" {
                if let PathArguments::AngleBracketed(ref generic_args) = segment.arguments {
                    if let Some(generic_arg) = generic_args.args.first() {
                        if let GenericArgument::Type(t) = generic_arg {
                            return t;
                        }
                    }
                }
            }
        }
    }

    ty
}

fn parse_attrs(attrs: &Vec<Attribute>) -> Option<String> {
    attrs
        .iter()
        .map(|a| match a.parse_meta() {
            Ok(meta) => match meta {
                Meta::List(ml) => {
                    let path = &ml.path;
                    let nested = &ml.nested;
                    if let Some(segment) = path.segments.last() {
                        if segment.ident == "builder" {
                            let nested_meta = nested.iter().next().unwrap();
                            if let NestedMeta::Meta(m) = nested_meta {
                                if let Meta::NameValue(name_value) = m {
                                    if name_value.path.is_ident("each") {
                                        if let Lit::Str(ref lit_str) = name_value.lit {
                                            return Some(lit_str.value());
                                        }
                                    }
                                }
                            }
                        }
                    }
                    None
                }
                _ => None,
            },
            _ => None,
        })
        .collect()
}

fn is_using_each_attr(attrs: &Vec<Attribute>) -> bool {
    attrs
        .iter()
        .next()
        .map(|a| match a.parse_meta() {
            Ok(meta) => match meta {
                Meta::List(ml) => {
                    let path = &ml.path;
                    let nested = &ml.nested;
                    if let Some(segment) = path.segments.last() {
                        if segment.ident == "builder" {
                            let nested_meta = nested.iter().next().unwrap();
                            if let NestedMeta::Meta(m) = nested_meta {
                                if let Meta::NameValue(name_value) = m {
                                    if name_value.path.is_ident("each") {
                                        if let Lit::Str(_) = name_value.lit {
                                            return true;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    false
                }
                _ => false,
            },
            _ => false,
        })
        .unwrap_or(false)
}

fn make_field_setters(fields: &Punctuated<Field, Comma>) -> TokenStream2 {
    fields
        .iter()
        .map(|f| {
            let name = &f.ident;
            let ty = &f.ty;
            let attrs = &f.attrs;

            let ty = extract_type_from_option(&ty);

            if is_vec(ty) && is_using_each_attr(attrs) {
                let ty = extract_type_from_vec(&ty);
                if let Some(new_arg) = parse_attrs(attrs) {
                    let new_arg = Ident::new(&new_arg, Span::call_site());
                    let new_code = quote! {
                        pub fn #new_arg(&mut self, #new_arg: #ty) -> &mut Self {
                            match self.#name {
                                Some(ref mut v) => {
                                    v.push(#new_arg);
                                },
                                None => {
                                    self.#name = Some(vec![#new_arg]);
                                },
                            };

                            self
                        }
                    };

                    return new_code;
                }
            }

            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        })
        .collect()
}

fn make_build_method(
    struct_name: &Ident,
    struct_fields_from_builder: TokenStream2,
) -> TokenStream2 {
    quote! {
        pub fn build(&mut self) -> std::result::Result<#struct_name, std::boxed::Box<dyn std::error::Error>> {
            Ok(#struct_name {
                #struct_fields_from_builder
            })
        }
    }
}

fn populate_builder_fields_with_error_handling(fields: &Punctuated<Field, Comma>) -> TokenStream2 {
    fields
        .iter()
        .map(|f| {
            let name = &f.ident;
            let ty = &f.ty;
            if is_option(ty) {
                quote! {
                    #name: self.#name.clone(),
                }
            } else if is_vec(ty) && is_using_each_attr(&f.attrs) {
                quote! {
                    #name: self.#name
                        .clone()
                        .unwrap_or_else(|| vec![]),
                }
            } else {
                quote! {
                    #name: self.#name
                        .as_ref()
                        .ok_or_else(|| BuilderError::NoneField(stringify!(#name).to_string()))?
                        .clone(),
                }
            }
        })
        .collect()
}

fn create_builder_error() -> TokenStream2 {
    quote! {
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
    }
}

fn create_builder_struct(builder_name: &Ident, fields: TokenStream2) -> TokenStream2 {
    quote! {
        pub struct #builder_name { #fields }
    }
}

fn impl_builder_method_on_struct(
    struct_name: &Ident,
    builder_struct_name: &Ident,
    fields: &TokenStream2,
) -> TokenStream2 {
    quote! {
        impl #struct_name {
            pub fn builder() -> #builder_struct_name {
                #builder_struct_name {
                    #fields
                }
            }
        }
    }
}

fn impl_builder_methods(
    builder_struct_name: &Ident,
    struct_name: &Ident,
    struct_fields: &Punctuated<Field, Comma>,
) -> TokenStream2 {
    let builder_setters = make_field_setters(struct_fields);
    let struct_fields_from_builder = populate_builder_fields_with_error_handling(struct_fields);
    let build_method = make_build_method(struct_name, struct_fields_from_builder);

    quote! {
        impl #builder_struct_name {
            #builder_setters

            #build_method
        }
    }
}
