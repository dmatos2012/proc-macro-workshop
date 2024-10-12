#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro2::{Delimiter, Group, Punct, Spacing, TokenTree};
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, parse_str,
    spanned::Spanned,
    AttrStyle, Attribute, Data, Field, FieldsNamed, GenericArgument, Ident, Lit, LitStr, Meta,
    MetaList, Path, PathArguments, Type,
};

#[derive(Debug)]
// struct StructFields(Vec<String>);
struct StructFields(Vec<StructField>);

// #[derive(Debug)]
// struct StructField {
//     name: String,
//     ty: String,
// }

#[derive(Debug)]
struct StructField {
    name: String,
    ty: Group,
    attr: std::option::Option<String>,
}
fn check_attr_valid(attrs: &Vec<Attribute>) -> std::result::Result<(), syn::Error> {
    let maybe_attr = attrs
        .iter()
        .filter(|attr| attr.style == AttrStyle::Outer)
        .next();
    if let std::option::Option::Some(attr) = maybe_attr {
        if let Meta::List(meta_list) = &attr.meta {
            // get Path segment ident builder
            // get token stream with ident each
            //dbg!(&meta_list);
            meta_list
                .path
                .segments
                .iter()
                .find(|seg| seg.ident == "builder")
                .expect("No builder found");
            // make new empty span
            meta_list
                .tokens
                .clone()
                .into_iter()
                .map(|token| {
                    if let TokenTree::Ident(ref ident) = token {
                        let ident_str = ident.to_string();
                        if ident_str != "each" {
                            // source_text shouldnt be relied on but this gives me what I want.
                            let builder = meta_list.path.segments.span().source_text();
                            let expected_attr_msg =
                                format!(r#"`{}(each = "...")`"#, builder.unwrap());
                            // new_spanned is key to getting it right, otherwise it will
                            // just point to different parts of the span
                            return Err(syn::Error::new_spanned(
                                attr.meta.clone(),
                                format!("expected {}", expected_attr_msg),
                            ));
                        }
                    }
                    Ok(())
                })
                .collect::<std::result::Result<(), syn::Error>>()?;
        };
        Ok(())
    } else {
        // no attribute found, thats fine
        Ok(())
    }
}
fn extract_attr_str_literal(attrs: Vec<Attribute>) -> std::option::Option<String> {
    // I am looking for
    // Attr::Style::Outer
    // Meta::List(MetaList) with PathSegment ident == "builder"
    // Meta::List(MetaList) with TokenStream Ident == "each"
    // If those conditions, return the TokenStream Literal  symbol
    let maybe_attr = attrs
        .iter()
        .filter(|attr| attr.style == AttrStyle::Outer)
        .next();
    if let std::option::Option::Some(attr) = maybe_attr {
        if let Meta::List(meta_list) = &attr.meta {
            // get Path segment ident builder
            // get token stream with ident each
            // Now we need to find the value of the `each` attribute as a literal
            let builder_literal = meta_list
                .tokens
                .clone()
                .into_iter()
                .filter_map(|token| {
                    if let TokenTree::Literal(lit) = token {
                        let lit_str = syn::parse_str::<LitStr>(lit.to_string().as_str()).unwrap();
                        std::option::Option::Some(lit_str.value())
                    } else {
                        std::option::Option::None
                    }
                })
                .collect::<Vec<String>>();
            assert_eq!(builder_literal.len(), 1);
            std::option::Option::Some(builder_literal.first().unwrap().clone())
        } else {
            std::option::Option::None
        }
    } else {
        std::option::Option::None
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    let data = input.data;
    let mut struct_fields: Vec<StructField> = Vec::new();
    let name = input.ident;
    let builder_name = Ident::new(&format!("{}Builder", name), name.span());
    let visibility = input.vis;
    // lets assume for now that its always a `Data::Struct` as input,
    // but thats not necessarily the case!
    match data {
        Data::Struct(st) => {
            let fields = st.fields;
            for field in fields {
                let mut field_composition = String::new();
                let mut field_name = String::new();
                let attrs = field.attrs;
                let valid_atts = check_attr_valid(&attrs);
                if let Err(e) = valid_atts {
                    return proc_macro::TokenStream::from(e.to_compile_error());
                }
                let attr_literal = extract_attr_str_literal(attrs);
                // Iteration for EACH field
                match field.ty {
                    Type::Path(p) => {
                        // let path = p.path;
                        if let std::option::Option::Some(field_ident) = field.ident {
                            field_name = field_ident.to_string();
                        }
                        let segments = p.path.segments;
                        for segment in segments {
                            let field_type = segment.ident;
                            field_composition.push_str(field_type.to_string().as_str());
                            match segment.arguments {
                                // Catch the 'String' in std::option::Option<String>
                                PathArguments::AngleBracketed(generic_arguments) => {
                                    // this will be repeated, but for the future
                                    // please extract it out to a function
                                    // but we just want to make this work now, so it should be ok
                                    // Technically it wont work if there are more arguments but we
                                    for arg in generic_arguments.args {
                                        match arg {
                                            GenericArgument::Type(ty) => match ty {
                                                Type::Path(p1) => {
                                                    for seg in p1.path.segments {
                                                        let field_type_bracket = seg.ident;
                                                        field_composition.push_str("<");
                                                        field_composition.push_str(
                                                            field_type_bracket.to_string().as_str(),
                                                        );
                                                        field_composition.push_str(">");
                                                        //  I will assume that `PathArguments::None`, otherwise
                                                        // this might end up going endlessly
                                                        // this will do fine for now
                                                    }
                                                }
                                                _ => (),
                                            },
                                            _ => (),
                                        }
                                    }
                                }
                                // for now i dont need to handle these type of arguments
                                PathArguments::Parenthesized(_) => (),
                                // None indicates that its something like `String`, no args
                                PathArguments::None => (),
                            }
                            // if segment.ident.to_string().contains("Option") {
                            //     println!("option")
                            // }
                        }
                    }
                    _ => (),
                }

                // let type_tokens: TokenStream = parse_str(&field_composition).unwrap_or_else(|_| {
                //     panic!("Failed to parse type string: {}", &field_composition)
                // });
                let type_tokens = parse_str(&field_composition).unwrap();
                let group = Group::new(Delimiter::None, type_tokens);
                struct_fields.push(StructField {
                    name: field_name,
                    ty: group,
                    attr: attr_literal,
                });
            }
        }
        _ => (),
    }

    // Current Hack: If a type signature is Option<T>, then we return whatever
    // they sent.
    // Else, we unwrap it to give `<T>` back.
    //dbg!(&struct_fields);
    // find current_dir type
    //dbg!(&struct_fields);
    let build_method_iter = struct_fields.iter().map(|field| {
        let expanded_name = Ident::new(field.name.clone().as_str(), Span::call_site());
        let expanded_type = field.ty.clone();
        if field.ty.to_string().contains("Option") {
            quote! {
                #expanded_name: self.#expanded_name.clone(),
            }
        } else {
            quote! {
                //#expanded_name: self.#expanded_name.clone().unwrap_or(String::from("current_dir not set")),
                #expanded_name: self.#expanded_name.clone().unwrap(),
            }
        }
    });
    let build_method = quote! { #name {#(#build_method_iter)*}};
    let fields_with_attrs = struct_fields
        .iter()
        .filter(|field| {
            // make sure you dont generate a builder method if the attr has the same name as the
            // field
            field.attr.is_some() && field.attr.clone().unwrap() != field.name.clone().to_string()
        })
        .collect::<Vec<&StructField>>();

    let each_builder_method = struct_fields.iter().map(|field| {
        let expanded_name = Ident::new(field.name.clone().as_str(), Span::call_site());
        let expanded_type = field.ty.clone();
        if expanded_type.to_string().contains("Option") {
            quote! {
                fn #expanded_name(&mut self, #expanded_name: String) -> &mut #builder_name {
                    self.#expanded_name = std::option::Option::Some(#expanded_name);
                    self
                }
            }
        } else {
            quote! {
                fn #expanded_name(&mut self, #expanded_name: #expanded_type) -> &mut #builder_name {
                    self.#expanded_name = std::option::Option::Some(#expanded_name);
                    self
                }
            }
        }
    });
    let gen_each_builder_methods = quote! {
        #(#each_builder_method)*
    };
    let each_builder_att_method = fields_with_attrs.iter().map(|field| {
        let field_name_ident = Ident::new(field.name.as_str(), Span::call_site());
        let field_attr_ident = Ident::new(field.attr.clone().unwrap().as_str(), Span::call_site());
        quote! {
            fn #field_attr_ident(&mut self, #field_attr_ident: String) -> &mut #builder_name {
                self.#field_attr_ident.push(#field_attr_ident);
                self.#field_name_ident = std::option::Option::Some(self.#field_attr_ident.clone());
                self
            }
        }
    });
    let gen_builder_attr_methods = quote! {
        #(#each_builder_att_method)*
    };
    let builder_struct_fields = struct_fields.iter().map(|field| {
        let expanded_name = Ident::new(field.name.clone().as_str(), Span::call_site());
        let expanded_type = field.ty.clone();
        if expanded_type.to_string().contains("Option") {
            // this is a hack for current_dir since it always should have the signature
            // option<string> even if its type "should" be option<option<string>>
            quote! {
                #expanded_name: std::option::Option<String>,
            }
        } else {
            quote! {
                #expanded_name: std::option::Option<#expanded_type>,
            }
        }
    });
    let builder_struct_fields_initial = struct_fields.iter().map(|field| {
        let expanded_name = Ident::new(field.name.clone().as_str(), Span::call_site());
        let expanded_type = field.ty.clone();
        quote! {
            #expanded_name: std::option::Option::None,
        }
    });
    let builder_struct_atts = struct_fields.iter().map(|field| {
        if field.attr.is_some() && field.attr.clone().unwrap() != field.name.clone().to_string() {
            // For attributes, we assume that its type is always a `Vec<String>`
            let expanded_name = Ident::new(field.attr.clone().unwrap().as_str(), Span::call_site());
            quote! {
                #expanded_name: Vec<String>,
            }
        } else {
            quote! {}
        }
    });

    let builder_struct_atts_initial = struct_fields.iter().map(|field| {
        //field.attr.clone().unwrap() != field.name.clone().to_string()
        if field.attr.is_some() && field.attr.clone().unwrap() != field.name.clone().to_string() {
            // For attributes, we assume that its type is always a `Vec<String>`
            let expanded_name = Ident::new(field.attr.clone().unwrap().as_str(), Span::call_site());
            quote! {
                #expanded_name: Vec::new(),
            }
        } else {
            quote! {}
        }
    });
    let builder_struct =
        quote! {struct #builder_name {#(#builder_struct_fields)* #(#builder_struct_atts)*}};
    let expanded_name = Ident::new(struct_fields[0].name.clone().as_str(), Span::call_site());
    let expanded_type = struct_fields[0].ty.clone();
    let expanded = quote! {
        use std::error::Error;
        #builder_struct
        impl #name {
            #visibility fn builder() -> #builder_name {
                #builder_name {
                    #(#builder_struct_fields_initial)*
                    #(#builder_struct_atts_initial)*
                }
            }
        }

        impl #builder_name {
            #gen_each_builder_methods

            #gen_builder_attr_methods

            //}
            fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn Error>> {
                let cmd = #build_method;
                Ok(cmd)
            }

        }
    };
    proc_macro::TokenStream::from(expanded)
}
