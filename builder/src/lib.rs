#![allow(dead_code)]
#![allow(unused_imports)]
use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro2::{Delimiter, Group, Punct, Spacing, TokenTree};
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, parse_str, AttrStyle, Attribute, Data, Field, FieldsNamed, GenericArgument,
    Ident, Lit, LitStr, Meta, MetaList, Path, PathArguments, Type,
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
    attr: Option<String>,
}
fn extract_attr_str_literal(attrs: Vec<Attribute>) -> Option<String> {
    // I am looking for
    // Attr::Style::Outer
    // Meta::List(MetaList) with PathSegment ident == "builder"
    // Meta::List(MetaList) with TokenStream Ident == "each"
    // If those conditions, return the TokenStream Literal  symbol
    let maybe_attr = attrs
        .iter()
        .filter(|attr| attr.style == AttrStyle::Outer)
        .next();
    if let Some(attr) = maybe_attr {
        if let Meta::List(meta_list) = &attr.meta {
            // get Path segment ident builder
            // get token stream with ident each
            meta_list
                .path
                .segments
                .iter()
                .find(|seg| seg.ident == "builder")
                .expect("No builder found");
            // Now that we know we have a Pathsegment builder, we proceed to find the tokens
            //let token = meta_list.tokens.iter().find(|token| );
            meta_list
                .tokens
                .clone()
                .into_iter()
                .find(|token| {
                    if let TokenTree::Ident(ident) = token {
                        ident.to_string() == "each"
                    } else {
                        false
                    }
                })
                .expect("No each found");
            // We have secured that there is a `builder` with `each` in the attribute
            // Now we need to find the value of the `each` attribute as a literal
            let builder_literal = meta_list
                .tokens
                .clone()
                .into_iter()
                .filter_map(|token| {
                    if let TokenTree::Literal(lit) = token {
                        let lit_str = syn::parse_str::<LitStr>(lit.to_string().as_str()).unwrap();
                        Some(lit_str.value())
                    } else {
                        None
                    }
                })
                .collect::<Vec<String>>();
            assert_eq!(builder_literal.len(), 1);
            Some(builder_literal.first().unwrap().clone())
        } else {
            None
        }
    } else {
        None
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
                let attr_literal = extract_attr_str_literal(attrs);
                // Iteration for EACH field
                match field.ty {
                    Type::Path(p) => {
                        // let path = p.path;
                        if let Some(field_ident) = field.ident {
                            field_name = field_ident.to_string();
                        }
                        let segments = p.path.segments;
                        for segment in segments {
                            let field_type = segment.ident;
                            field_composition.push_str(field_type.to_string().as_str());
                            match segment.arguments {
                                // Catch the 'String' in Option<String>
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
    let current_dir_handling = if struct_fields[3].ty.clone().to_string().contains("Option") {
        quote! {current_dir: self.current_dir.clone(),}
    } else {
        quote! {current_dir: self.current_dir.clone().unwrap_or(String::from("current_dir not set")),}
    };
    // Generate following code:
    //
    let fields_with_attrs = struct_fields
        .iter()
        .filter(|field| {
            // make sure you dont generate a builder method if the attr has the same name as the
            // field
            field.attr.is_some() && field.attr.clone().unwrap() != field.name.clone().to_string()
        })
        .collect::<Vec<&StructField>>();

    //for field in fields_with_attrs {
    // you can assume the element will always be a String(for now)

    let each_builder_methods = fields_with_attrs.iter().map(|field| {
        let field_name_ident = Ident::new(field.name.as_str(), Span::call_site());
        let field_attr_ident = Ident::new(field.attr.clone().unwrap().as_str(), Span::call_site());
        quote! {
            fn #field_attr_ident(&mut self, #field_attr_ident: String) -> &mut #builder_name {
                self.#field_attr_ident.push(#field_attr_ident);
                self.#field_name_ident = Some(self.#field_attr_ident.clone());
                self
            }
        }
    });
    let gen_builder_methods = quote! {
        #(#each_builder_methods)*
    };
    let expanded = quote! {
        use std::error::Error;
        #visibility struct #builder_name {
            executable: Option<String>,
            args: Option<Vec<String>>,
            arg: Vec<String>,
            env: Option<Vec<String>>,
            current_dir: Option<String>,
        }
        impl #name {
            #visibility fn builder() -> #builder_name {
                #builder_name {
                    executable: None,
                    args: None,
                    arg: Vec::new(),
                    env: None,
                    current_dir: None,
                }
            }
        }

        impl #builder_name {
            fn executable(&mut self, executable: String) -> &mut #builder_name {
                self.executable = Some(executable);
                self
            }

            #gen_builder_methods
            fn args(&mut self, args: Vec<String>) -> &mut #builder_name {
                self.args = Some(args);
                self
            }
            fn env(&mut self, env: Vec<String>) -> &mut #builder_name {
                self.env = Some(env);
                self
            }
            fn current_dir(&mut self, current_dir: String) -> &mut #builder_name {
                self.current_dir = Some(current_dir);
                self
            }
            fn build(&mut self) -> Result<#name, Box<dyn Error>> {
                self.executable.as_ref().ok_or("executable not set")?;
                self.args.as_ref().ok_or("args not set")?;
                self.env.as_ref().ok_or("env not set")?;
                let cmd = #name {executable: self.executable.clone().unwrap(),
                    args: self.args.clone().unwrap(),
                    env: self.env.clone().unwrap(),
                    // current_dir: self.current_dir.clone().unwrap()};
                    // current_dir: self.current_dir.clone()};
                    #current_dir_handling };
                Ok(cmd)
            }

        }
    };
    proc_macro::TokenStream::from(expanded)
}
