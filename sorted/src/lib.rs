use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::visit_mut::VisitMut;
use syn::{Expr, Ident, Pat, Stmt};

struct MatchVisitor;

impl VisitMut for MatchVisitor {
    fn visit_expr_match_mut(&mut self, node: &mut syn::ExprMatch) {
        // do it only if it has a `sorted` attribute, not always
        // these can be modified later.
        node.attrs.clear();
    }
}

fn check_variants_sorted(idents: &Vec<Ident>) -> std::result::Result<(), syn::Error> {
    let mut sorted_idents = idents.clone();
    sorted_idents.sort();
    let mut res = idents
        .iter()
        .zip(sorted_idents.iter())
        .filter(|(unsorted, sorted)| !unsorted.eq(sorted));
    // For now we only care about the first one where it isnt equal
    // If its empty, it means its ordered correctly
    match res.next() {
        None => Ok(()),
        Some((unsorted_item, sorted_item)) => {
            //panic!("hi");
            Err(syn::Error::new(
                sorted_item.span(),
                format!(
                    "{} should sort before {}",
                    sorted_item.to_string(),
                    unsorted_item.to_string()
                ),
            ))
        }
    }?;
    Ok(())
}

fn sort_enum(input: &TokenStream) -> std::result::Result<TokenStream2, syn::Error> {
    let item = syn::parse::<syn::Item>(input.to_owned())?;
    // so the item has a thing you can check called brace_token,
    // brace_token has arms, and then the arm has the tuple sruct to be checked
    match item {
        syn::Item::Enum(item_enum) => {
            let variants = item_enum.variants.clone();
            let enum_ = variants
                .into_iter()
                .map(|variant| variant.ident)
                .collect::<Vec<Ident>>();
            check_variants_sorted(&enum_)?;
            Ok(item_enum.to_token_stream())
        }
        _ => Err(syn::Error::new(
            Span::call_site(),
            "expected enum or match expression",
        )),
    }
}

fn get_match_arm_idents(input: &TokenStream) -> std::result::Result<Vec<Ident>, syn::Error> {
    let fn_item = syn::parse::<syn::ItemFn>(input.to_owned())?;
    let stmts = fn_item.clone().block.stmts;
    let match_idents_big = stmts
        .into_iter()
        .filter_map(|stmt| {
            if let Stmt::Expr(expr, _) = stmt {
                if let Expr::Match(expr_match) = expr {
                    let arms = expr_match.arms;
                    let idents1_big = arms
                        .iter()
                        .map(|arm| {
                            if let Pat::TupleStruct(tuple_struct) = arm.clone().pat {
                                let segments = tuple_struct.path.segments;
                                let idents = segments
                                    .iter()
                                    .map(|segment| segment.clone().ident)
                                    .collect::<Vec<Ident>>();
                                idents
                            } else {
                                vec![]
                            }
                        })
                        .collect::<Vec<Vec<Ident>>>();
                    // this probably can be done inside all the filter maps code, but want to make it work first.
                    let idents1 = idents1_big.into_iter().flatten().collect::<Vec<Ident>>();
                    Some(idents1)
                } else {
                    None
                }
            } else {
                None
            }
        })
        .collect::<Vec<Vec<Ident>>>();

    let match_idents = match_idents_big
        .into_iter()
        .flatten()
        .collect::<Vec<Ident>>();
    Ok(match_idents)
}

fn clear_match_atts(input: &TokenStream) -> std::result::Result<TokenStream2, syn::Error> {
    let mut fn_item = syn::parse::<syn::ItemFn>(input.to_owned())?;
    MatchVisitor.visit_item_fn_mut(&mut fn_item);
    Ok(fn_item.to_token_stream())
}
fn check_enum(input: &TokenStream) -> std::result::Result<TokenStream2, syn::Error> {
    let mut fn_item = syn::parse::<syn::ItemFn>(input.to_owned())?;
    MatchVisitor.visit_item_fn_mut(&mut fn_item);
    let arm_idents = get_match_arm_idents(&input)?;
    check_variants_sorted(&arm_idents)?;
    Ok(fn_item.to_token_stream())
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    if let Ok(fn_item) = clear_match_atts(&input) {
        let res = check_enum(&input);
        match res {
            Ok(input_token) => input_token.into(),
            Err(e) => {
                let err = e.to_compile_error();
                let original_item: proc_macro2::TokenStream = fn_item.into();
                quote! {
                    #original_item
                    #err
                }
                .into()
            }
        }
    } else {
        input.into()
    }
}

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    match sort_enum(&input) {
        Ok(stream) => stream.into(),
        Err(e) => {
            let err = e.to_compile_error();
            // this is needed as otherwise the macro does not see the input enum
            // and therefor brings all the `unused_imports` warnings
            // By providing the input enum, Rust compiler can see that its indeed used
            // and only output the relevant error
            let input_enum: proc_macro2::TokenStream = input.into();
            quote! {
                #input_enum
                #err
            }
            .into()
        }
    }
}
