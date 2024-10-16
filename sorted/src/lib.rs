use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::visit_mut::VisitMut;
use syn::{Ident, Pat};

struct MatchVisitor {
    idents: Vec<Ident>,
}

impl MatchVisitor {
    fn new() -> Self {
        MatchVisitor { idents: Vec::new() }
    }
}

impl VisitMut for MatchVisitor {
    fn visit_expr_match_mut(&mut self, node: &mut syn::ExprMatch) {
        // do it only if it has a `sorted` attribute, not always
        // these can be modified later.
        //node.attrs.clear();
        for arm in &node.arms {
            if let Pat::TupleStruct(tuple_struct) = arm.clone().pat {
                let segments = tuple_struct.path.segments;
                for segment in segments {
                    self.idents.push(segment.clone().ident);
                }
            }
        }
        node.attrs.clear();
        syn::visit_mut::visit_expr_match_mut(self, node);
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
        Some((unsorted_item, sorted_item)) => Err(syn::Error::new(
            sorted_item.span(),
            format!(
                "{} should sort before {}",
                sorted_item.to_string(),
                unsorted_item.to_string()
            ),
        )),
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

fn check_enum(
    input: &TokenStream,
) -> std::result::Result<(TokenStream2, Option<syn::Error>), syn::Error> {
    let mut fn_item = syn::parse::<syn::ItemFn>(input.to_owned())?;
    let mut match_visitor = MatchVisitor::new();
    match_visitor.visit_item_fn_mut(&mut fn_item);
    let modified_fn_item = fn_item.to_token_stream();
    //let arm_idents = get_match_arm_idents(&input)?;
    // I need the modified versions in both so thats why we do it like this
    match check_variants_sorted(&match_visitor.idents) {
        Ok(_) => Ok((modified_fn_item, None)),
        Err(e) => Ok((modified_fn_item, Some(e))),
    }
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let res = check_enum(&input);
    match res {
        Ok((modified_fn_token, maybe_error)) => match maybe_error {
            None => modified_fn_token.into(),
            Some(e) => {
                let err = e.to_compile_error();
                quote! {
                    #modified_fn_token
                    #err
                }
                .into()
            }
        },
        // this one handles syn errors, which is super unlikely
        Err(e) => {
            let err = e.to_compile_error();
            let input_stream: proc_macro2::TokenStream = input.into();
            quote! {
                #input_stream
                #err
            }
            .into()
        }
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
