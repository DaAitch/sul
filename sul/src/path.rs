use std::{cmp::Ordering, collections::HashMap};

use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;

const SEP: char = '/';
const TEMPLATE_START: char = '{';
const TEMPLATE_END: char = '}';

/// Deal:
/// - `let ctrl = (self.make_controller)();` creates a controller
/// - `(self.not_found)()(request)` invokes the not_found handler
///
/// Returns: (match block, request types)
pub fn expand_route_matcher<'a>(
    routes: impl IntoIterator<Item = &'a Route>,
    path_id: syn::Ident,
    method_id: syn::Ident,
) -> (TokenStream2, TokenStream2) {
    let mut method_map: HashMap<hyper::Method, PathItem> = Default::default();

    for route in routes {
        let path_item = method_map.entry(route.method.clone()).or_default();
        insert_into(path_item, &route.path, route);
    }

    let mut request_type_sources = Vec::new();

    let method_arms: Vec<TokenStream2> = method_map
        .into_iter()
        .map(|(method, b)| {
            let id = syn::Ident::new(method.as_str(), Span::call_site());

            let path_token_match =
                next_token_match_source(&b, &Vec::new(), &mut request_type_sources);

            quote! {
                &hyper::Method:: #id => {
                    #path_token_match
                }
            }
        })
        .collect();

    (
        quote! {
            {
                let mut tokens = #path_id.split( #SEP ).filter(|t| t.len() > 0);
                match #method_id {
                    #(#method_arms) *

                    #[allow(unreachable_patterns)]
                    _ => {
                        (self.not_found)(request).boxed()
                    }
                }
            }
        },
        quote! {
            #(#request_type_sources) *
        },
    )
}

fn key_prio(a: &&PathItemType, b: &&PathItemType) -> std::cmp::Ordering {
    match (a, b) {
        (PathItemType::Template, PathItemType::Template) => Ordering::Equal,
        (PathItemType::Path(p1), PathItemType::Path(p2)) => p1.cmp(p2),
        (PathItemType::Template, _) => Ordering::Less,
        (_, PathItemType::Template) => Ordering::Greater,
    }
}

fn next_token_match_source(
    path_item: &PathItem,
    parameters: &Vec<String>,
    request_type_sources: &mut Vec<TokenStream2>,
) -> TokenStream2 {
    let mut keys: Vec<&PathItemType> = path_item.sub_items.keys().collect();
    keys.sort_by(key_prio);

    let mut s = Vec::with_capacity(keys.len());
    for key in keys {
        let item = path_item.sub_items.get(key).expect("iterating over keys");

        match key {
            &PathItemType::Template => {
                let template_name = item
                    .parent_template_name
                    .as_ref()
                    .expect("template items should always have template name set");
                let id_source = syn::Ident::new(&template_name, Span::call_site());

                let mut parameters = parameters.clone();
                parameters.push(template_name.clone());
                let arm_source = next_token_match_source(item, &parameters, request_type_sources);

                s.push(quote! {
                    Some(#id_source) => {
                        #arm_source
                    }
                });
            }
            &PathItemType::Path(ref path) => {
                let arm_source = next_token_match_source(item, parameters, request_type_sources);
                s.push(quote! {
                    Some(#path) => {
                        #arm_source
                    }
                });
            }
        }
    }

    let none_arm_source = match path_item.route_end {
        Some(route_end) => {
            // TODO: other methods ...
            let method_ucc = super::ucc(route_end.method.to_string().to_lowercase());
            let request_type_name = format!("{}{}Request", method_ucc, super::ucc(&route_end.path));
            let request_type_name_id =
                syn::Ident::new(request_type_name.as_str(), Span::call_site());
            let request_type_field_sources: Vec<TokenStream2> = parameters
                .iter()
                .map(|p| {
                    let p = syn::Ident::new(super::sc(p).as_str(), Span::call_site());
                    quote! {
                        pub #p: String
                    }
                })
                .collect();

            request_type_sources.push(quote! {
                pub struct #request_type_name_id {
                    #(#request_type_field_sources), *
                }
            });

            let request_field_initializer_sources: Vec<TokenStream2> = parameters
                .iter()
                .map(|p| {
                    let p = syn::Ident::new(super::sc(p).as_str(), Span::call_site());
                    quote! {
                        #p: #p.to_owned()
                    }
                })
                .collect();

            let controller_fn_id = &route_end.operation_name_id;
            let response_type_id = &route_end.response_type_id;

            Some(quote! {
                None => {
                    let controller = (self.make_controller)();
                    controller. #controller_fn_id ( super:: #request_type_name_id {
                        #(#request_field_initializer_sources), *
                    })
                        // Explicit parameter type is needed
                        // to not allow any response type.
                        // `.map_into::<Response<Body>>()` would lever out
                        // that check.
                        .map(|response: super::#response_type_id| {
                            let response: Response<Body> = response.into();
                            response
                        }).boxed()
                }
            })
        }
        None => None,
    };

    quote! {
        match tokens.next() {
            #none_arm_source
            #(#s) *

            #[allow(unreachable_patterns)]
            _ => {
                (self.not_found)(request).boxed()
            }
        }
    }
}

fn insert_into<'a>(
    mut path_item: &mut PathItem<'a>,
    outstanding_path: impl AsRef<str>,
    route_end: &'a Route,
) {
    for token in outstanding_path.as_ref().split(SEP).skip(1) {
        let (key, template_name) = as_item_type(token);
        let parent_template_name = template_name.clone();

        let next_path_item = path_item
            .sub_items
            .entry(key)
            .or_insert_with(move || PathItem {
                parent_template_name,
                ..Default::default()
            });

        assert_eq!(next_path_item.parent_template_name, template_name);
        path_item = next_path_item;
    }

    path_item.route_end = Some(route_end);
}

pub struct Route {
    pub method: hyper::Method,
    pub path: String,
    pub operation_name_id: syn::Ident,
    pub response_type_id: syn::Ident,
}

struct PathItem<'a> {
    parent_template_name: Option<String>,
    route_end: Option<&'a Route>,
    sub_items: HashMap<PathItemType, PathItem<'a>>,
}

impl<'a> Default for PathItem<'a> {
    fn default() -> Self {
        PathItem {
            parent_template_name: None,
            route_end: None,
            sub_items: Default::default(),
        }
    }
}

/// returns (type, template name)
fn as_item_type(path_item: impl AsRef<str>) -> (PathItemType, Option<String>) {
    let path_item = path_item.as_ref();
    if path_item.starts_with(TEMPLATE_START) && path_item.ends_with(TEMPLATE_END) {
        (
            PathItemType::Template,
            Some(path_item[1..path_item.len() - 1].to_owned()),
        )
    } else {
        (PathItemType::Path(path_item.to_string()), None)
    }
}

#[derive(Hash, PartialEq, Eq, Clone)]
enum PathItemType {
    Template,
    Path(String),
}
