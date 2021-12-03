use std::{cmp::Ordering, collections::HashMap};

use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;

use crate::{
    id,
    naming::{get_operation_id, get_parameter_id, get_request_type_id, get_response_type_id},
    openapi::{OperationObject, check_path_parameters},
    OpenAPIExpansionContext, APISERVICE_CALL_METHOD_ID, APISERVICE_CALL_PATH_ID,
};

const SEP: char = '/';
const TEMPLATE_START: char = '{';
const TEMPLATE_END: char = '}';

/// Deal:
/// - `let ctrl = (self.make_controller)();` creates a controller
/// - `(self.not_found)()(request)` invokes the not_found handler
///
/// Returns: match block source
pub(crate) fn expand_route_matcher<'a>(
    ctx: &mut OpenAPIExpansionContext,
    routes: impl IntoIterator<Item = &'a Route<'a>>,
) -> TokenStream2 {
    let mut method_map: HashMap<hyper::Method, PathNode> = Default::default();

    for route in routes {
        let node = method_map.entry(route.method.clone()).or_default();
        insert_into(node, &route.path, route);
    }

    let match_method_arm_sources = method_map.into_iter().map(|(method, node)| {
        let method_match_arm_source = expand_node_matcher(ctx, &node, &Vec::new());

        let id = id(method);
        quote! {
            &hyper::Method:: #id => {
                #method_match_arm_source
            }
        }
    });

    let path_id = id(APISERVICE_CALL_PATH_ID);
    let method_id = id(APISERVICE_CALL_METHOD_ID);

    quote! {
        {
            let mut tokens = #path_id.split( #SEP ).filter(|t| t.len() > 0);
            match #method_id {
                #(#match_method_arm_sources) *

                #[allow(unreachable_patterns)]
                _ => {
                    (self.not_found)(request).boxed()
                }
            }
        }
    }
}

fn expand_node_matcher(
    ctx: &mut OpenAPIExpansionContext,
    node: &PathNode,
    parameters: &Vec<String>,
) -> TokenStream2 {
    let mut keys: Vec<&PathNodeType> = node.children.keys().collect();
    keys.sort();

    let mut match_token_arm_sources = Vec::with_capacity(keys.len());
    for key in keys {
        let node = node.children.get(key).expect("iterating over keys");

        match key {
            &PathNodeType::Template => {
                let parameter_name = node
                    .parameter_name
                    .as_ref()
                    .expect("parameter items should always have parameter name set");
                let parameter_id = id(parameter_name);

                let mut parameters = parameters.clone();
                parameters.push(parameter_name.clone());
                let arm_source = expand_node_matcher(ctx, node, &parameters);

                match_token_arm_sources.push(quote! {
                    Some(#parameter_id) => {
                        #arm_source
                    }
                });
            }
            &PathNodeType::Path(ref path) => {
                let arm_source = expand_node_matcher(ctx, node, parameters);
                match_token_arm_sources.push(quote! {
                    Some(#path) => {
                        #arm_source
                    }
                });
            }
        }
    }

    let none_arm_source = match node.route {
        Some(route) => {
            // validate that path parameters
            check_path_parameters(parameters, route.operation, &route.method, &route.path).expect("Path parameters have to be valid");

            // construct request types
            let type_id = {
                let type_id = route.get_request_type_id();

                let fields = parameters.iter().map(get_parameter_id).map(|p| {
                    quote! {
                        pub #p: String
                    }
                });

                ctx.user_mod_sources.push(quote! {
                    pub struct #type_id {
                        #(#fields), *
                    }
                });

                type_id
            };

            let initializer = parameters.iter().map(|p| {
                let p = syn::Ident::new(super::snake_case(p).as_str(), Span::call_site());
                quote! {
                    #p: #p.to_owned()
                }
            });

            let operation_id = route.get_operation_id();
            let response_type_id = route.get_response_type_id();

            Some(quote! {
                None => {
                    let controller = (self.make_controller)();
                    controller. #operation_id ( super:: #type_id {
                        #(#initializer), *
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
            #(#match_token_arm_sources) *

            #[allow(unreachable_patterns)]
            _ => {
                (self.not_found)(request).boxed()
            }
        }
    }
}

fn insert_into<'a>(
    mut node: &mut PathNode<'a>,
    outstanding_path: impl AsRef<str>,
    route: &'a Route,
) {
    for token in outstanding_path.as_ref().split(SEP).skip(1) {
        let (key, parameter_name) = get_node_type(token);

        let child = node.children.entry(key).or_insert_with(|| PathNode {
            parameter_name: parameter_name.clone(),
            ..Default::default()
        });

        assert_eq!(child.parameter_name, parameter_name);
        node = child;
    }

    node.route = Some(route);
}

pub struct Route<'a> {
    pub method: hyper::Method,
    pub path: String,
    pub operation: &'a OperationObject,
}

impl<'a> Route<'a> {
    pub fn get_operation_id(&self) -> syn::Ident {
        get_operation_id(self.operation, &self.method, &self.path)
    }

    pub fn get_request_type_id(&self) -> syn::Ident {
        get_request_type_id(self.operation, &self.method, &self.path)
    }

    pub fn get_response_type_id(&self) -> syn::Ident {
        get_response_type_id(self.operation, &self.method, &self.path)
    }
}

struct PathNode<'a> {
    parameter_name: Option<String>,
    route: Option<&'a Route<'a>>,
    children: HashMap<PathNodeType, PathNode<'a>>,
}

impl<'a> Default for PathNode<'a> {
    fn default() -> Self {
        PathNode {
            parameter_name: None,
            route: None,
            children: Default::default(),
        }
    }
}

/// returns (type, parameter name)
fn get_node_type(path_token: impl AsRef<str>) -> (PathNodeType, Option<String>) {
    let path_token = path_token.as_ref();
    if path_token.starts_with(TEMPLATE_START) && path_token.ends_with(TEMPLATE_END) {
        (
            PathNodeType::Template,
            Some(path_token[1..path_token.len() - 1].to_owned()),
        )
    } else {
        (PathNodeType::Path(path_token.to_string()), None)
    }
}

#[derive(Hash, PartialEq, Eq, Clone, Ord)]
enum PathNodeType {
    Template,
    Path(String),
}

impl PartialOrd for PathNodeType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(match (self, other) {
            (PathNodeType::Template, PathNodeType::Template) => Ordering::Equal,
            (PathNodeType::Path(p1), PathNodeType::Path(p2)) => p1.cmp(p2),
            (PathNodeType::Template, _) => Ordering::Less,
            (_, PathNodeType::Template) => Ordering::Greater,
        })
    }
}
