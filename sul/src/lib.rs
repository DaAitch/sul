#![feature(proc_macro_span)]

use openapi::{OperationObject, ResponseObject, SchemaObject};
use path::Route;
use proc_macro::{Span, TokenStream};
use proc_macro2::{Span as Span2, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use std::ops::Deref;
use syn::{parse_macro_input, AttributeArgs, ItemStruct, Lit, NestedMeta, TypePath};

mod openapi;
mod path;

// return quote_spanned! {
//     yaml_filename.span() => compile_error!("error here");
// }.into();

#[proc_macro_attribute]
pub fn openapi(attr: TokenStream, item: TokenStream) -> TokenStream {
    let document = {
        let args = parse_macro_input!(attr as AttributeArgs);
        let yaml_filename = if let Some(NestedMeta::Lit(Lit::Str(s))) = args.iter().next() {
            s.value()
        } else {
            panic!("expecting string to file: e.g. \"open-api-file.yaml\"")
        };

        let path = Span::call_site().source_file().path();
        let parent = path.parent().unwrap();
        let yaml_file_path = parent.join(yaml_filename);

        openapi::read_openapi(yaml_file_path).unwrap()
    };

    let mut routes = Vec::new();

    let mut method_sources: Vec<TokenStream2> = Vec::new();
    for (path, path_item) in document.paths {
        if let Some(operation) = &path_item.get {
            construct_routes(
                hyper::Method::GET,
                path.clone(),
                operation,
                &mut method_sources,
                &mut routes,
            );
        }

        if let Some(operation) = &path_item.put {
            construct_routes(
                hyper::Method::PUT,
                path.clone(),
                operation,
                &mut method_sources,
                &mut routes,
            );
        }

        if let Some(operation) = &path_item.post {
            construct_routes(
                hyper::Method::POST,
                path.clone(),
                operation,
                &mut method_sources,
                &mut routes,
            );
        }

        if let Some(operation) = &path_item.delete {
            construct_routes(
                hyper::Method::DELETE,
                path.clone(),
                operation,
                &mut method_sources,
                &mut routes,
            );
        }

        if let Some(operation) = &path_item.options {
            construct_routes(
                hyper::Method::OPTIONS,
                path.clone(),
                operation,
                &mut method_sources,
                &mut routes,
            );
        }

        if let Some(operation) = &path_item.head {
            construct_routes(
                hyper::Method::HEAD,
                path.clone(),
                operation,
                &mut method_sources,
                &mut routes,
            );
        }

        if let Some(operation) = &path_item.patch {
            construct_routes(
                hyper::Method::PATCH,
                path.clone(),
                operation,
                &mut method_sources,
                &mut routes,
            );
        }

        if let Some(operation) = &path_item.trace {
            construct_routes(
                hyper::Method::TRACE,
                path.clone(),
                operation,
                &mut method_sources,
                &mut routes,
            );
        }
    }

    let path_id = id("path");
    let method_id = id("method");
    let (route_matcher_source, request_types_source) =
        path::expand_route_matcher(&routes, path_id, method_id);

    let controller_struct = parse_macro_input!(item as ItemStruct);
    let controller_id = &controller_struct.ident;

    let api_service_source = expand_service_mod_source(controller_id, route_matcher_source);

    quote! {
        #(#method_sources) *

        #request_types_source

        #api_service_source

        #controller_struct
    }
    .into()
}

fn construct_routes(
    method: hyper::Method,
    path: String,
    operation: &OperationObject,
    method_sources: &mut Vec<TokenStream2>,
    routes: &mut Vec<Route>,
) {
    let method_lc = method.as_str().to_lowercase();
    let result = expand_response_source(&path, &method_lc, operation);
    method_sources.push(result.struct_source);

    let path_sc = sc(&path);
    let operation_name_id = format_ident!("{}_{}", method_lc, path_sc);

    routes.push(path::Route {
        method,
        operation_name_id,
        path,
        response_type_id: result.struct_id,
    });
}

struct ResponseSourceResult {
    struct_id: syn::Ident,
    struct_source: TokenStream2,
}

/// Expands at the http method, e.g. `paths./users.get` for the name "GetUsers"
fn expand_response_source(
    path: impl AsRef<str>,
    method_lc: impl AsRef<str>,
    oa_method_spec: &OperationObject,
) -> ResponseSourceResult {
    let method_ucc = ucc(method_lc.as_ref());
    let path_ucc = ucc(path.as_ref());
    let name_ucc = method_ucc + &path_ucc;

    let mut data_struct_sources: Vec<TokenStream2> = Vec::new();
    let mut response_struct_impl_sources: Vec<TokenStream2> = Vec::new();

    let response_struct_name = name_ucc.clone() + "Response";
    let struct_id = id(&response_struct_name);

    for (oa_status_code, oa_response_spec) in &oa_method_spec.responses {
        let status_name_lc = get_status_name_lc(oa_status_code);
        let status_name_ucc = ucc(status_name_lc);

        let method_path_status_name = name_ucc.clone().to_owned() + &status_name_ucc;
        let data_type = expand_schema_source(
            &method_path_status_name,
            &oa_response_spec.content.application_json.schema,
            &mut data_struct_sources,
        );

        let status_code_name_id = id(status_name_lc);

        let fn_doc_source = expand_method_doc(&path, &method_lc, oa_status_code, oa_response_spec);

        response_struct_impl_sources.push(quote! {
            #fn_doc_source
            pub fn #status_code_name_id(data: &#data_type) -> #struct_id {
                let content = serde_json::to_string(data).unwrap();
                let body = hyper::Body::from(content);

                #struct_id {
                    response: hyper::Response::builder()
                        .body(body)
                        .unwrap()
                }
            }
        })
    }

    let response_struct_doc_source = expand_response_doc(path, method_lc, &oa_method_spec);

    let struct_source = quote! {
        #(#data_struct_sources) *

        #response_struct_doc_source
        pub struct #struct_id {
            #[doc(hidden)]
            response: hyper::Response<hyper::Body>,
        }

        #[doc(hidden)]
        impl Into<hyper::Response<hyper::Body>> for #struct_id {
            fn into(self) -> hyper::Response<hyper::Body> {
                self.response
            }
        }

        impl #struct_id {
            #(#response_struct_impl_sources) *
        }
    };

    ResponseSourceResult {
        struct_id,
        struct_source,
    }
}

/// Expand schema source, returning the result type.
fn expand_schema_source(
    name: impl AsRef<str>,
    oa_schema: &SchemaObject,
    data_struct_sources: &mut Vec<TokenStream2>,
) -> TypePath {
    match oa_schema {
        SchemaObject::Array(oa_array_items_schema) => {
            let sub_type = expand_schema_source(name, oa_array_items_schema, data_struct_sources);
            let array_type_string = format!(
                "std::vec::Vec<{}>",
                sub_type.path.get_ident().unwrap().to_string() // TODO: Vec<Vec<String>> will fail?
            );

            syn::parse_str(array_type_string.as_str()).unwrap()
        }
        SchemaObject::Object(oa_object_props_schema) => {
            let mut struct_props_sources: Vec<TokenStream2> = Vec::new();
            for (oa_prop_name_cc, oa_schema) in oa_object_props_schema.deref() {
                let oa_prop_name_ucc = ucc(oa_prop_name_cc);
                let name = name.as_ref().to_owned() + &oa_prop_name_ucc;

                let underlying_prop_type =
                    expand_schema_source(&name, oa_schema, data_struct_sources);
                let prop_id = id(&sc(&oa_prop_name_cc));

                struct_props_sources.push(quote! {
                    #[serde(rename = #oa_prop_name_cc)]
                    #prop_id: #underlying_prop_type
                });
            }

            let data_struct_id = id(&name);

            let data_struct_source = quote! {
                #[derive(serde::Serialize, Debug)]
                pub struct #data_struct_id {
                    #(#struct_props_sources), *
                }
            };
            data_struct_sources.push(data_struct_source);
            syn::parse_str(name.as_ref()).unwrap()
        }
        SchemaObject::String => syn::parse_str("String").unwrap(),
    }
}

// OpenAPI helper

fn get_status_name_lc(status_code: impl AsRef<str>) -> &'static str {
    match status_code.as_ref() {
        "200" => "ok",
        "401" => "unauthorized",
        _ => "unknown",
    }
}

fn expand_doc(text: impl AsRef<str>) -> TokenStream2 {
    let text = text.as_ref();

    quote! {
        #[doc = #text]
    }
}

fn expand_response_doc(
    path: impl AsRef<str>,
    method_lc: impl AsRef<str>,
    spec: &OperationObject,
) -> TokenStream2 {
    let path = path.as_ref();
    let method_lc = method_lc.as_ref();
    let method_auc = auc(method_lc);

    let doc = match &spec.description {
        Some(desc) => format!("`{} {}`\n\n{}", method_auc, path, desc),
        None => format!("`{} {}`", method_auc, path),
    };

    expand_doc(doc)
}

fn expand_method_doc(
    path: impl AsRef<str>,
    method_lc: impl AsRef<str>,
    status_code: impl AsRef<str>,
    spec: &ResponseObject,
) -> TokenStream2 {
    let path = path.as_ref();
    let method_lc = method_lc.as_ref();
    let status_code = status_code.as_ref();

    let method_auc = auc(method_lc);

    expand_doc(format!(
        "`{} {} -> {}`\n\n{}",
        method_auc, path, status_code, &spec.description
    ))
}

fn expand_service_mod_source(
    controller_id: &syn::Ident,
    route_matcher_source: TokenStream2,
) -> TokenStream2 {
    quote! {
        #[doc = "tower-service implementations"]
        mod service {
            use hyper::{Request, Body, service::Service, Method, Response};
            use futures::{future::BoxFuture, Future, FutureExt};
            use std::{
                convert::Infallible,
                task::{Context, Poll},
                result::Result,
                pin::Pin,
            };

            pub struct ApiService<F, N> {
                #[doc(hidden)]
                make_controller: F,
                #[doc(hidden)]
                not_found: N,
            }

            impl<F, N> ApiService<F, N> {
                pub fn new<R>(make_controller: F, not_found: N) -> ApiService<F, N>
                where
                    F: Fn() -> super::#controller_id,
                    N: Fn(Request<Body>) -> R,
                    R: Future<Output = Response<Body>> + 'static + Send,
                {
                    ApiService {
                        make_controller,
                        not_found,
                    }
                }
            }

            impl<F, N, R> Service<Request<Body>> for ApiService<F, N>
            where
                F: Fn() -> super::#controller_id,
                N: Fn(Request<Body>) -> R,
                R: Future<Output = Response<Body>> + 'static + Send,
            {
                type Response = Response<Body>;
                type Error = Infallible;
                type Future = ControllerFuture;

                fn poll_ready(&mut self, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
                    Poll::Ready(Ok(()))
                }

                fn call(&mut self, request: Request<Body>) -> Self::Future {
                    let method = request.method();
                    let path = request.uri().path();

                    let action = {
                        #route_matcher_source
                    };

                    ControllerFuture { action }
                }
            }

            #[doc(hidden)]
            pub struct ControllerFuture {
                action: BoxFuture<'static, Response<Body>>,
            }

            #[doc(hidden)]
            impl futures::Future for ControllerFuture {
                type Output = Result<Response<Body>, Infallible>;

                fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                    match self.action.as_mut().poll(cx) {
                        Poll::Pending => Poll::Pending,
                        Poll::Ready(result) => Poll::Ready(Ok(result)),
                    }
                }
            }
        }
    }
}

// basic helper

fn id(id: impl AsRef<str>) -> syn::Ident {
    syn::Ident::new(id.as_ref(), Span2::call_site())
}

/// snake-case
fn sc(expr: impl AsRef<str>) -> String {
    let expr = expr.as_ref();
    let mut s = String::with_capacity(2 * expr.len());

    let mut separator_allowed = false;

    for ch in expr.chars() {
        match ch {
            'A'..='Z' => {
                separator_allowed = true;
                s.push('_');
                s.push(ch.to_ascii_lowercase());
            }
            'a'..='z' => {
                separator_allowed = true;
                s.push(ch);
            }
            _ => {
                if separator_allowed {
                    separator_allowed = false;
                    s.push('_');
                }
            }
        }
    }

    s
}

/// upper camel-case
fn ucc(expr: impl AsRef<str>) -> String {
    let expr = expr.as_ref();
    let mut s = String::with_capacity(2 * expr.len());

    let mut next_upper = true;

    for ch in expr.chars() {
        match ch {
            'A'..='Z' => {
                next_upper = false;
                s.push(ch);
            }
            'a'..='z' => {
                if next_upper {
                    next_upper = false;
                    s.push(ch.to_ascii_uppercase());
                } else {
                    s.push(ch);
                }
            }
            _ => {
                next_upper = true;
            }
        }
    }

    s
}

/// all upper case
fn auc(expr: impl AsRef<str>) -> String {
    expr.as_ref()
        .chars()
        .map(|ch| ch.to_ascii_uppercase())
        .collect()
}

// tests

#[cfg(test)]
mod tests {
    use crate::{sc, ucc};

    #[test]
    fn some_tests() {
        assert_eq!(sc("errorCode"), "error_code".to_owned());
        assert_eq!(sc("/users"), "users".to_owned());
        assert_eq!(sc("/some/thing"), "some_thing".to_owned());
        assert_eq!(ucc("/users"), "Users".to_owned());
    }
}
