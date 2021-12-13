#![feature(proc_macro_span)]

use hyper::Method;
use naming::{
    get_prop_id, get_request_body_type_id, get_request_type_id, get_schema_array_subtype_id,
    get_schema_prop_type_id,
};
use openapi::{Document, OperationObject, ResponseObject, SchemaObject};
use path::Route;
use proc_macro::{Span, TokenStream};
use proc_macro2::{Span as Span2, TokenStream as TokenStream2};
use quote::quote;
use std::ops::Deref;
use syn::{parse_macro_input, AttributeArgs, ItemStruct, Lit, NestedMeta};

use crate::naming::{get_response_builder_param_type_id, get_response_type_id};

mod naming;
mod openapi;
mod path;

// return quote_spanned! {
//     yaml_filename.span() => compile_error!("error here");
// }.into();

const APISERVICE_CALL_PATH_ID: &str = "path";
const APISERVICE_CALL_METHOD_ID: &str = "method";

struct OpenAPIExpansionContext<'a> {
    document: &'a Document,
    user_mod_sources: Vec<TokenStream2>,
    service_mod_sources: Vec<TokenStream2>,
}

impl<'a> OpenAPIExpansionContext<'a> {
    fn new(document: &'a Document) -> Self {
        Self {
            document,
            user_mod_sources: Vec::new(),
            service_mod_sources: Vec::new(),
        }
    }
}

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

        openapi::Document::from_file(yaml_file_path).unwrap()
    };

    let mut ctx = OpenAPIExpansionContext::new(&document);
    let mut routes = Vec::new();

    for (path, item) in &document.paths {
        // TODO(daaitch): good error message
        let item = document
            .get_path_item_ref(item)
            .expect("cannot find path item $ref");

        for (method, operation) in [
            (Method::GET, &item.get),
            (Method::PUT, &item.put),
            (Method::POST, &item.post),
            (Method::DELETE, &item.delete),
            (Method::OPTIONS, &item.options),
            (Method::HEAD, &item.head),
            (Method::PATCH, &item.patch),
            (Method::TRACE, &item.trace),
        ] {
            if let Some(operation) = operation {
                let route = expand_route(&mut ctx, method, path.clone(), operation);

                routes.push(route);
            }
        }
    }

    let route_matcher_source = path::expand_route_matcher(&mut ctx, &routes);

    let controller_struct = parse_macro_input!(item as ItemStruct);
    ctx.user_mod_sources.push(quote! { #controller_struct });

    let controller_id = &controller_struct.ident;
    expand_service(&mut ctx, controller_id, route_matcher_source);

    {
        let user_mod_sources = &ctx.user_mod_sources;
        let service_mod_sources = &ctx.service_mod_sources;

        quote! {
            #(#user_mod_sources) *

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

                #(#service_mod_sources) *
            }
        }
        .into()
    }
}

fn expand_route<'a>(
    ctx: &mut OpenAPIExpansionContext,
    method: hyper::Method,
    path: String,
    operation: &'a OperationObject,
) -> Route<'a> {
    expand_response_source(ctx, &method, &path, operation);
    expand_request_body_source(ctx, &method, &path, operation);

    path::Route {
        method,
        operation,
        path,
    }
}

/// Expands at the http method, e.g. `paths./users.get` for the name "GetUsers"
fn expand_response_source(
    ctx: &mut OpenAPIExpansionContext,
    method: &Method,
    path: impl AsRef<str>,
    operation: &OperationObject,
) {
    let mut impl_sources: Vec<TokenStream2> = Vec::new();

    let struct_id = get_response_type_id(operation, method, &path);

    for (status_code, response) in &operation.responses {
        let type_id = get_response_builder_param_type_id(&operation, method, &path, status_code);
        let schema = ctx
            .document
            .get_schema_ref(&response.content.application_json.schema)
            .expect("TODO");
        expand_schema_source(ctx, &type_id, schema);

        let status_code_name_id = id(get_status_name_lc(status_code));

        let fn_doc_source = expand_method_doc(&method, &path, status_code, response);

        impl_sources.push(quote! {
            #fn_doc_source
            pub fn #status_code_name_id(data: &#type_id) -> #struct_id {
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

    let response_struct_doc_source = expand_response_doc(&method, path, &operation);

    ctx.user_mod_sources.push(quote! {
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
            #(#impl_sources) *
        }
    });
}

/// Expands at the http method, e.g. `paths./users.get` for the name "GetUsersRequestBody"
fn expand_request_body_source(
    ctx: &mut OpenAPIExpansionContext,
    method: &Method,
    path: impl AsRef<str>,
    operation: &OperationObject,
) {
    let request_type_id = get_request_type_id(operation, method, path);
    let request_body_type_id = get_request_body_type_id(&request_type_id);

    match &operation.request_body {
        Some(request_body) => {
            let schema = ctx
                .document
                .get_schema_ref(&request_body.content.application_json.schema)
                .expect("TODO");
            expand_schema_source(ctx, &request_body_type_id, schema);
        }
        None => {
            ctx.user_mod_sources.push(quote! {
                #[derive(serde::Serialize, serde::Deserialize, Debug)]
                pub struct #request_body_type_id {}
            });
        }
    }
}

/// Expand schema source, returning the result type.
fn expand_schema_source(
    ctx: &mut OpenAPIExpansionContext,
    type_id: &syn::Ident,
    schema: &SchemaObject,
) {
    match schema {
        SchemaObject::Array(items_schema) => {
            let sub_type_id = get_schema_array_subtype_id(type_id);
            expand_schema_source(ctx, &sub_type_id, items_schema);
            ctx.user_mod_sources.push(quote! {
                type #type_id = std::vec::Vec<#sub_type_id>;
            });
        }
        SchemaObject::Object(props_schema) => {
            let mut struct_prop_sources = Vec::new();
            for (prop_name, prop_schema) in props_schema.deref() {
                let name = get_schema_prop_type_id(type_id, prop_name);
                expand_schema_source(ctx, &name, prop_schema);
                let prop_id = get_prop_id(prop_name);

                struct_prop_sources.push(quote! {
                    #[serde(rename = #prop_name)]
                    #prop_id: #name
                });
            }

            ctx.user_mod_sources.push(quote! {
                #[derive(serde::Serialize, serde::Deserialize, Debug)]
                pub struct #type_id {
                    #(#struct_prop_sources), *
                }
            });
        }
        SchemaObject::String => {
            ctx.user_mod_sources.push(quote! {
                type #type_id = String;
            });
        }
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
    method: &Method,
    path: impl AsRef<str>,
    spec: &OperationObject,
) -> TokenStream2 {
    let path = path.as_ref();

    let doc = match &spec.description {
        Some(desc) => format!("`{} {}`\n\n{}", method.as_str(), path, desc),
        None => format!("`{} {}`", method.as_str(), path),
    };

    expand_doc(doc)
}

fn expand_method_doc(
    method: &Method,
    path: impl AsRef<str>,
    status_code: impl AsRef<str>,
    spec: &ResponseObject,
) -> TokenStream2 {
    let path = path.as_ref();
    let status_code = status_code.as_ref();

    expand_doc(format!(
        "`{} {} -> {}`\n\n{}",
        method.as_str(),
        path,
        status_code,
        &spec.description
    ))
}

fn expand_service(
    ctx: &mut OpenAPIExpansionContext,
    controller_id: &syn::Ident,
    route_matcher_source: TokenStream2,
) {
    ctx.service_mod_sources.push(quote! {
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
                let method = request.method().clone();
                let path = request.uri().path().to_owned();

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
    });
}

// basic helper

fn id(id: impl AsRef<str>) -> syn::Ident {
    syn::Ident::new(id.as_ref(), Span2::call_site())
}

/// snake-case
fn snake_case(expr: impl AsRef<str>) -> String {
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
fn upper_camel_case(expr: impl AsRef<str>) -> String {
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

// tests

#[cfg(test)]
mod tests {
    use crate::{snake_case, upper_camel_case};

    #[test]
    fn some_tests() {
        assert_eq!(snake_case("errorCode"), "error_code".to_owned());
        assert_eq!(snake_case("/users"), "users".to_owned());
        assert_eq!(snake_case("/some/thing"), "some_thing".to_owned());
        assert_eq!(upper_camel_case("/users"), "Users".to_owned());
    }
}
