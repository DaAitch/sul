use crate::{naming, openapi as oa};
use proc_macro2::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use std::{cmp::Ordering, collections::HashMap, ops::Deref};
use syn::ItemStruct;

const PATH_SEPARATOR: char = '/';

pub fn expand_openapi_tokenstream(
    controller_struct: ItemStruct,
    document: &oa::Document,
) -> TokenStream {
    let stream = OpenAPITokenStream::new(controller_struct);
    stream.expand(document)
}

struct OpenAPITokenStream {
    controller_struct: ItemStruct,
    user_mod_sources: Vec<TokenStream>,
    service_mod_sources: Vec<TokenStream>,
}

impl OpenAPITokenStream {
    fn new(controller_struct: ItemStruct) -> Self {
        Self {
            controller_struct,
            user_mod_sources: Vec::new(),
            service_mod_sources: Vec::new(),
        }
    }

    fn expand(mut self, document: &oa::Document) -> TokenStream {
        self.expand_controller();
        self.expand_service(document);
        self.expand_schema_components(document);
        self.expand_objects(document);

        let user_mod_sources = self.user_mod_sources;
        let service_mod_sources = self.service_mod_sources;

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
    }

    fn expand_controller(&mut self) {
        let controller_struct = &self.controller_struct;
        self.user_mod_sources.push(quote! { #controller_struct })
    }

    fn expand_service(&mut self, document: &oa::Document) {
        let route_matcher_source = {
            let mut routes = Vec::new();

            for (path, item) in &document.paths {
                let item = document
                    .get_path_item_ref(item)
                    .expect("cannot find path item $ref");

                for (method, operation) in item.operations() {
                    routes.push(Route {
                        method,
                        path,
                        operation,
                    });
                }
            }

            self.expand_route_matcher(&routes)
        };

        let controller_id = &self.controller_struct.ident;

        self.service_mod_sources.push(quote! {
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

    fn expand_schema_components(&mut self, document: &oa::Document) {
        match &document.components {
            Some(components) => match &components.schemas {
                Some(schemas) => {
                    for (key, schema_or_ref) in schemas {
                        let id = naming::get_components_name_id(key);
                        self.expand_schema_source(&id, schema_or_ref);
                    }
                }
                None => {}
            },
            None => {}
        }
    }

    fn expand_objects(&mut self, document: &oa::Document) {
        for (path, path_item) in &document.paths {
            if let oa::PathItemObjectOrRef::Object(object) = path_item {
                self.expand_path_item_object(object, path);
            }
        }

        if let Some(components) = &document.components {
            if let Some(path_items) = &components.path_items {
                for (path, path_item) in path_items {
                    if let oa::PathItemObjectOrRef::Object(object) = path_item {
                        self.expand_path_item_object(object, path);
                    }
                }
            }
        }
    }

    /// Expands at the http method, e.g. `paths./users.get` for the name "GetUsers"
    fn expand_response_source(
        &mut self,
        method: &hyper::Method,
        path: &dyn oa::PathItemType,
        operation: &oa::OperationObject,
    ) {
        let mut impl_sources: Vec<TokenStream> = Vec::new();

        let struct_id = operation.response_type_id(method, path);

        for (status_code, response) in &operation.responses {
            let response_data_type_id = operation.response_data_type_id(method, path, status_code);
            self.expand_schema_source(
                &response_data_type_id,
                &response.content.application_json.schema,
            );

            let fn_name_id = status_code.response_fn_name_id();
            let fn_doc_source = expand_method_doc(&method, path, status_code, response);
            let code = status_code.code();

            impl_sources.push(quote! {
                #fn_doc_source
                pub async fn #fn_name_id(data: #response_data_type_id) -> #struct_id {
                    let result = tokio::task::spawn_blocking(move || {
                        let data = data;
                        serde_json::to_string(&data).unwrap()
                    }).await;

                    let response = match result {
                        Ok(content) => {
                            hyper::Response::builder()
                                .status(#code)
                                .body(hyper::Body::from(content))
                                .unwrap()
                        },
                        Err(err) => {
                            hyper::Response::builder()
                                .status(500)
                                .body(hyper::Body::from("tokio spawn failed"))
                                .unwrap()
                        }
                    };

                    #struct_id {
                        response
                    }
                }
            })
        }

        let response_struct_doc_source = expand_response_doc(&method, path, &operation);

        self.user_mod_sources.push(quote! {
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
        &mut self,
        method: &hyper::Method,
        path: &dyn oa::PathItemType,
        operation: &oa::OperationObject,
    ) {
        let request_type_id = naming::get_request_type_id(operation, method, path);
        let request_body_type_id = naming::get_request_body_type_id(&request_type_id);

        match &operation.request_body {
            Some(request_body) => {
                self.expand_schema_source(
                    &request_body_type_id,
                    &request_body.content.application_json.schema,
                );
            }
            None => {
                self.user_mod_sources.push(quote! {
                    pub type #request_body_type_id = ();
                });
            }
        }
    }

    /// Expand schema source, returning the result type.
    fn expand_schema_source(&mut self, type_id: &Ident, schema_or_ref: &oa::SchemaObjectOrRef) {
        match schema_or_ref {
            oa::SchemaObjectOrRef::Ref(r) => {
                let ref_type_id = naming::get_schema_object_ref_type_id(r);
                self.user_mod_sources.push(quote! {
                    type #type_id = #ref_type_id;
                })
            }
            oa::SchemaObjectOrRef::Object(object) => match object {
                oa::SchemaObject::Array(items_schema) => {
                    let sub_type_id = naming::get_schema_array_subtype_id(type_id);
                    self.expand_schema_source(&sub_type_id, items_schema);
                    self.user_mod_sources.push(quote! {
                        type #type_id = std::vec::Vec<#sub_type_id>;
                    });
                }
                oa::SchemaObject::Object(props_schema) => {
                    let mut struct_prop_sources = Vec::new();
                    for (prop_name, prop_schema) in props_schema.deref() {
                        let name = naming::get_schema_prop_type_id(type_id, prop_name);
                        self.expand_schema_source(&name, prop_schema);
                        let prop_id = naming::get_prop_id(prop_name);

                        struct_prop_sources.push(quote! {
                            #[serde(rename = #prop_name)]
                            #prop_id: #name
                        });
                    }

                    self.user_mod_sources.push(quote! {
                        #[derive(serde::Serialize, serde::Deserialize, Debug)]
                        pub struct #type_id {
                            #(#struct_prop_sources), *
                        }
                    });
                }
                oa::SchemaObject::String(_) => {
                    self.user_mod_sources.push(quote! {
                        type #type_id = String;
                    });
                }
                oa::SchemaObject::Integer(oa::DataTypeIntegerFormat::Int32) => {
                    self.user_mod_sources.push(quote! {
                        type #type_id = i32;
                    });
                }
                oa::SchemaObject::Integer(oa::DataTypeIntegerFormat::Int64) => {
                    self.user_mod_sources.push(quote! {
                        type #type_id = i64;
                    });
                }
                oa::SchemaObject::Number(oa::DataTypeNumberFormat::Float) => {
                    self.user_mod_sources.push(quote! {
                        type #type_id = f32;
                    });
                }
                oa::SchemaObject::Number(oa::DataTypeNumberFormat::Double) => {
                    self.user_mod_sources.push(quote! {
                        type #type_id = f64;
                    });
                }
            },
        }
    }

    fn expand_path_item_object(
        &mut self,
        object: &oa::PathItemObject,
        path: &dyn oa::PathItemType,
    ) {
        for (method, operation) in object.operations() {
            self.expand_response_source(&method, path, operation);
            self.expand_parameters_source(&method, path, operation);
            self.expand_request_body_source(&method, path, operation);
        }
    }

    fn expand_parameters_source(
        &mut self,
        method: &hyper::Method,
        path: &dyn oa::PathItemType,
        operation: &oa::OperationObject,
    ) {
        if let Some(parameters) = &operation.parameters {
            for parameter in parameters {
                let schema_or_ref = match &parameter.schema_or_content {
                    oa::ParameterObjectContentOrSchema::Content(content) => {
                        &content.application_json.schema
                    }
                    oa::ParameterObjectContentOrSchema::Schema(schema) => schema,
                };

                let parameter_type_id = naming::get_parameter_type_id(operation, &method, path);
                self.expand_schema_source(&parameter_type_id, schema_or_ref);
            }
        }
    }

    /// Deal:
    /// - `let ctrl = (self.make_controller)();` creates a controller
    /// - `(self.not_found)()(request)` invokes the not_found handler
    ///
    /// Returns: match block source
    fn expand_route_matcher<'a>(
        &mut self,
        routes: impl IntoIterator<Item = &'a Route<'a>>,
    ) -> TokenStream {
        let mut method_map: HashMap<hyper::Method, PathNode> = Default::default();

        for route in routes {
            let node = method_map.entry(route.method.clone()).or_default();
            node.insert_into(route.path.path(), route);
        }

        let match_method_arm_sources = method_map.into_iter().map(|(method, node)| {
            let method_match_arm_source = self.expand_node_matcher(&node, &Vec::new());

            let id = naming::get_method_enum_value(&method);
            quote! {
                hyper::Method:: #id => {
                    #method_match_arm_source
                }
            }
        });

        quote! {
            {
                let mut tokens = path.split( #PATH_SEPARATOR ).filter(|t| t.len() > 0);
                match method {
                    #(#match_method_arm_sources) *

                    #[allow(unreachable_patterns)]
                    _ => {
                        (self.not_found)(request).boxed()
                    }
                }
            }
        }
    }

    fn expand_node_matcher(&mut self, node: &PathNode, parameters: &Vec<Ident>) -> TokenStream {
        let mut keys: Vec<&PathNodeType> = node.children.keys().collect();
        keys.sort();

        let mut match_token_arm_sources = Vec::with_capacity(keys.len());
        for key in keys {
            let node = node.children.get(key).expect("iterating over keys");

            match key {
                &PathNodeType::Template => {
                    let parameter_id = node
                        .parameter_id
                        .as_ref()
                        .expect("parameter items should always have parameter name set");

                    let mut parameters = parameters.clone();
                    parameters.push(parameter_id.clone());
                    let arm_source = self.expand_node_matcher(node, &parameters);

                    match_token_arm_sources.push(quote! {
                        Some(#parameter_id) => {
                            #arm_source
                        }
                    });
                }
                &PathNodeType::Path(ref path) => {
                    let arm_source = self.expand_node_matcher(node, parameters);
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
                route
                    .operation
                    .check_path_parameters(parameters, &route.method, &route.path)
                    .expect("Path parameters have to be valid");

                // construct request types
                let type_id = route.get_request_type_id();
                let parameters_type_id = route.get_request_parameters_type_id();
                let body_type_id = route.get_request_body_type_id();

                {
                    let fields = parameters.iter().map(|p| {
                        quote! {
                            pub #p: String
                        }
                    });

                    self.user_mod_sources.push(quote! {
                        pub struct #type_id {
                            pub parameters: #parameters_type_id,
                            pub body: #body_type_id,
                        }

                        pub struct #parameters_type_id {
                            #(#fields), *
                        }
                    });
                }

                let initializer = parameters.iter().map(|p| {
                    quote! {
                        #p: #p.to_owned()
                    }
                });

                let operation_id = route.get_operation_id();
                let response_type_id = route.get_response_type_id();

                let future_source = match route.operation.request_body {
                    // when we need to deserialize a request body
                    Some(_) => {
                        quote! {
                            let body = hyper::body::to_bytes(request).await.unwrap(); // TODO: error handling
                            let body = serde_json::from_slice::<super::#body_type_id>(body.as_ref());
                            match body {
                                Ok(body) => {
                                    let response: super::#response_type_id = controller.#operation_id ( super:: #type_id {
                                        parameters,
                                        body,
                                    }).await;

                                    let response: hyper::Response<hyper::Body> = response.into();
                                    response
                                }
                                Err(error) => {
                                    // TODO: make a handler?
                                    hyper::Response::builder()
                                        .status(400)
                                        .body(hyper::Body::from(format!("Error: {}", error)))
                                        .unwrap()
                                }
                            }
                        }
                    }
                    // When we don't need to deserialize a request body.
                    // That is because deserializing a union `()` wants a JSON `null` value.
                    // An empty payload is not a valid JSON.
                    None => quote! {
                        let response: super::#response_type_id = controller.#operation_id ( super:: #type_id {
                            parameters,
                            body: (),
                        }).await;

                        let response: hyper::Response<hyper::Body> = response.into();
                        response
                    },
                };

                Some(quote! {
                    None => {
                        let controller = (self.make_controller)();
                        let parameters = super::#parameters_type_id {
                            #(#initializer), *
                        };

                        async move {
                            #future_source
                        }.boxed()
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
}

// OpenAPI helper

fn expand_doc(text: impl AsRef<str>) -> TokenStream {
    let text = text.as_ref();

    quote! {
        #[doc = #text]
    }
}

fn expand_response_doc(
    method: &hyper::Method,
    path: &dyn oa::PathItemType,
    spec: &oa::OperationObject,
) -> TokenStream {
    let doc = match &spec.description {
        Some(desc) => format!("`{} {}`\n\n{}", method.as_str(), path, desc),
        None => format!("`{} {}`", method.as_str(), path),
    };

    expand_doc(doc)
}

fn expand_method_doc(
    method: &hyper::Method,
    path: &dyn oa::PathItemType,
    status_code: &oa::StatusCode,
    spec: &oa::ResponseObject,
) -> TokenStream {
    expand_doc(format!(
        "`{} {} -> {}`\n\n{}",
        method.as_str(),
        path,
        status_code,
        &spec.description
    ))
}

struct Route<'a> {
    method: hyper::Method,
    path: &'a oa::PathItemKey,
    operation: &'a oa::OperationObject,
}

impl<'a> Route<'a> {
    fn get_operation_id(&self) -> syn::Ident {
        naming::get_operation_id(self.operation, &self.method, self.path)
    }

    fn get_request_type_id(&self) -> syn::Ident {
        naming::get_request_type_id(self.operation, &self.method, self.path)
    }

    fn get_response_type_id(&self) -> syn::Ident {
        self.operation.response_type_id(&self.method, self.path)
    }

    fn get_request_parameters_type_id(&self) -> syn::Ident {
        let id = self.get_request_type_id();
        naming::get_request_parameters_type_id(&id)
    }

    fn get_request_body_type_id(&self) -> syn::Ident {
        let id = self.get_request_type_id();
        naming::get_request_body_type_id(&id)
    }
}

struct PathNode<'a> {
    parameter_id: Option<Ident>,
    route: Option<&'a Route<'a>>,
    children: HashMap<PathNodeType, PathNode<'a>>,
}

impl<'a> PathNode<'a> {
    fn insert_into(&mut self, outstanding_path: impl AsRef<str>, route: &'a Route) {
        let mut node = self;

        for token in outstanding_path.as_ref().split(PATH_SEPARATOR).skip(1) {
            let (key, parameter_name) = get_node_type(token);

            let child = node.children.entry(key).or_insert_with(|| PathNode {
                parameter_id: parameter_name.clone(),
                ..Default::default()
            });

            assert_eq!(child.parameter_id, parameter_name);
            node = child;
        }

        node.route = Some(route);
    }
}

impl<'a> Default for PathNode<'a> {
    fn default() -> Self {
        PathNode {
            parameter_id: None,
            route: None,
            children: Default::default(),
        }
    }
}

/// returns (type, parameter name)
fn get_node_type(path_token: impl AsRef<str>) -> (PathNodeType, Option<Ident>) {
    const TEMPLATE_START: char = '{';
    const TEMPLATE_END: char = '}';

    let path_token = path_token.as_ref();
    if path_token.starts_with(TEMPLATE_START) && path_token.ends_with(TEMPLATE_END) {
        (
            PathNodeType::Template,
            Some(Ident::new(
                &path_token[1..path_token.len() - 1],
                Span::call_site(),
            )),
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
