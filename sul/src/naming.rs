use crate::openapi as oa;
use crate::util::{snake_case, upper_camel_case};
use proc_macro2::Span;
use quote::format_ident;

/// needed for `..Parameter` type
pub fn get_operation_id_ucc(
    operation: &oa::OperationObject,
    method: &oa::Method,
    path: &dyn oa::PathItemType,
) -> syn::Ident {
    match &operation.operation_id {
        Some(operation_id) => id(upper_camel_case(operation_id)),
        None => format_ident!("{}{}", method.ucc_token_id(), path.type_token()),
    }
}

pub fn get_request_type_id(
    operation: &oa::OperationObject,
    method: &oa::Method,
    path: &dyn oa::PathItemType,
) -> syn::Ident {
    let prefix = match &operation.operation_id {
        Some(operation_id) => upper_camel_case(operation_id),
        None => {
            format!("{}{}", method.ucc_token_id(), path.type_token())
        }
    };

    format_ident!("{}Request", prefix)
}

pub fn get_parameter_type_id(
    operation: &oa::OperationObject,
    method: &oa::Method,
    path: &dyn oa::PathItemType,
) -> syn::Ident {
    let operation_id_ucc = get_operation_id_ucc(operation, method, path);
    format_ident!("{}Parameter", operation_id_ucc)
}

pub fn get_operation_id(
    operation: &oa::OperationObject,
    method: &oa::Method,
    path: &dyn oa::PathItemType,
) -> syn::Ident {
    match &operation.operation_id {
        Some(operation_id) => id(snake_case(operation_id)),
        None => {
            format_ident!("{}_{}", method.sc_token_id(), path.fn_token())
        }
    }
}

pub fn get_schema_prop_type_id(
    parent_prop_type_id: &syn::Ident,
    prop_name: impl AsRef<str>,
) -> syn::Ident {
    format_ident!("{}{}", parent_prop_type_id, upper_camel_case(prop_name))
}

pub fn get_prop_id(prop_name: impl AsRef<str>) -> syn::Ident {
    id(snake_case(prop_name))
}

pub fn get_request_parameters_type_id(request_type_id: &syn::Ident) -> syn::Ident {
    format_ident!("{}{}", request_type_id, "Parameters")
}

pub fn get_request_body_type_id(request_type_id: &syn::Ident) -> syn::Ident {
    format_ident!("{}{}", request_type_id, "Body")
}

pub fn get_schema_array_subtype_id(parent_prop_type_id: &syn::Ident) -> syn::Ident {
    format_ident!("{}{}", parent_prop_type_id, "Item")
}

pub fn get_schema_object_ref_type_id(r: &oa::SchemaObjectRef) -> syn::Ident {
    // TODO(daaitch): deny-list ? name collision
    format_ident!("{}", r.name)
}

pub fn get_components_name_id(name: impl AsRef<str>) -> syn::Ident {
    format_ident!("{}", upper_camel_case(name.as_ref()))
}

// basic helper

fn id(id: impl AsRef<str>) -> syn::Ident {
    syn::Ident::new(id.as_ref(), Span::call_site())
}
