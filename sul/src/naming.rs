use quote::format_ident;

use crate::{get_status_name_lc, id, openapi::OperationObject, snake_case, upper_camel_case};

pub fn get_request_type_id(
    operation: &OperationObject,
    method: impl AsRef<str>,
    path: impl AsRef<str>,
) -> syn::Ident {
    match &operation.operation_id {
        Some(operation_id) => {
            format_ident!("{}Request", upper_camel_case(operation_id))
        }
        None => {
            let method_ucc = upper_camel_case(method.as_ref().to_lowercase());
            let path_sc = upper_camel_case(path);

            format_ident!("{}{}Request", method_ucc, path_sc)
        }
    }
}

pub fn get_response_type_id(
    operation: &OperationObject,
    method: impl AsRef<str>,
    path: impl AsRef<str>,
) -> syn::Ident {
    match &operation.operation_id {
        Some(operation_id) => {
            format_ident!("{}Response", upper_camel_case(operation_id))
        }
        None => {
            let method_ucc = upper_camel_case(method.as_ref().to_lowercase());
            let path_sc = upper_camel_case(path);

            format_ident!("{}{}Response", method_ucc, path_sc)
        }
    }
}

pub fn get_response_builder_param_type_id(
    operation: &OperationObject,
    method: impl AsRef<str>,
    path: impl AsRef<str>,
    status_code: impl AsRef<str>,
) -> syn::Ident {
    let status_name_ucc = upper_camel_case(get_status_name_lc(status_code));

    match &operation.operation_id {
        Some(operation_id) => {
            format_ident!("{}{}", upper_camel_case(operation_id), status_name_ucc)
        }
        None => {
            let method_ucc = upper_camel_case(method.as_ref().to_lowercase());
            let path_ucc = upper_camel_case(path);

            format_ident!("{}{}{}", method_ucc, path_ucc, status_name_ucc)
        }
    }
}

pub fn get_parameter_id(parameter_name: impl AsRef<str>) -> syn::Ident {
    id(snake_case(parameter_name.as_ref()))
}

pub fn get_operation_id(
    operation: &OperationObject,
    method: impl AsRef<str>,
    path: impl AsRef<str>,
) -> syn::Ident {
    match &operation.operation_id {
        Some(operation_id) => id(snake_case(operation_id)),
        None => {
            let method_lc = method.as_ref().to_lowercase();
            let path_sc = snake_case(&path);
            format_ident!("{}_{}", method_lc, path_sc)
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
