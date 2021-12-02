use quote::format_ident;

use crate::{id, openapi::OperationObject, snake_case, upper_camel_case};

pub fn get_request_type_id(method: impl AsRef<str>, path: impl AsRef<str>) -> syn::Ident {
    let method_ucc = upper_camel_case(method.as_ref().to_lowercase());
    let path_sc = upper_camel_case(path);

    format_ident!("{}{}Request", method_ucc, path_sc)
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
