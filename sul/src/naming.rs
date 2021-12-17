use crate::openapi as oa;
use proc_macro2::{Ident, Span};
use quote::format_ident;

pub fn get_operation_id_ucc(
    operation: &oa::OperationObject,
    method: impl AsRef<str>,
    path: impl AsRef<str>,
) -> syn::Ident {
    match &operation.operation_id {
        Some(operation_id) => id(upper_camel_case(operation_id)),
        None => format_ident!(
            "{}{}",
            upper_camel_case(method.as_ref().to_lowercase()),
            upper_camel_case(&path)
        ),
    }
}

pub fn get_request_type_id(
    operation: &oa::OperationObject,
    method: impl AsRef<str>,
    path: impl AsRef<str>,
) -> syn::Ident {
    let prefix = match &operation.operation_id {
        Some(operation_id) => upper_camel_case(operation_id),
        None => {
            let method_ucc = upper_camel_case(method.as_ref().to_lowercase());
            let path_sc = upper_camel_case(path);

            format!("{}{}", method_ucc, path_sc)
        }
    };

    format_ident!("{}Request", prefix)
}

pub fn get_response_type_id(
    operation: &oa::OperationObject,
    method: impl AsRef<str>,
    path: impl AsRef<str>,
) -> syn::Ident {
    let prefix = match &operation.operation_id {
        Some(operation_id) => upper_camel_case(operation_id),
        None => {
            let method_ucc = upper_camel_case(method.as_ref().to_lowercase());
            let path_sc = upper_camel_case(path);

            format!("{}{}", method_ucc, path_sc)
        }
    };

    format_ident!("{}Response", prefix)
}

pub fn get_response_builder_param_type_id(
    operation: &oa::OperationObject,
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

pub fn get_parameter_type_id(
    operation: &oa::OperationObject,
    method: impl AsRef<str>,
    path: impl AsRef<str>,
) -> syn::Ident {
    let operation_id_ucc = get_operation_id_ucc(operation, method, path);
    format_ident!("{}Parameter", operation_id_ucc)
}

pub fn get_operation_id(
    operation: &oa::OperationObject,
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

pub fn get_status_name_lc(status_code: impl AsRef<str>) -> &'static str {
    match status_code.as_ref() {
        "200" => "ok",
        "401" => "unauthorized",
        _ => "unknown",
    }
}

pub fn get_status_code_response_fn_name_id(status_code: impl AsRef<str>) -> Ident {
    id(get_status_name_lc(status_code))
}

pub fn get_method_enum_value(method: &hyper::Method) -> Ident {
    id(method)
}

pub fn get_parameter_name_id(name: impl AsRef<str>) -> Ident {
    id(name)
}

// basic helper

fn id(id: impl AsRef<str>) -> syn::Ident {
    syn::Ident::new(id.as_ref(), Span::call_site())
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
    use super::*;

    #[test]
    fn some_tests() {
        assert_eq!(snake_case("errorCode"), "error_code".to_owned());
        assert_eq!(snake_case("/users"), "users".to_owned());
        assert_eq!(snake_case("/some/thing"), "some_thing".to_owned());
        assert_eq!(upper_camel_case("/users"), "Users".to_owned());
    }
}
