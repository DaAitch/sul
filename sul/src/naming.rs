use quote::format_ident;

use crate::{id, snake_case, upper_camel_case};

pub fn get_request_type_id(method: impl AsRef<str>, path: impl AsRef<str>) -> syn::Ident {
    let method_ucc = upper_camel_case(method.as_ref().to_lowercase());
    let path_sc = upper_camel_case(path);

    format_ident!("{}{}Request", method_ucc, path_sc)
}

pub fn get_parameter_id(parameter_name: impl AsRef<str>) -> syn::Ident {
    id(snake_case(parameter_name.as_ref()))
}
