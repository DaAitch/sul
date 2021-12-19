#![feature(proc_macro_span)]

use proc_macro::{Span, TokenStream};
use syn::{parse_macro_input, AttributeArgs, ItemStruct, Lit, NestedMeta};
use tokenstream::expand_openapi_tokenstream;

mod naming;
mod openapi;
mod tokenstream;
mod util;

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

        openapi::Document::from_file(yaml_file_path).unwrap()
    };

    let controller_struct = parse_macro_input!(item as ItemStruct);
    expand_openapi_tokenstream(controller_struct, &document).into()
}
