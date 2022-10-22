extern crate core;

use quote::quote;
use syn::{parse_macro_input, parse_quote};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let _ = input;

    let ast = parse_macro_input!(input as syn::DeriveInput);

    let struct_name = &ast.ident;

    let fields = match ast.data {
        syn::Data::Struct(ref data) => &data.fields,
        syn::Data::Enum(_) | syn::Data::Union(_) => {
            unimplemented!()
        }
    };

    let generics = add_trait_bounds(fields, ast.generics);

    let (impl_generics, ty_generics, where_clause) = &generics.split_for_impl();

    let place_value = fields.iter().map(|f: &syn::Field| {
        let str_val = match &f.ident {
            Some(x) => {
                format!("{}", x)
            }
            _ => "".to_string(),
        };

        let name = &f.ident;
        let val = get_formatted_attr_value(&f.attrs);
        let format = match val {
            None => {
                quote! { &self.#name }
            }
            Some(x) => {
                quote! { &format_args!(#x, &self.#name) }
            }
        };
        quote! {
            .field(#str_val, #format)
        }
    });

    let struct_str = format!("{}", struct_name);
    proc_macro::TokenStream::from(quote! {
        impl #impl_generics std::fmt::Debug for #struct_name #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {

                f.debug_struct(#struct_str)
                #(#place_value)*
                .finish()
            }
        }
    })
}

fn get_formatted_attr_value(attrs: &[syn::Attribute]) -> std::option::Option<String> {
    if let std::option::Option::Some(attr) = attrs.last() {
        if let std::result::Result::Ok(syn::Meta::NameValue(syn::MetaNameValue {
            lit: syn::Lit::Str(lit_str),
            path,
            ..
        })) = attr.parse_meta()
        {
            if path.is_ident("debug") {
                return std::option::Option::Some(lit_str.value());
            }
        }
    }
    None
}

fn add_trait_bounds(fields: &syn::Fields, generics: syn::Generics) -> syn::Generics {
    let mut phantom_ty_idents = std::collections::HashSet::new();
    let mut non_phantom_ty_idents = std::collections::HashSet::new();
    let mut g = generics;
    for field in fields {
        match extract_ty_idents(field) {
            None => {}
            Some((ident, Some(ty))) => {
                if ident == "PhantomData" {
                    phantom_ty_idents.insert(ty);
                }
            }
            Some((ident, None)) => {
                non_phantom_ty_idents.insert(ident);
            }
        }
    }

    for param in &mut g.params {
        if let syn::GenericParam::Type(type_param) = param {
            if !phantom_ty_idents.contains(&type_param.ident) {
                type_param.bounds.push(parse_quote!(std::fmt::Debug));
            }
        }
    }
    g
}

fn extract_ty_idents(
    field: &syn::Field,
) -> Option<(&syn::Ident, std::option::Option<&syn::Ident>)> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { segments, .. },
        ..
    }) = &field.ty
    {
        if let std::option::Option::Some(syn::PathSegment {
            ident,
            arguments:
                syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, .. }),
        }) = segments.last()
        {
            if let std::option::Option::Some(syn::GenericArgument::Type(ty)) = args.last() {
                match extract_inner_type(ty) {
                    None => {}
                    Some(inner_type) => {
                        return std::option::Option::Some((ident, Some(inner_type)));
                    }
                }
            }
            return std::option::Option::Some((ident, None));
        } else if let std::option::Option::Some(syn::PathSegment { ident, .. }) = segments.last() {
            return std::option::Option::Some((ident, std::option::Option::None));
        }
    }
    std::option::Option::None
}

fn extract_inner_type(ty: &syn::Type) -> Option<&syn::Ident> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { segments, .. },
        ..
    }) = ty
    {
        if let std::option::Option::Some(syn::PathSegment { ident, .. }) = segments.last() {
            return std::option::Option::Some(ident);
        }
    }
    None
}
