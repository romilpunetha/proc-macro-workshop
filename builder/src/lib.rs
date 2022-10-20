use quote::{format_ident, quote};
use syn::parse_macro_input;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as syn::DeriveInput);

    let fields: &syn::Fields = match ast.data {
        syn::Data::Struct(ref data) => &data.fields,
        syn::Data::Enum(_) | syn::Data::Union(_) => {
            unimplemented!()
        }
    };

    let builder_struct = get_builder_struct(fields, &ast);

    let identity_impl = get_identity_impl(fields, &ast);

    let builder_impl = get_builder_impl(fields, &ast);

    proc_macro::TokenStream::from(quote! {
        #builder_struct
        #identity_impl
        #builder_impl
    })
}

fn get_builder_struct(fields: &syn::Fields, ast: &syn::DeriveInput) -> proc_macro2::TokenStream {
    let name = &ast.ident;
    let builder_name = format_ident!("{}Builder", name);

    let builder_fields = fields.iter().map(|f: &syn::Field| {
        let field_name = &f.ident;
        let ty = &f.ty;
        if extract_inner_type(ty, "Option").is_some() {
            quote! { #field_name: #ty }
        } else {
            quote! { #field_name: std::option::Option<#ty> }
        }
    });

    quote! {
        pub struct #builder_name {
            #(#builder_fields),*
        }
    }
}

fn get_identity_impl(fields: &syn::Fields, ast: &syn::DeriveInput) -> proc_macro2::TokenStream {
    let name = &ast.ident;
    let builder_name = format_ident!("{}Builder", name);
    let init_builder_fields = fields.iter().map(|f: &syn::Field| {
        let field_name = &f.ident;
        let ty = &f.ty;
        let inner_type = extract_inner_type(ty, "Vec");
        if inner_type.is_some() {
            quote!(#field_name: std::option::Option::Some(vec!()))
        } else {
            quote!(#field_name: std::option::Option::None)
        }
    });

    quote! {
        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                     #(#init_builder_fields),*
                }
            }
        }
    }
}

fn get_builder_impl(fields: &syn::Fields, ast: &syn::DeriveInput) -> proc_macro2::TokenStream {
    let name = &ast.ident;
    let builder_name = format_ident!("{}Builder", name);

    let setter_methods = get_setter_methods(fields);
    let set_fields = fields.iter().map(|f: &syn::Field| {
        let field_name = &f.ident;
        let ty = &f.ty;

        let inner_ty = extract_inner_type(ty, "Option");
        if inner_ty.is_some() {
            quote! { #field_name: self.#field_name.clone() }
        } else {
            quote! { #field_name: self.#field_name.clone().ok_or("field was not set")? }
        }
    });
    quote! {
        impl #builder_name {
            #setter_methods

            fn build(&mut self) -> std::result::Result<Command, std::boxed::Box<dyn std::error::Error>> {
                std::result::Result::Ok(#name {
                #(#set_fields),*
                })
            }
        }
    }
}

fn get_setter_methods(fields: &syn::Fields) -> proc_macro2::TokenStream {
    let setter_methods = fields.iter().map(|f: &syn::Field| {
        let field_name = &f.ident;
        let single_setter = get_single_setter(f);
        if f.attrs.is_empty() {
            quote! {
                #single_setter
            }
        } else {
            match extract_each_attr_value(f.attrs.as_ref()) {
                Ok(attr_value) => {
                    let multi_setter = get_multi_setter(&attr_value, f);
                    if field_name.clone().unwrap() == attr_value {
                        quote! {
                         #multi_setter
                        }
                    } else {
                        let multi_setter = get_multi_setter(&attr_value, f);
                        quote! {
                            #single_setter
                            #multi_setter
                        }
                    }
                }
                std::result::Result::Err(err) => err,
            }
        }
    });

    quote! {
        #(#setter_methods)*
    }
}

fn get_single_setter(f: &syn::Field) -> proc_macro2::TokenStream {
    let field_name = &f.ident;
    let ty = &f.ty;
    let inner_ty = extract_inner_type(ty, "Option");

    if inner_ty.is_some() {
        quote! {
             fn #field_name(&mut self, #field_name: #inner_ty) -> &mut Self {
                self.#field_name = std::option::Option::Some(#field_name);
                self
            }
        }
    } else {
        quote! {
             fn #field_name(&mut self, #field_name: #ty) -> &mut Self {
                self.#field_name = std::option::Option::Some(#field_name);
                self
            }
        }
    }
}

fn get_multi_setter(method_name: &String, f: &syn::Field) -> proc_macro2::TokenStream {
    let field_name = &f.ident;
    let ty = extract_inner_type(&f.ty, "Vec");
    let method_name = format_ident!("{}", method_name);

    quote! {
        fn #method_name(&mut self, #field_name: #ty) -> &mut Self {
            if let std::option::Option::Some(ref mut v) = self.#field_name {
                v.push(#field_name);
            } else {
                self.#field_name = std::option::Option::Some(vec![#field_name]);
            }
            self
        }
    }
}

fn extract_inner_type<'t>(ty: &'t syn::Type, expected_ident: &str) -> Option<&'t syn::Type> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { segments, .. },
        ..
    }) = ty
    {
        if let std::option::Option::Some(syn::PathSegment {
            ident,
            arguments:
                syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, .. }),
        }) = segments.last()
        {
            if ident == expected_ident {
                if let std::option::Option::Some(syn::GenericArgument::Type(ty)) = args.last() {
                    return std::option::Option::Some(ty);
                }
            }
        }
    }
    None
}

fn extract_each_attr_value(
    attrs: &[syn::Attribute],
) -> std::result::Result<String, proc_macro2::TokenStream> {
    if let std::option::Option::Some(attr) = attrs.last() {
        if let std::result::Result::Ok(syn::Meta::List(list)) = attr.parse_meta() {
            if list.path.is_ident("builder") {
                if let std::option::Option::Some(syn::NestedMeta::Meta(syn::Meta::NameValue(
                    syn::MetaNameValue {
                        path,
                        lit: syn::Lit::Str(lit_str),
                        ..
                    },
                ))) = list.nested.last()
                {
                    return if path.is_ident("each") {
                        std::result::Result::Ok(lit_str.value())
                    } else {
                        std::result::Result::Err(
                            syn::Error::new_spanned(list, "expected `builder(each = \"...\")`")
                                .to_compile_error(),
                        )
                    };
                }
            }
        }
    }
    unreachable!("you should've check for attrs emptiness before")
}
