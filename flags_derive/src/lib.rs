#![allow(dead_code, unused_imports)]
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::mem::Discriminant;
use std::str::FromStr;

use darling::FromMeta;
use proc_macro::TokenStream;
use proc_macro_crate::FoundCrate;
use proc_macro_crate::crate_name;
use proc_macro2::Span;
use proc_macro2::TokenStream as TokenStream2;
use quote::ToTokens;
use quote::quote;
use quote::quote_spanned;
use syn::DataEnum;
use syn::Error;
use syn::ExprField;
use syn::FieldMutability;
use syn::Fields;
use syn::File;
use syn::Ident;
use syn::Index;
use syn::Item;
use syn::ItemEnum;
use syn::ItemMod;
use syn::ItemStruct;
use syn::Lit;
use syn::LitInt;
use syn::Member;
use syn::Token;
use syn::Type;
use syn::TypePath;
use syn::Visibility;
use syn::parse::discouraged;
use syn::parse_quote;
use syn::spanned::Spanned;
use syn::token;
// use syn::token::Type;
use syn::{Expr, ExprLit};
fn _derive_flags(item: TokenStream) -> Result<TokenStream, syn::Error> {
    let contents: File = syn::parse(item.clone())?;
    let mut module_items = Vec::new();
    let mut resulting = TokenStream2::new();
    let mut flag_enum = None;
    let mut flags_struct = None;
    for item in contents.items {
        match item {
            Item::Enum(mut item @ ItemEnum { .. })
                if item
                    .attrs
                    .iter()
                    .find(|a| -> bool { a.path() == &parse_quote!(flag) })
                    .is_some() =>
            {
                item.attrs.retain(|a| a.path() != &parse_quote!(flag));
                flag_enum = Some(item);
            }
            Item::Struct(mut item @ ItemStruct { .. })
                if item
                    .attrs
                    .iter()
                    .find(|a| -> bool { a.path() == &parse_quote!(flags) })
                    .is_some() =>
            {
                item.attrs.retain(|a| a.path() != &parse_quote!(flags));
                flags_struct = Some(item);
            }
            a => module_items.push(a),
        }
    }
    let mut flag_enum = if let Some(item) = flag_enum {
        item
    } else {
        return Err(Error::new(
            Span::call_site(),
            "No enum found marked with `#[flag]` attribute",
        ));
    };
    let mut flags_struct = if let Some(item) = flags_struct {
        item
    } else {
        return Err(Error::new(
            Span::call_site(),
            "No struct found marked with `#[flags]` attribute",
        ));
    };
    if flags_struct.fields.len() != 1 {
        return Err(Error::new(
            flags_struct.span(),
            format!("Flag struct must have one and only one field"),
        ));
    }
    let (bits_field, tp, intsize) = match flags_struct.fields.iter_mut().next().unwrap() {
        syn::Field { vis, ident, ty, .. } => {
            *vis = Visibility::Public(Token![pub](vis.span()));
            let tp = ty.to_token_stream().to_string();
            if !tp.starts_with("u") {
                return Err(Error::new(ty.span(), "Type must be an unsigned integer"));
            };
            let size = if &tp == "usize" {
                std::mem::size_of::<usize>() * 8
            } else {
                if let Ok(v @ (8 | 16 | 32 | 64)) = usize::from_str(&tp[1..]) {
                    v
                } else {
                    return Err(Error::new(
                        ty.span(),
                        "Type must be one of u8 u16 u32 u64 or usize",
                    ));
                }
            };

            (
                if let Some(name) = ident {
                    Member::Named(name.clone())
                } else {
                    Member::Unnamed(Index::from(0))
                },
                Ident::new(&tp, ty.span()),
                size,
            )
        }
    };
    let mut disciminants: BTreeSet<u32> = BTreeSet::new();
    for var in flag_enum.variants.iter_mut() {
        // var.discriminant.map(|(_,e)|  )
        let req = match var.discriminant.as_ref().map(|a| &a.1) {
            Some(Expr::Lit(ExprLit {
                lit: Lit::Int(li), ..
            })) => match li.base10_parse::<u128>() {
                Ok(a) if a.is_power_of_two() => a.trailing_zeros(),
                Ok(_) => {
                    return Err(Error::new(
                        li.span(),
                        format!("All variants values must be a power of 2."),
                    ));
                }
                Err(e) => return Err(e.into()),
            },

            Some(a) => {
                return Err(Error::new(a.span(), "Discriminant must be literal"));
            }
            None => disciminants.iter().max().map(|a| a + 1).unwrap_or(0),
        };
        let mut s = String::new();
        s.push_str("0b");
        s.push_str(&"0".repeat(intsize - (req as usize) - 1));
        s.push_str("1");
        s.push_str(&"0".repeat(req as usize));
        let v = LitInt::new(&s, var.span());
        var.discriminant = Some((token::Eq(var.span()), parse_quote!(#v)));
        if !disciminants.insert(req) {
            return Err(Error::new(var.span(), "Duplicate keys"));
        }
    }

    let enum_ident = flag_enum.ident.clone();
    let struct_ident = flags_struct.ident.clone();
    let mod_ident = Ident::new(
        &format!("__flagmod_{:}", &struct_ident.to_string().to_lowercase()),
        struct_ident.span(),
    );
    let mut a: u32 = 0;
    for d in disciminants {
        a |= 1 << d;
    }
    let mask = LitInt::new(&format!("0b{:b}", a), enum_ident.span());
    let crate_ = match crate_name("easybitflags") {
        Ok(o) => o,
        Err(e) => return Err(Error::new(Span::call_site(), e.to_string())),
    };
    let use_crate = match crate_ {
        FoundCrate::Itself => quote!(
            use crate::*;
        ),
        FoundCrate::Name(name) => {
            let ident = Ident::new(&name, Span::call_site());
            quote!( use #ident::*; )
        }
    };
    resulting.extend(quote! {
        #[allow(dead_code)]
        pub mod #mod_ident {
            use std::fmt::Debug;

            use std::ops::{BitAnd, BitOr, BitXor, Deref, Not, Shl, Shr};

            #use_crate

            #[repr(#tp)]
            #[derive(PartialEq, Eq,Clone,Copy)]
            #flag_enum

            impl AsRef<#struct_ident> for #enum_ident {
                fn as_ref(&self) -> &#struct_ident {
                    unsafe {std::mem::transmute(self)}
                }
            }
            #[derive(Clone,Copy,PartialEq, Eq)]
            #[repr(transparent)]
            #flags_struct
            impl Deref for #struct_ident {
                type Target = #tp;
                fn deref(&self) -> &#tp {
                    &self.#bits_field
                }
            }
            impl AsRef<#tp> for #struct_ident {
                fn as_ref(&self) -> &#tp {
                    &self.#bits_field
                }

            }

            impl FlagValue for #enum_ident {
                type Bits = #tp;
                type Collection = #struct_ident;
                const MASK: #tp = #mask;
                #[inline]
                fn to_primitive(&self) -> #tp {
                    unsafe { std::mem::transmute(*self) }
                }
                #[inline]
                fn from_primitive(value:&#tp) -> Option<Self> {

                    unsafe { std::mem::transmute(*value) }
                }
            }
            impl From<#enum_ident> for #struct_ident {
                fn from(value:#enum_ident) -> Self {
                    unsafe {std::mem::transmute(value)}
                }
            }
            impl FlagValueCollection<#enum_ident> for #struct_ident {

                #[inline]
                fn to_primitive(&self) -> #tp {
                    unsafe { std::mem::transmute(*self) }
                }
                #[inline]
                fn from_primitive(value:&#tp) -> Self {
                    unsafe { std::mem::transmute(*value & #enum_ident::MASK) }
                }
                fn len(&self) -> u32 {
                    self.count_ones()
                }

            }
            impl Debug for #struct_ident {
                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    let mut a = self.into_iter();
                    let mut b = a.next();
                    write!(f, "(")?;
                    loop {
                        if b.is_none() {
                            break;
                        }
                        let v = b.unwrap();
                        write!(f, "{:?}", v)?;

                        b = a.next();
                        if b.is_some() {
                            write!(f, " | ")?;
                        }
                    }
                    write!(f, ")")
                }
            }
            impl<T: Into<#struct_ident>> BitOr<T> for #struct_ident {
                type Output = Self;
                fn bitor(self, rhs: T) -> Self::Output {
                    Self(self.0.bitor(rhs.into().0))
                }
            }
            impl<T: Into<#struct_ident>> BitOr<T> for #enum_ident {
                type Output = #struct_ident;
                fn bitor(self, rhs: T) -> Self::Output {
                    #struct_ident(self as #tp | rhs.into().0)
                }
            }

            impl<T: Into<#struct_ident>> BitAnd<T> for #struct_ident {
                type Output = #struct_ident;

                fn bitand(self, rhs: T) -> Self::Output {
                    Self(self.0.bitand(rhs.into().0))
                }
            }
            impl<T: Into<#struct_ident>> BitAnd<T> for #enum_ident {
                type Output = #struct_ident;
                fn bitand(self, rhs: T) -> Self::Output {
                    #struct_ident(self as #tp & rhs.into().0)
                }
            }
            impl Not for #enum_ident {
                type Output = #struct_ident;

                fn not(self) -> Self::Output {
                    unsafe { std::mem::transmute(!(self as #tp) & Self::MASK) }
                }
            }
            impl Not for #struct_ident {
                type Output = #struct_ident;

                fn not(self) -> Self::Output {
                    unsafe { std::mem::transmute(!self.0 & #enum_ident::MASK) }
                }
            }
                impl PartialEq<#enum_ident> for #struct_ident {
                    fn eq(&self, other: &#enum_ident) -> bool {
                        self.0 == *other as #tp
                    }
                }
                impl PartialEq<#struct_ident> for #enum_ident {
                    fn eq(&self, other: &#struct_ident) -> bool {
                        *self as #tp == other.0
                    }
                }
                    }
        pub use #mod_ident::*;
    });
    Ok(resulting.into())
}

fn wrapper<T, R: Into<TokenStream>, F: Fn(T) -> Result<R, syn::Error>>(
    value: T,
    func: F,
) -> TokenStream {
    match func(value) {
        Ok(t) => t.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

/// Derive flags
#[proc_macro]
pub fn derive_flags(item: TokenStream) -> TokenStream {
    wrapper(item, _derive_flags)
}
