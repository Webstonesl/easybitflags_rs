pub use flags_derive::derive_flags;
use std::{
    fmt::Debug,
    marker::PhantomData,
    ops::{BitAnd, BitOr, BitXor, Deref, Not, Shl, Shr},
};

// A trait to define which primitives can be used as a primitive for a flags. Restricted to unsigned types
pub trait FlagPrimitive:
    Sized
    + Not<Output = Self>
    + BitAnd<Self, Output = Self>
    + BitOr<Self, Output = Self>
    + BitAnd<Self, Output = Self>
    + BitXor<Self, Output = Self>
    + Shl<Self, Output = Self>
    + Shr<Self, Output = Self>
    + Clone
    + Copy
    + Debug
    + Eq
{
    const ZERO: Self;
    const ONE: Self;
}
impl FlagPrimitive for u8 {
    const ZERO: Self = 0;
    const ONE: Self = 1;
}
impl FlagPrimitive for u16 {
    const ZERO: Self = 0;
    const ONE: Self = 1;
}
impl FlagPrimitive for u32 {
    const ZERO: Self = 0;
    const ONE: Self = 1;
}
impl FlagPrimitive for u64 {
    const ZERO: Self = 0;
    const ONE: Self = 1;
}
impl FlagPrimitive for u128 {
    const ZERO: Self = 0;
    const ONE: Self = 1;
}
impl FlagPrimitive for usize {
    const ZERO: Self = 0;
    const ONE: Self = 1;
}
pub trait FlagValue: Sized + Debug + Clone + Copy + AsRef<Self::Collection> {
    type Bits: FlagPrimitive;
    type Collection: FlagValueCollection<Self>;
    const MASK: Self::Bits;
    fn to_primitive(&self) -> Self::Bits;
    fn from_primitive(value: &Self::Bits) -> Option<Self>;
    fn all() -> Self::Collection {
        Self::Collection::from_primitive(&Self::MASK)
    }
}
pub trait FlagValueCollection<E: FlagValue>:
    Sized + Deref<Target = E::Bits> + AsRef<E::Bits> + From<E>
{
    fn to_primitive(&self) -> E::Bits;
    fn from_primitive(value: &E::Bits) -> Self;
    fn into_iter(&self) -> FlagIterator<E::Bits, E, Self> {
        FlagIterator::new(self.to_primitive())
    }
    fn len(&self) -> u32;
}
pub struct FlagIterator<T: FlagPrimitive, V: FlagValue<Bits = T>, C: FlagValueCollection<V>> {
    mask: T,
    current_bit: T,
    value: T,
    phantom: PhantomData<V>,
    collection: PhantomData<C>,
}
impl<T: FlagPrimitive, V: FlagValue<Bits = T>, C: FlagValueCollection<V>> FlagIterator<T, V, C> {
    pub fn new(value: T) -> Self {
        FlagIterator {
            mask: V::MASK,
            current_bit: T::ONE,
            value,
            phantom: PhantomData,
            collection: PhantomData,
        }
    }
}
impl<T: FlagPrimitive, V: FlagValue<Bits = T>, C: FlagValueCollection<V>> Iterator
    for FlagIterator<T, V, C>
{
    type Item = V;

    fn next(&mut self) -> Option<Self::Item> {
        if self.mask == T::ZERO {
            return None;
        }
        while self.mask & self.current_bit == T::ZERO {
            self.current_bit = self.current_bit << T::ONE;
        }
        self.mask = self.mask & !self.current_bit & self.value;
        V::from_primitive(&self.current_bit)
    }
}

#[cfg(test)]
mod test {
    use crate::*;
    use flags_derive::derive_flags;
    use std::ops::Deref;
    derive_flags! {
        #[derive(Debug)]
        #[flag]
        pub enum Flag {
            A,B,C,D
        }
        #[flags]
        pub struct Flags(u64);
    }

    #[derive(Debug, Clone, Copy)]
    #[repr(u8)]
    pub enum Style {
        Bold = 1,
        Italic = 2,
        Underline = 4,
    }
    impl AsRef<Styles> for Style {
        fn as_ref(&self) -> &Styles {
            unsafe { std::mem::transmute(self) }
        }
    }
    impl FlagValue for Style {
        type Collection = Styles;
        type Bits = u8;

        const MASK: u8 = 0x7;

        fn to_primitive(&self) -> u8 {
            unsafe { std::mem::transmute(*self) }
        }

        fn from_primitive(value: &u8) -> Option<Self> {
            Some(match *value {
                1 => Self::Bold,
                2 => Self::Italic,
                4 => Self::Underline,
                _ => return None,
            })
        }
    }
    #[repr(transparent)]
    pub struct Styles(u8);
    impl Deref for Styles {
        type Target = u8;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }
    impl AsRef<u8> for Styles {
        fn as_ref(&self) -> &u8 {
            &self.0
        }
    }
    impl From<Style> for Styles {
        fn from(value: Style) -> Self {
            unsafe { std::mem::transmute(value) }
        }
    }
    impl FlagValueCollection<Style> for Styles {
        fn to_primitive(&self) -> u8 {
            self.0
        }

        fn from_primitive(value: &u8) -> Self {
            Self(*value)
        }
        fn len(&self) -> u32 {
            self.count_ones()
        }
    }
    impl<T: Into<Styles>> BitOr<T> for Styles {
        type Output = Self;
        fn bitor(self, rhs: T) -> Self::Output {
            Self(self.0.bitor(rhs.into().0))
        }
    }
    impl<T: Into<Styles>> BitOr<T> for Style {
        type Output = Styles;
        fn bitor(self, rhs: T) -> Self::Output {
            Styles(self as u8 | rhs.into().0)
        }
    }

    impl<T: Into<Styles>> BitAnd<T> for Styles {
        type Output = Styles;

        fn bitand(self, rhs: T) -> Self::Output {
            Self(self.0.bitand(rhs.into().0))
        }
    }
    impl<T: Into<Styles>> BitAnd<T> for Style {
        type Output = Styles;
        fn bitand(self, rhs: T) -> Self::Output {
            Styles(self as u8 & rhs.into().0)
        }
    }
    impl Not for Style {
        type Output = Styles;

        fn not(self) -> Self::Output {
            unsafe { std::mem::transmute(!(self as u8) & Self::MASK) }
        }
    }
    impl Not for Styles {
        type Output = Styles;

        fn not(self) -> Self::Output {
            unsafe { std::mem::transmute(!self.0 & Style::MASK) }
        }
    }
    impl Debug for Styles {
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
    impl PartialEq<Style> for Styles {
        fn eq(&self, other: &Style) -> bool {
            self.0 == *other as u8
        }
    }
    impl PartialEq<Styles> for Style {
        fn eq(&self, other: &Styles) -> bool {
            *self as u8 == other.0
        }
    }
    // fn test_not<T: Not<Output = T> + PartialEq + Copy + Debug>(v: T) {
    //     assert_eq!(!(!v), v)
    // }
    #[test]
    fn test() {
        let styles = (Style::Italic | Style::Bold) & Style::Italic;
        println!("{:?}", styles);
    }
}
