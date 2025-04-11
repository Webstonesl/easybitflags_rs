pub use flags_derive::derive_flags;
use std::{
    fmt::Debug,
    marker::PhantomData,
    ops::{BitAnd, BitOr, BitXor, Deref, Not, Shl, Shr},
};

/// A trait to define which primitives can be used as a primitive for a flags.
/// Restricted to unsigned types for ease of use.
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
    /// <code>0</code> in this type
    const ZERO: Self;
    /// <code>1</code> in this type
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
/// Trait of the flag enum.
pub trait FlagValue: Sized + Debug + Clone + Copy + AsRef<Self::Collection> {
    /// The innertype in which this element is stored.
    type Bits: FlagPrimitive;
    /// Collection Type associated with Self
    type Collection: FlagValueCollection<Self>;
    /// MASK of all valid bits
    const MASK: Self::Bits;
    fn to_primitive(&self) -> Self::Bits;
    fn from_primitive(value: &Self::Bits) -> Option<Self>;
    fn all() -> Self::Collection {
        Self::Collection::from_primitive(&Self::MASK)
    }
    fn contains<T: Into<Self::Collection>>(&self, needle: T) -> bool {
        let needle: Self::Bits = needle.into().to_primitive();
        let haystack: Self::Bits = self.to_primitive();
        needle & haystack == needle
    }
}
/// Trait of the flag collections
pub trait FlagValueCollection<E: FlagValue>:
    Sized + Deref<Target = E::Bits> + AsRef<E::Bits> + From<E>
{
    fn to_primitive(&self) -> E::Bits;
    fn from_primitive(value: &E::Bits) -> Self;
    fn into_iter(&self) -> FlagIterator<E::Bits, E, Self> {
        FlagIterator::new(self.to_primitive())
    }
    fn len(&self) -> u32;
    fn contains<T: Into<Self>>(&self, needle: T) -> bool {
        let needle: E::Bits = needle.into().to_primitive();
        let haystack: E::Bits = self.to_primitive();
        needle & haystack == needle
    }
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
    use flags_derive::derive_flags;
    derive_flags! {
        #[derive(Debug)]
        #[flag]
        pub enum Flag {
            A = 2,B = 4,C = 8,D = 16
        }
        #[flags]
        pub struct Flags(u8);
    }
    #[test]
    fn test() {
        dbg!(Flag::A | Flag::B);
    }
}
