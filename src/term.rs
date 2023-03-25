use crate::raw_term::{RawTermGeneralType, RawTermType};
use crate::RawTerm;

use keylist::Keylist;
use nom::error::Error;
use nom::Err as NomErr;
use num_bigint::BigInt;
use ordered_float::OrderedFloat;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::iter::FromIterator;

mod improper_list;
mod ord;

pub use improper_list::ImproperList;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde_impl", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde_impl", serde(untagged))]
/// Higher level Elixir types
///
/// These can be created by using the `Term::from` methods or creating them directly.
///
/// ```rust
/// # use erlang_term::Term;
///
/// let string = Term::from("ok");
/// let atom = Term::Atom("ok".to_string());
///
/// assert!(string.is_string());
/// assert!(atom.is_atom());
/// assert_ne!(string, atom);
/// ```
///
/// The `Term::as_*` will unwrap the Term into a specific type if the Term is that type
///
/// ```rust
/// # use erlang_term::Term;
///
/// let atom = Term::Atom("ok".to_string());
/// assert_eq!(Some("ok".to_string()), atom.as_atom());
///
/// let atom = Term::Atom("ok".to_string());
/// assert_eq!(None, atom.as_string());
/// ```
///
/// Convert to RawTerm
///
/// ```rust
/// # use erlang_term::{Term, RawTerm};
///
/// let term = Term::from("testing");
/// let raw_term = RawTerm::from(term);
///
/// assert_eq!(RawTerm::Binary(vec![116, 101, 115, 116, 105, 110, 103]), raw_term);
/// ```
///
/// Convert from RawTerm
///
/// ```rust
/// # use erlang_term::{Term, RawTerm};
///
/// let raw_term = RawTerm::Binary(vec![116, 101, 115, 116, 105, 110, 103]);
/// let term = Term::from(raw_term);
///
/// assert_eq!(Some("testing".to_string()), term.as_string());
/// ```
///
pub enum Term {
    Byte(u8),
    Int(i32),
    Float(OrderedFloat<f64>),
    String(String),
    Atom(String),
    Bytes(Vec<u8>),
    Bool(bool),
    Nil,
    BigInt(BigInt),
    Charlist(Vec<u8>),
    Map(HashMap<Term, Term>),
    Keyword(Keylist<String, Term>),
    List(Vec<Term>),
    Tuple(Vec<Term>),
    Other(RawTerm),
}

// elixir formats lists with numbers below 32 as lists otherwise as charlists
// elixir formats binaries with numbers below 32 as lists otherwise as string

impl From<RawTerm> for Term {
    fn from(term: RawTerm) -> Self {
        use RawTerm::*;
        use Term::*;
        match term {
            SmallInt(x) => Byte(x),
            RawTerm::Int(x) => Term::Int(x),
            RawTerm::Float(x) => Term::Float(x),
            Binary(x) if is_string_printable(&x) => match std::str::from_utf8(&x) {
                Ok(s) => Term::String(s.to_string()),
                Err(_) => Bytes(x),
            },
            Binary(x) => Bytes(x),
            RawTerm::String(x) => Charlist(x),
            SmallBigInt(x) => BigInt(x),
            LargeBigInt(x) => BigInt(x),
            RawTerm::List(x) => {
                if x.iter().all(|x| x.is_atom_pair()) {
                    let map = Keylist::from_iter(
                        x.into_iter()
                            .map(|x| x.as_atom_pair().unwrap())
                            .map(|(a, b)| (a, Term::from(b))),
                    );
                    Term::Keyword(map)
                } else {
                    Term::List(raw_term_list_to_term_list(x))
                }
            }
            RawTerm::Nil => Term::List(Vec::new()),
            SmallTuple(x) => Tuple(raw_term_list_to_term_list(x)),
            LargeTuple(x) => Tuple(raw_term_list_to_term_list(x)),
            AtomDeprecated(x) => atom_to_term(x),
            SmallAtomDeprecated(x) => atom_to_term(x),
            SmallAtom(x) => atom_to_term(x),
            RawTerm::Atom(x) => atom_to_term(x),
            RawTerm::Map(x) => Term::Map(
                x.into_iter()
                    .map(|(a, b)| (Term::from(a), Term::from(b)))
                    .collect(),
            ),
            x => Other(x),
        }
    }
}

impl Hash for Term {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        use Term::*;

        match self {
            Byte(x) => x.hash(state),
            Int(x) => x.hash(state),
            Float(x) => x.hash(state),
            String(x) => x.hash(state),
            Atom(x) => x.hash(state),
            Bytes(x) => x.hash(state),
            Bool(x) => x.hash(state),
            Nil => ().hash(state),
            BigInt(x) => x.hash(state),
            Charlist(x) => x.hash(state),
            Map(x) => {
                // copied from https://github.com/rust-lang/rust/pull/48366
                state.write_u64(
                    x.iter()
                        .map(|kv| {
                            let mut h = std::collections::hash_map::DefaultHasher::new();
                            kv.hash(&mut h);
                            h.finish()
                        })
                        .fold(0, u64::wrapping_add),
                )
            }
            Keyword(x) => x.hash(state),
            List(x) => x.hash(state),
            Tuple(x) => x.hash(state),
            Other(x) => x.hash(state),
        }
    }
}

fn is_string_printable(binary: &[u8]) -> bool {
    binary.iter().all(is_string_printable_byte)
}

fn is_string_printable_byte(byte: &u8) -> bool {
    // elixir 0xA0..0xD7FF
    // let the String::from_utf8 check if it is a valid utf8 string
    if byte >= &0xA0 {
        return true;
    }

    // '\n\r\t\v\b\f\e\d\a'
    if [10, 13, 9, 11, 8, 12, 27, 127, 7].contains(byte) {
        return true;
    }
    // elixir 0x20..0x7E
    if (0x20..=0x7E).contains(byte) {
        return true;
    }

    false
}

fn raw_term_list_to_term_list(raw_list: Vec<RawTerm>) -> Vec<Term> {
    raw_list.into_iter().map(Term::from).collect()
}

fn atom_to_term(atom: String) -> Term {
    match atom.as_ref() {
        "false" => Term::Bool(false),
        "true" => Term::Bool(true),
        "nil" => Term::Nil,
        _ => Term::Atom(atom),
    }
}

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", print_elixir_term(self))
    }
}

pub fn print_elixir_term(term: &Term) -> String {
    use Term::*;

    match term {
        Bool(b) => b.to_string(),
        Nil => "nil".to_string(),
        String(s) => format!("{:?}", s),
        Atom(a) => format_atom(a),
        Byte(b) => b.to_string(),
        Bytes(b) => {
            let bytes: Vec<_> = b.iter().map(|x| x.to_string()).collect();
            let mut inner = bytes.join(", ");
            inner.insert_str(0, "<<");
            inner.push_str(">>");
            inner
        }
        Charlist(c) => {
            format!("{:?}", c)
        }
        Int(i) => i.to_string(),
        Float(f) => f.to_string(),
        BigInt(b) => b.to_string(),
        Keyword(k) => {
            let list: Vec<_> = k
                .iter()
                .map(|(k, v)| {
                    let mut a = format_atom(k);
                    a = a.trim_start_matches(':').to_string();
                    format!("{}: {}", a, print_elixir_term(v))
                })
                .collect();
            let mut inner = list.join(", ");
            inner.insert(0, '[');
            inner.push(']');
            inner
        }
        List(l) => {
            let list: Vec<_> = l.iter().map(print_elixir_term).collect();
            let mut inner = list.join(", ");
            inner.insert(0, '[');
            inner.push(']');
            inner
        }
        Tuple(t) => {
            let list: Vec<_> = t.iter().map(print_elixir_term).collect();
            let mut inner = list.join(", ");
            inner.insert(0, '{');
            inner.push('}');
            inner
        }
        Map(m) => {
            let list: Vec<_> = m
                .iter()
                .map(|(k, v)| format!("{} => {}", print_elixir_term(k), print_elixir_term(v)))
                .collect();
            let mut inner = list.join(", ");
            inner.insert_str(0, "%{");
            inner.push('}');
            inner
        }
        other => format!("#{:?}", other),
    }
}

fn format_atom(a: &str) -> String {
    if a.is_empty() {
        return String::from(r#":"""#);
    }
    if a.chars().all(|x| x.is_ascii_alphanumeric()) {
        if a.chars().next().unwrap().is_ascii_uppercase() {
            return a.to_string();
        }
        if !a.chars().next().unwrap().is_ascii_digit() {
            return format!(":{}", a);
        }
    }
    format!(r#":"{}""#, a)
}

impl Term {
    pub fn from_bytes(input: &[u8]) -> Result<Term, NomErr<Error<&[u8]>>> {
        Ok(Term::from(RawTerm::from_bytes(input)?))
    }

    pub fn to_bytes(self) -> Vec<u8> {
        RawTerm::from(self).to_bytes()
    }

    #[cfg(feature = "zlib")]
    pub fn to_gzip_bytes(self, level: flate2::Compression) -> std::io::Result<Vec<u8>> {
        RawTerm::from(self).to_gzip_bytes(level)
    }

    pub fn as_type(&self) -> RawTermType {
        match self {
            Term::Byte(_) => RawTermType::SmallInt,
            Term::Int(_) => RawTermType::Int,
            Term::Float(_) => RawTermType::Float,
            Term::String(_) => RawTermType::Binary,
            Term::Atom(_) => RawTermType::Atom,
            Term::Bytes(_) => RawTermType::Binary,
            Term::Bool(_) => RawTermType::SmallAtom,
            Term::Nil => RawTermType::SmallAtom,
            Term::BigInt(_) => RawTermType::LargeBigInt,
            Term::Charlist(_) => RawTermType::String,
            Term::Map(_) => RawTermType::Map,
            Term::Keyword(_) => RawTermType::List,
            Term::List(_) => RawTermType::List,
            Term::Tuple(_) => RawTermType::LargeTuple,
            Term::Other(x) => x.as_type(),
        }
    }

    pub fn as_general_type(&self) -> RawTermGeneralType {
        RawTermGeneralType::from(self.as_type())
    }

    pub fn is_byte(&self) -> bool {
        use Term::*;
        match self {
            Byte(_) => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        use Term::*;
        match self {
            String(_) => true,
            _ => false,
        }
    }

    pub fn is_atom(&self) -> bool {
        use Term::*;
        match self {
            Atom(_) => true,
            _ => false,
        }
    }

    pub fn is_tuple(&self) -> bool {
        use Term::*;
        match self {
            Tuple(_) => true,
            _ => false,
        }
    }

    ///
    /// Check if the Term is a tuple of length 2
    ///
    /// ```rust
    /// # use erlang_term::Term;
    /// let term = Term::from((1, 2));
    /// assert!(term.is_pair_tuple());
    /// ```
    pub fn is_pair_tuple(&self) -> bool {
        use Term::*;
        match self {
            Tuple(x) if x.len() == 2 => true,
            _ => false,
        }
    }

    pub fn is_list(&self) -> bool {
        use Term::*;
        match self {
            List(_) => true,
            _ => false,
        }
    }

    ///
    /// Check if the Term is of the form `("string", any)`
    ///
    /// ```rust
    /// # use erlang_term::Term;
    /// let term = Term::from(("test", 1));
    /// assert!(term.is_string_tuple_pair());
    /// let term = Term::from((1, 2));
    /// assert!(!term.is_string_tuple_pair());
    /// ```
    pub fn is_string_tuple_pair(&self) -> bool {
        use Term::*;
        match self {
            Tuple(x) if (x.len() == 2) & x[0].is_string() => true,
            _ => false,
        }
    }

    pub fn as_bool(self) -> Option<bool> {
        use Term::*;
        match self {
            Bool(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_nil(self) -> Option<()> {
        use Term::*;
        match self {
            Nil => Some(()),
            _ => None,
        }
    }

    pub fn as_byte(self) -> Option<u8> {
        use Term::*;
        match self {
            Byte(x) => Some(x),
            _ => None,
        }
    }
    pub fn as_int(self) -> Option<i32> {
        use Term::*;
        match self {
            Int(x) => Some(x),
            _ => None,
        }
    }
    pub fn as_float(self) -> Option<f64> {
        use Term::*;
        match self {
            Float(x) => Some(*x),
            _ => None,
        }
    }
    pub fn as_atom(self) -> Option<String> {
        use Term::*;
        match self {
            Atom(x) => Some(x),
            _ => None,
        }
    }
    pub fn as_string(self) -> Option<String> {
        use Term::*;
        match self {
            String(x) => Some(x),
            _ => None,
        }
    }
    pub fn as_bytes(self) -> Option<Vec<u8>> {
        use Term::*;
        match self {
            Bytes(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_charlist(self) -> Option<Vec<u8>> {
        use Term::*;
        match self {
            Charlist(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_big_int(self) -> Option<BigInt> {
        use Term::*;
        match self {
            BigInt(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_keyword(self) -> Option<Keylist<String, Term>> {
        use Term::*;
        match self {
            Keyword(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_list(self) -> Option<Vec<Term>> {
        use Term::*;
        match self {
            List(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_tuple(self) -> Option<Vec<Term>> {
        use Term::*;
        match self {
            Tuple(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_map(self) -> Option<HashMap<Term, Term>> {
        use Term::*;
        match self {
            Map(x) => Some(x),
            _ => None,
        }
    }

    ///
    /// Unwrap and convert the term into a `HashMap<String, Term>` if the term is a map and all the keys are strings
    /// Note that this will create a new map, so for one-offs you should use the `Term::as_map` function
    ///
    pub fn as_string_map(self) -> Option<HashMap<String, Term>> {
        use Term::*;
        match self {
            Map(x) if x.keys().all(|y| y.is_string()) => {
                let new_map = HashMap::from_iter(
                    x.into_iter()
                        .map(|(k, v)| (k.as_string().expect("checked this in the match"), v)),
                );
                Some(new_map)
            }
            _ => None,
        }
    }

    ///
    /// Unwrap and convert the term into a `HashMap<String, Term>` if the term is a map and all the keys are atoms
    /// Note that this will create a new map, so for one-offs you should use the `Term::as_map` function
    ///
    pub fn as_atom_map(self) -> Option<HashMap<String, Term>> {
        use Term::*;
        match self {
            Map(x) if x.keys().all(|y| y.is_atom()) => {
                let new_map = HashMap::from_iter(
                    x.into_iter()
                        .map(|(k, v)| (k.as_atom().expect("checked this in the match"), v)),
                );
                Some(new_map)
            }
            _ => None,
        }
    }
}

macro_rules! impl_from_float {
    ($type: ty) => {
        impl From<$type> for Term {
            fn from(input: $type) -> Term {
                Term::Float((input as f64).into())
            }
        }
    };
}

macro_rules! impl_from_integer {
    ($type: ty) => {
        impl From<$type> for Term {
            fn from(input: $type) -> Term {
                Term::Int(input as i32)
            }
        }
    };
}

macro_rules! impl_from_big_integer {
    ($type: ty) => {
        impl From<$type> for Term {
            fn from(input: $type) -> Term {
                Term::BigInt(BigInt::from(input))
            }
        }
    };
}

impl From<bool> for Term {
    fn from(input: bool) -> Term {
        Term::Bool(input)
    }
}

impl From<u8> for Term {
    fn from(input: u8) -> Term {
        Term::Byte(input)
    }
}

impl From<()> for Term {
    fn from(_input: ()) -> Term {
        Term::Nil
    }
}

impl From<String> for Term {
    fn from(input: String) -> Term {
        Term::String(input)
    }
}

impl From<&str> for Term {
    fn from(input: &str) -> Term {
        Term::String(String::from(input))
    }
}

macro_rules! impl_tuple {
    ($($idx:tt $t:tt),+) => {
        impl<$($t,)+> From<($($t,)+)> for Term
        where
            $($t: Into<Term>,)+
        {
            fn from(input: ($($t,)+)) -> Self {
                Term::Tuple(vec!($(
                   input.$idx.into(),
                )+))
            }
        }
    };
}

impl_tuple!(0 A);
impl_tuple!(0 A, 1 B);
impl_tuple!(0 A, 1 B, 2 C);
impl_tuple!(0 A, 1 B, 2 C, 3 D);
impl_tuple!(0 A, 1 B, 2 C, 3 D, 4 E);
impl_tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F);
impl_tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G);
impl_tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H);
impl_tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I);
impl_tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I, 9 J);
impl_tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I, 9 J, 10 K);
impl_tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I, 9 J, 10 K, 11 L);
impl_tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I, 9 J, 10 K, 11 L, 12 M);
impl_tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I, 9 J, 10 K, 11 L, 12 M, 13 N);
impl_tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I, 9 J, 10 K, 11 L, 12 M, 13 N, 14 O);
impl_tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I, 9 J, 10 K, 11 L, 12 M, 13 N, 14 O, 15 P);

impl<T: Into<Term>> From<Vec<T>> for Term {
    fn from(input: Vec<T>) -> Term {
        let data: Vec<Term> = input.into_iter().map(|x| x.into()).collect();
        if data.iter().all(|x| x.is_byte()) {
            Term::Bytes(data.into_iter().map(|x| x.as_byte().unwrap()).collect())
        } else if data.iter().all(|x| x.is_string_tuple_pair()) {
            Term::Keyword(Keylist::from_iter(
                data.into_iter()
                    .map(|x| x.as_tuple().unwrap())
                    .map(|mut x| {
                        let second = x.pop().unwrap();
                        let first = x.pop().unwrap();
                        (first, second)
                    })
                    .map(|(a, b)| (a.as_string().unwrap(), b)),
            ))
        } else if data.iter().all(|x| x.is_pair_tuple()) {
            Term::Map(HashMap::from_iter(
                data.into_iter()
                    .map(|x| x.as_tuple().unwrap())
                    .map(|mut x| {
                        let second = x.pop().unwrap();
                        let first = x.pop().unwrap();
                        (first, second)
                    }),
            ))
        } else {
            Term::List(data)
        }
    }
}

impl<K: Into<Term>, V: Into<Term>> From<HashMap<K, V>> for Term {
    fn from(input: HashMap<K, V>) -> Term {
        Term::Map(HashMap::from_iter(
            input.into_iter().map(|(k, v)| (k.into(), v.into())),
        ))
    }
}

impl_from_float!(f32);
impl_from_float!(f64);
impl_from_integer!(i8);
impl_from_integer!(i16);
impl_from_integer!(u16);
impl_from_integer!(i32);
impl_from_big_integer!(usize);
impl_from_big_integer!(u32);
impl_from_big_integer!(i64);
impl_from_big_integer!(u64);
impl_from_big_integer!(i128);
impl_from_big_integer!(u128);

#[test]
fn is_string_printable_test() {
    let data = &[7, 33, 125];
    assert!(is_string_printable(data));
    assert!(std::str::from_utf8(data).is_ok());

    let data = &[194, 160];
    assert!(is_string_printable(data));
    assert!(std::str::from_utf8(data).is_ok());

    // utf8 but still invalid according this function
    let data = &[1, 2, 3, 4];
    assert!(!is_string_printable(data));
    assert!(std::str::from_utf8(data).is_ok());

    // not utf8 but still valid according this function
    let data = &[202, 218, 82, 75, 227, 11, 203, 41, 103, 208, 244, 215];
    assert!(is_string_printable(data));
    assert!(std::str::from_utf8(data).is_err())
}

#[cfg(test)]
mod convert_tests {
    use crate::{from_bytes, read_binary, RawTerm, Term};
    use keylist::Keylist;
    use num_bigint::BigInt;
    use std::collections::HashMap;

    #[test]
    fn mixed_list() {
        use crate::Term::*;

        let input = read_binary("bins/mixed_list.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(
            List(vec![
                Byte(1),
                String("some".to_string()),
                Byte(2),
                String("text".to_string())
            ]),
            Term::from(out)
        );
    }

    #[test]
    fn number_list() {
        use crate::Term::*;

        let input = read_binary("bins/number_list.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(Charlist(vec![1, 2, 3, 4]), Term::from(out));
    }

    #[test]
    fn binary() {
        use crate::Term::*;

        let input = read_binary("bins/binary.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(Bytes(vec![1, 2, 3, 4]), Term::from(out));
    }

    #[test]
    fn binary_non_utf8() {
        use crate::Term::*;

        let input = read_binary("bins/non_utf8_string.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(
            Bytes(vec![
                143, 45, 211, 57, 243, 220, 73, 235, 239, 201, 232, 189, 101
            ]),
            Term::from(out)
        );
    }

    #[test]
    fn binary_utf8() {
        use crate::Term::*;

        let input = read_binary("bins/small_string.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(
            String(std::string::String::from("just some text")),
            Term::from(out)
        );
    }

    #[test]
    fn keyword() {
        let input = read_binary("bins/keyword.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        let mut expected = Keylist::new();
        expected.push("just".to_string(), Term::String("some key".to_string()));
        expected.push("other".to_string(), Term::String("value".to_string()));
        expected.push("just".to_string(), Term::Int(1234));

        assert_eq!(Term::Keyword(expected), Term::from(out));
    }

    #[test]
    fn atom_map() {
        let input = read_binary("bins/atom_map.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        let mut expected = HashMap::new();
        expected.insert(
            Term::Atom("just".to_string()),
            Term::String("some key".to_string()),
        );
        expected.insert(
            Term::Atom("other".to_string()),
            Term::String("value".to_string()),
        );

        assert_eq!(Term::Map(expected), Term::from(out));
    }

    #[test]
    fn map() {
        use std::iter::FromIterator;

        let input = read_binary("bins/map.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        let mut sub = HashMap::new();
        sub.insert(Term::Atom("test".to_string()), Term::Bool(false));
        let mut nested = HashMap::new();
        nested.insert(Term::String("ok".to_string()), Term::List(Vec::new()));

        let expected = HashMap::from_iter(vec![
            (Term::Byte(1), Term::String("one".to_string())),
            (
                Term::Atom("tuple".to_string()),
                Term::Tuple(vec![Term::Byte(1), Term::Atom("more".to_string())]),
            ),
            (
                Term::List(vec![Term::String("list as a key".to_string())]),
                Term::List(vec![Term::String("another".to_string()), Term::Map(sub)]),
            ),
            (Term::String("float".to_string()), Term::Float(3.14.into())),
            (
                Term::String("large".to_string()),
                Term::BigInt(BigInt::parse_bytes(b"123456789123456789", 10).unwrap()),
            ),
            (Term::String("nested".to_string()), Term::Map(nested)),
        ]);

        assert_eq!(Term::Map(expected), Term::from(out));
    }

    #[test]
    fn nil() {
        let out = RawTerm::Atom("nil".to_string());
        assert_eq!(Term::Nil, Term::from(out));
    }

    #[test]
    fn false_test() {
        let out = RawTerm::Atom("false".to_string());
        assert_eq!(Term::Bool(false), Term::from(out));
    }

    #[test]
    fn true_test() {
        let out = RawTerm::Atom("true".to_string());
        assert_eq!(Term::Bool(true), Term::from(out));
    }
}

#[cfg(test)]
mod from_tests {
    use crate::Term;
    use keylist::Keylist;
    use num_bigint::BigInt;
    use std::collections::HashMap;
    use std::iter::FromIterator;

    #[test]
    fn from_i32() {
        assert_eq!(Term::Int(12345), 12345i32.into())
    }

    #[test]
    fn from_u8() {
        assert_eq!(Term::Byte(3), 3u8.into())
    }

    #[test]
    fn from_u32() {
        assert_eq!(Term::BigInt(BigInt::from(12345)), 12345u32.into())
    }

    #[test]
    fn from_i128() {
        assert_eq!(
            Term::BigInt(BigInt::from(1111111111112345i128)),
            1111111111112345i128.into()
        )
    }

    #[test]
    fn from_str() {
        assert_eq!(Term::String(String::from("test")), "test".into())
    }

    #[test]
    fn from_string() {
        assert_eq!(
            Term::String(String::from("test")),
            String::from("test").into()
        )
    }

    #[test]
    fn from_list_to_keyword() {
        let input = vec![("test", 12), ("testing", 129)];
        let expected = Term::Keyword(Keylist::from(vec![
            ("test".to_string(), 12.into()),
            ("testing".to_string(), 129.into()),
        ]));
        assert_eq!(expected, input.into())
    }

    #[test]
    fn from_list_keyword_to_map() {
        use Term::*;
        let input = vec![(vec!["test"], 12), (vec!["testing"], 129)];
        let expected = Term::Map(HashMap::from_iter(vec![
            (List(vec!["test".into()]), 12.into()),
            (List(vec!["testing".into()]), 129.into()),
        ]));
        assert_eq!(expected, input.into())
    }

    #[test]
    fn from_list_nested() {
        use Term::*;
        let input = vec![vec![12], vec![1234]];
        let expected = Term::List(vec![List(vec![12.into()]), List(vec![1234.into()])]);
        assert_eq!(expected, input.into())
    }

    #[test]
    fn from_list() {
        let input = vec!["test", "testing", "more", "another"];
        let expected = Term::List(vec![
            "test".into(),
            "testing".into(),
            "more".into(),
            "another".into(),
        ]);
        assert_eq!(expected, input.into())
    }

    #[test]
    fn from_map() {
        use std::collections::HashMap;
        use std::iter::FromIterator;

        let map = HashMap::from_iter(vec![(1u8, "test"), (5u8, "testing")]);

        assert_eq!(
            Term::Map(HashMap::from_iter(vec![
                (1u8.into(), "test".into()),
                (5u8.into(), "testing".into())
            ])),
            map.into()
        );
    }

    #[test]
    fn from_tuple_1() {
        let expected = Term::Tuple(vec![0.into()]);

        assert_eq!(expected, (0,).into())
    }

    #[test]
    fn from_tuple_2() {
        let expected = Term::Tuple(vec![0.into(), 1.into()]);

        assert_eq!(expected, (0, 1).into())
    }

    #[test]
    fn from_tuple_16() {
        let expected = Term::Tuple((0..16).map(Term::from).collect());

        assert_eq!(
            expected,
            (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15).into()
        )
    }
}

#[cfg(test)]
mod print {
    use super::print_elixir_term;
    use crate::{RawTerm, Term};
    use keylist::Keylist;
    use num_bigint::BigInt;
    use std::collections::HashMap;
    use std::iter::FromIterator;

    #[test]
    fn display() {
        let keylist: Keylist<String, Term> = Keylist::from_iter(vec![
            ("test".into(), 1.into()),
            ("Test".into(), 2.into()),
            ("1234Test".into(), 3.into()),
        ]);
        assert_eq!(
            r#"[test: 1, Test: 2, "1234Test": 3]"#,
            Term::Keyword(keylist).to_string()
        );
    }

    #[test]
    fn elixir_term() {
        assert_eq!(
            "\"testing\"",
            print_elixir_term(&Term::String(String::from("testing")))
        );
        assert_eq!("123", print_elixir_term(&Term::Byte(123)));
        assert_eq!("nil", print_elixir_term(&Term::Nil));
        assert_eq!(
            ":testing",
            print_elixir_term(&Term::Atom(String::from("testing")))
        );
        assert_eq!(
            "NiceModule",
            print_elixir_term(&Term::Atom(String::from("NiceModule")))
        );
        assert_eq!(
            ":\":D:D\"",
            print_elixir_term(&Term::Atom(String::from(":D:D")))
        );
        assert_eq!(
            ":\"1234testing\"",
            print_elixir_term(&Term::Atom(String::from("1234testing")))
        );
        assert_eq!(
            "<<1, 2, 3, 4, 5, 6, 7>>",
            print_elixir_term(&Term::Bytes(vec![1, 2, 3, 4, 5, 6, 7]))
        );
        assert_eq!(
            "[1, 2, 3, 4, 5, 6, 7]",
            print_elixir_term(&Term::Charlist(vec![1, 2, 3, 4, 5, 6, 7]))
        );
        assert_eq!("3.123124123123123", print_elixir_term(&Term::Float(3.123_124_123_123_123.into())));
        assert_eq!("123456789123456789123456789123456789123456789123456789123456789123456789123456789123456789123456789123456789", print_elixir_term(&Term::BigInt(BigInt::parse_bytes(b"123456789123456789123456789123456789123456789123456789123456789123456789123456789123456789123456789123456789", 10).unwrap())));
        let keylist: Keylist<String, Term> = Keylist::from_iter(vec![
            ("test".into(), 1.into()),
            ("Test".into(), 2.into()),
            ("1234Test".into(), 3.into()),
        ]);
        assert_eq!(
            r#"[test: 1, Test: 2, "1234Test": 3]"#,
            print_elixir_term(&Term::Keyword(keylist))
        );
        let list = vec![
            "hallo".into(),
            Term::Byte(123),
            Term::Nil,
            Term::Bytes(vec![1, 2, 3, 4, 5, 6, 7]),
        ];
        assert_eq!(
            r#"["hallo", 123, nil, <<1, 2, 3, 4, 5, 6, 7>>]"#,
            print_elixir_term(&Term::List(list))
        );
        let list = vec![
            "hallo".into(),
            Term::Byte(123),
            Term::Nil,
            Term::Bytes(vec![1, 2, 3, 4, 5, 6, 7]),
        ];
        assert_eq!(
            r#"{"hallo", 123, nil, <<1, 2, 3, 4, 5, 6, 7>>}"#,
            print_elixir_term(&Term::Tuple(list))
        );
        let map: HashMap<Term, Term> = HashMap::from_iter(vec![
            ("test".into(), 1.into()),
            ("Test".into(), 2.into()),
            ("1234Test".into(), 3.into()),
            ("testing testing".into(), 4.into()),
        ]);
        let map_text = print_elixir_term(&Term::Map(map));
        assert!(map_text.contains("Test"));
        assert!(map_text.contains("%{"));
        assert!(map_text.contains(r#""1234Test" => 3"#));

        let map = HashMap::from_iter(vec![
            (Term::List(vec!["test".into()]), 12.into()),
            (Term::List(vec!["testing".into()]), 129.into()),
        ]);

        let map_text = print_elixir_term(&Term::Map(map));
        assert!(map_text.contains(r#"["test"] => 12"#));
        assert!(map_text.contains(r#"["testing"] => 129"#));
        assert!(map_text.contains("%{"));

        assert_eq!(
            "#Other(Int(-123))",
            print_elixir_term(&Term::Other(RawTerm::Int(-123)))
        );
    }
}

#[cfg(all(test, feature = "serde_impl"))]
mod serde_tests {
    use crate::{RawTerm, Term};
    #[test]
    fn json_round_trip() {
        let input = r#"
        {
            "number": 1,
            "float": 3.14,
            "list": [1,2,3,4, {"nested": "true"}],
            "map": {
                "test": 123456
            },
            "string": "testing",
            "boolean": true,
            "none": null
        }
        "#;
        let value: serde_json::Value = serde_json::from_str(input).unwrap();
        let term: Term = serde_json::from_value(value.clone()).unwrap();
        let raw_term = RawTerm::from(term);
        let bytes = raw_term.to_bytes();
        let new_raw_term = RawTerm::from_bytes(&bytes).unwrap();
        let new_term = Term::from(new_raw_term);
        let json = serde_json::to_value(&new_term).unwrap();

        assert_eq!(value, json);
    }

    #[test]
    fn elixir_round_trip() {
        // r#"
        // [
        //     number: 1,
        //     float: 3.14,
        //     list: [1,2,3,4, %{"nested" => "true"}],
        //     map: %{
        //         "test" => 123456
        //     },
        //     string: "testing",
        //     boolean: true,
        //     none: nil
        // ]
        // "#;

        let input = vec![
            131, 108, 0, 0, 0, 7, 104, 2, 100, 0, 6, 110, 117, 109, 98, 101, 114, 97, 1, 104, 2,
            100, 0, 5, 102, 108, 111, 97, 116, 70, 64, 9, 30, 184, 81, 235, 133, 31, 104, 2, 100,
            0, 4, 108, 105, 115, 116, 108, 0, 0, 0, 5, 97, 1, 97, 2, 97, 3, 97, 4, 116, 0, 0, 0, 1,
            109, 0, 0, 0, 6, 110, 101, 115, 116, 101, 100, 109, 0, 0, 0, 4, 116, 114, 117, 101,
            106, 104, 2, 100, 0, 3, 109, 97, 112, 116, 0, 0, 0, 1, 109, 0, 0, 0, 4, 116, 101, 115,
            116, 98, 0, 1, 226, 64, 104, 2, 100, 0, 6, 115, 116, 114, 105, 110, 103, 109, 0, 0, 0,
            7, 116, 101, 115, 116, 105, 110, 103, 104, 2, 100, 0, 7, 98, 111, 111, 108, 101, 97,
            110, 100, 0, 4, 116, 114, 117, 101, 104, 2, 100, 0, 4, 110, 111, 110, 101, 100, 0, 3,
            110, 105, 108, 106,
        ];

        // because deserialized values will never be atoms:
        // r#"
        // [
        //     {"number", 1},
        //     {"float", 3.14},
        //     {"list", [1, 2, 3, 4, %{"nested" => "true"}]},
        //     {"map", %{"test" => 123456}},
        //     {"string", "testing"},
        //     {"boolean", true},
        //     {"none", nil}
        // ]
        // "#;

        let adjusted_input = vec![
            131, 108, 0, 0, 0, 7, 104, 2, 109, 0, 0, 0, 6, 110, 117, 109, 98, 101, 114, 97, 1, 104,
            2, 109, 0, 0, 0, 5, 102, 108, 111, 97, 116, 70, 64, 9, 30, 184, 81, 235, 133, 31, 104,
            2, 109, 0, 0, 0, 4, 108, 105, 115, 116, 108, 0, 0, 0, 5, 97, 1, 97, 2, 97, 3, 97, 4,
            116, 0, 0, 0, 1, 109, 0, 0, 0, 6, 110, 101, 115, 116, 101, 100, 109, 0, 0, 0, 4, 116,
            114, 117, 101, 106, 104, 2, 109, 0, 0, 0, 3, 109, 97, 112, 116, 0, 0, 0, 1, 109, 0, 0,
            0, 4, 116, 101, 115, 116, 98, 0, 1, 226, 64, 104, 2, 109, 0, 0, 0, 6, 115, 116, 114,
            105, 110, 103, 109, 0, 0, 0, 7, 116, 101, 115, 116, 105, 110, 103, 104, 2, 109, 0, 0,
            0, 7, 98, 111, 111, 108, 101, 97, 110, 119, 4, 116, 114, 117, 101, 104, 2, 109, 0, 0,
            0, 4, 110, 111, 110, 101, 119, 3, 110, 105, 108, 106,
        ];

        let term = Term::from_bytes(&adjusted_input).unwrap();
        let json = serde_json::to_value(&term).unwrap();
        let new_term: Term = serde_json::from_value(json).unwrap();
        let output = new_term.to_bytes();

        assert_eq!(adjusted_input, output);

        // bonus: the input will be transformed to adjusted input
        let term = Term::from_bytes(&input).unwrap();
        let json = serde_json::to_value(&term).unwrap();
        let new_term: Term = serde_json::from_value(json).unwrap();
        let output = new_term.to_bytes();

        assert_eq!(adjusted_input, output);
    }
}
