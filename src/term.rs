use crate::RawTerm;

use keylist::Keylist;
use nom::error::Error;
use nom::Err as NomErr;
use num_bigint::BigInt;
use std::collections::HashMap;
use std::iter::FromIterator;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde_impl", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde_impl", serde(untagged))]
pub enum Term {
    Byte(u8),
    Int(i32),
    Float(f64),
    String(String),
    Atom(String),
    Bytes(Vec<u8>),
    Bool(bool),
    Nil,
    BigInt(BigInt),
    Charlist(Vec<u8>),
    Map(HashMap<String, Term>),
    Keyword(Keylist<String, Term>),
    List(Vec<Term>),
    Tuple(Vec<Term>),
    MapArbitrary(Keylist<Term, Term>),
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
            Binary(x) => {
                if x.iter().all(|x| x > &31) {
                    Term::String(std::string::String::from_utf8(x).expect("invalid utf8"))
                } else {
                    Bytes(x)
                }
            }
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
            RawTerm::Map(x) if term.is_string_map() => {
                let map = HashMap::from_iter(
                    x.into_iter()
                        .map(|(a, b)| (a.as_string_like().unwrap(), Term::from(b))),
                );
                Term::Map(map)
            }
            RawTerm::Map(x) => MapArbitrary(
                x.into_iter()
                    .map(|(a, b)| (Term::from(a), Term::from(b)))
                    .collect(),
            ),
            x => Other(x),
        }
    }
}

fn raw_term_list_to_term_list(raw_list: Vec<RawTerm>) -> Vec<Term> {
    raw_list.into_iter().map(|x| Term::from(x)).collect()
}

fn atom_to_term(atom: String) -> Term {
    match atom.as_ref() {
        "false" => Term::Bool(false),
        "true" => Term::Bool(true),
        "nil" => Term::Nil,
        _ => Term::Atom(atom),
    }
}
impl Term {
    pub fn from_bytes(input: &[u8]) -> Result<Term, NomErr<Error<&[u8]>>> {
        Ok(Term::from(RawTerm::from_bytes(input)?))
    }

    pub fn to_bytes(self) -> Vec<u8> {
        RawTerm::from(self).to_bytes()
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

    pub fn is_tuple(&self) -> bool {
        use Term::*;
        match self {
            Tuple(_) => true,
            _ => false,
        }
    }

    pub fn is_pair_tuple(&self) -> bool {
        use Term::*;
        match self {
            Tuple(x) if x.len() == 2 => true,
            _ => false,
        }
    }

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
            Float(x) => Some(x),
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

    pub fn as_map(self) -> Option<HashMap<String, Term>> {
        use Term::*;
        match self {
            Map(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_map_arbitrary(self) -> Option<Keylist<Term, Term>> {
        use Term::*;
        match self {
            MapArbitrary(x) => Some(x),
            _ => None,
        }
    }
}

macro_rules! impl_from_float {
    ($type: ty) => {
        impl From<$type> for Term {
            fn from(input: $type) -> Term {
                Term::Float(input as f64)
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

impl<T: Into<Term>, V: Into<Term>> From<(T, V)> for Term {
    fn from(input: (T, V)) -> Term {
        let t = input.0.into();
        let v = input.1.into();
        Term::Tuple(vec![t, v])
    }
}

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
            Term::MapArbitrary(Keylist::from_iter(
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

impl<K: ToString, V: Into<Term>> From<HashMap<K, V>> for Term {
    fn from(input: HashMap<K, V>) -> Term {
        Term::Map(HashMap::from_iter(
            input.into_iter().map(|(k, v)| (k.to_string(), v.into())),
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
        expected.insert("just".to_string(), Term::String("some key".to_string()));
        expected.insert("other".to_string(), Term::String("value".to_string()));

        assert_eq!(Term::Map(expected), Term::from(out));
    }

    #[test]
    fn map() {
        use std::iter::FromIterator;

        let input = read_binary("bins/map.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        let mut sub = HashMap::new();
        sub.insert("test".to_string(), Term::Bool(false));
        let mut nested = HashMap::new();
        nested.insert("ok".to_string(), Term::List(Vec::new()));

        let expected = Keylist::from_iter(vec![
            (Term::Byte(1), Term::String("one".to_string())),
            (
                Term::Atom("tuple".to_string()),
                Term::Tuple(vec![Term::Byte(1), Term::Atom("more".to_string())]),
            ),
            (
                Term::List(vec![Term::String("list as a key".to_string())]),
                Term::List(vec![Term::String("another".to_string()), Term::Map(sub)]),
            ),
            (Term::String("float".to_string()), Term::Float(3.14)),
            (
                Term::String("large".to_string()),
                Term::BigInt(BigInt::parse_bytes(b"123456789123456789", 10).unwrap()),
            ),
            (Term::String("nested".to_string()), Term::Map(nested)),
        ]);

        assert_eq!(Term::MapArbitrary(expected), Term::from(out));
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
        let expected = Term::MapArbitrary(Keylist::from(vec![
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
                (String::from("1"), "test".into()),
                (String::from("5"), "testing".into())
            ])),
            map.into()
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
