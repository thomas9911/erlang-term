use crate::RawTerm;

use keylist::Keylist;
use num_bigint::BigInt;
use std::collections::HashMap;
use std::iter::FromIterator;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
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
    // Keyword(Vec<(String, Term)>),
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
            Atom(x) => Some(x),
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

#[cfg(test)]
mod convert_tests {
    use crate::{from_term, read_binary, RawTerm, Term};
    use keylist::Keylist;
    use num_bigint::BigInt;
    use std::collections::HashMap;

    #[test]
    fn mixed_list() {
        use crate::Term::*;

        let input = read_binary("bins/mixed_list.bin").unwrap();
        let out = from_term(&input).unwrap();

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
        let out = from_term(&input).unwrap();

        assert_eq!(Charlist(vec![1, 2, 3, 4]), Term::from(out));
    }

    #[test]
    fn binary() {
        use crate::Term::*;

        let input = read_binary("bins/binary.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(Bytes(vec![1, 2, 3, 4]), Term::from(out));
    }

    #[test]
    fn keyword() {
        let input = read_binary("bins/keyword.bin").unwrap();
        let out = from_term(&input).unwrap();

        let mut expected = Keylist::new();
        expected.push("just".to_string(), Term::String("some key".to_string()));
        expected.push("other".to_string(), Term::String("value".to_string()));
        expected.push("just".to_string(), Term::Int(1234));

        assert_eq!(Term::Keyword(expected), Term::from(out));
    }

    #[test]
    fn atom_map() {
        let input = read_binary("bins/atom_map.bin").unwrap();
        let out = from_term(&input).unwrap();

        let mut expected = HashMap::new();
        expected.insert("just".to_string(), Term::String("some key".to_string()));
        expected.insert("other".to_string(), Term::String("value".to_string()));

        assert_eq!(Term::Map(expected), Term::from(out));
    }

    #[test]
    fn map() {
        use std::iter::FromIterator;

        let input = read_binary("bins/map.bin").unwrap();
        let out = from_term(&input).unwrap();

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

#[test]
fn serde_test_json_round_trip() {
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
    let new_raw_term = RawTerm::try_from_bytes(&bytes).unwrap();
    let new_term = Term::from(new_raw_term);
    let json = serde_json::to_value(&new_term).unwrap();

    assert_eq!(value, json);
}

#[test]
fn serde_test_elixir_round_trip() {
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
        131, 108, 0, 0, 0, 7, 104, 2, 100, 0, 6, 110, 117, 109, 98, 101, 114, 97, 1, 104, 2, 100,
        0, 5, 102, 108, 111, 97, 116, 70, 64, 9, 30, 184, 81, 235, 133, 31, 104, 2, 100, 0, 4, 108,
        105, 115, 116, 108, 0, 0, 0, 5, 97, 1, 97, 2, 97, 3, 97, 4, 116, 0, 0, 0, 1, 109, 0, 0, 0,
        6, 110, 101, 115, 116, 101, 100, 109, 0, 0, 0, 4, 116, 114, 117, 101, 106, 104, 2, 100, 0,
        3, 109, 97, 112, 116, 0, 0, 0, 1, 109, 0, 0, 0, 4, 116, 101, 115, 116, 98, 0, 1, 226, 64,
        104, 2, 100, 0, 6, 115, 116, 114, 105, 110, 103, 109, 0, 0, 0, 7, 116, 101, 115, 116, 105,
        110, 103, 104, 2, 100, 0, 7, 98, 111, 111, 108, 101, 97, 110, 100, 0, 4, 116, 114, 117,
        101, 104, 2, 100, 0, 4, 110, 111, 110, 101, 100, 0, 3, 110, 105, 108, 106,
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
        131, 108, 0, 0, 0, 7, 104, 2, 109, 0, 0, 0, 6, 110, 117, 109, 98, 101, 114, 97, 1, 104, 2,
        109, 0, 0, 0, 5, 102, 108, 111, 97, 116, 70, 64, 9, 30, 184, 81, 235, 133, 31, 104, 2, 109,
        0, 0, 0, 4, 108, 105, 115, 116, 108, 0, 0, 0, 5, 97, 1, 97, 2, 97, 3, 97, 4, 116, 0, 0, 0,
        1, 109, 0, 0, 0, 6, 110, 101, 115, 116, 101, 100, 109, 0, 0, 0, 4, 116, 114, 117, 101, 106,
        104, 2, 109, 0, 0, 0, 3, 109, 97, 112, 116, 0, 0, 0, 1, 109, 0, 0, 0, 4, 116, 101, 115,
        116, 98, 0, 1, 226, 64, 104, 2, 109, 0, 0, 0, 6, 115, 116, 114, 105, 110, 103, 109, 0, 0,
        0, 7, 116, 101, 115, 116, 105, 110, 103, 104, 2, 109, 0, 0, 0, 7, 98, 111, 111, 108, 101,
        97, 110, 119, 4, 116, 114, 117, 101, 104, 2, 109, 0, 0, 0, 4, 110, 111, 110, 101, 119, 3,
        110, 105, 108, 106,
    ];

    let raw_term = RawTerm::try_from_bytes(&adjusted_input).unwrap();
    let term = Term::from(raw_term);
    let json = serde_json::to_value(&term).unwrap();
    let new_term: Term = serde_json::from_value(json).unwrap();
    let new_raw_term = RawTerm::from(new_term);
    let output = new_raw_term.to_bytes();

    assert_eq!(adjusted_input, output);

    // bonus: the input will be transformed to adjusted input
    let raw_term = RawTerm::try_from_bytes(&input).unwrap();
    let term = Term::from(raw_term);
    let json = serde_json::to_value(&term).unwrap();
    let new_term: Term = serde_json::from_value(json).unwrap();
    let new_raw_term = RawTerm::from(new_term);
    let output = new_raw_term.to_bytes();

    assert_eq!(adjusted_input, output);
}
