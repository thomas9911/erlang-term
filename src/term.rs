use crate::RawTerm;

use multimap::MultiMap;
use num_bigint::BigInt;
use std::collections::HashMap;
use std::iter::FromIterator;

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Byte(u8),
    Int(i32),
    Float(f64),
    Atom(String),
    String(String),
    Bytes(Vec<u8>),
    Bool(bool),
    Nil,
    BigInt(BigInt),
    Charlist(Vec<u8>),
    // Keyword(Vec<(String, Term)>),
    Keyword(MultiMap<String, Term>),
    List(Vec<Term>),
    Tuple(Vec<Term>),
    Map(HashMap<String, Term>),
    MapArbitrary(Vec<(Term, Term)>),
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
                    let map = MultiMap::from_iter(
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

#[cfg(test)]
mod convert_tests {
    use crate::{from_term, read_binary, RawTerm, Term};
    use multimap::MultiMap;
    use num_bigint::BigInt;
    use std::collections::HashMap;

    #[test]
    fn mixed_list() {
        use crate::Term::*;

        let input = read_binary("bins/mixed_list.bin").unwrap();
        let out = from_term(&input).unwrap().pop().unwrap();

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
        let out = from_term(&input).unwrap().pop().unwrap();

        assert_eq!(Charlist(vec![1, 2, 3, 4]), Term::from(out));
    }

    #[test]
    fn binary() {
        use crate::Term::*;

        let input = read_binary("bins/binary.bin").unwrap();
        let out = from_term(&input).unwrap().pop().unwrap();

        assert_eq!(Bytes(vec![1, 2, 3, 4]), Term::from(out));
    }

    #[test]
    fn keyword() {
        let input = read_binary("bins/keyword.bin").unwrap();
        let out = from_term(&input).unwrap().pop().unwrap();

        let mut expected = MultiMap::new();
        expected.insert("just".to_string(), Term::String("some key".to_string()));
        expected.insert("other".to_string(), Term::String("value".to_string()));
        expected.insert("just".to_string(), Term::Int(1234));

        assert_eq!(Term::Keyword(expected), Term::from(out));
    }

    #[test]
    fn atom_map() {
        let input = read_binary("bins/atom_map.bin").unwrap();
        let out = from_term(&input).unwrap().pop().unwrap();

        let mut expected = HashMap::new();
        expected.insert("just".to_string(), Term::String("some key".to_string()));
        expected.insert("other".to_string(), Term::String("value".to_string()));

        assert_eq!(Term::Map(expected), Term::from(out));
    }

    #[test]
    fn map() {
        let input = read_binary("bins/map.bin").unwrap();
        let out = from_term(&input).unwrap().pop().unwrap();

        let mut sub = HashMap::new();
        sub.insert("test".to_string(), Term::Bool(false));
        let mut nested = HashMap::new();
        nested.insert("ok".to_string(), Term::List(Vec::new()));

        let expected = vec![
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
        ];

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
