use crate::Term;
use keylist::Keylist;
use nom::error::Error;
use nom::Err as NomErr;
use num_bigint::BigInt;
use ordered_float::OrderedFloat;
use std::collections::HashMap;
use strum::EnumDiscriminants;

#[derive(Debug, Clone, Hash, PartialEq, EnumDiscriminants)]
#[strum_discriminants(name(RawTermType))]
#[cfg_attr(feature = "serde_impl", derive(Serialize, Deserialize))]
pub enum RawTerm {
    // ATOM_CACHE_REF
    SmallInt(u8),
    Int(i32),
    // OldFloat(String),
    // NEW_PORT,
    // NEW_PID,
    SmallTuple(Vec<RawTerm>),
    LargeTuple(Vec<RawTerm>),
    Map(Vec<(RawTerm, RawTerm)>),
    Nil,
    String(Vec<u8>),
    List(Vec<RawTerm>),
    Improper(Box<RawTerm>),
    Binary(Vec<u8>),
    SmallBigInt(BigInt),
    LargeBigInt(BigInt),
    Pid {
        node: Box<RawTerm>,
        id: u32,
        serial: u32,
        creation: u8,
    },
    NewPid {
        node: Box<RawTerm>,
        id: u32,
        serial: u32,
        creation: u32,
    },
    Port {
        node: Box<RawTerm>,
        id: u32,
        creation: u8,
    },
    NewPort {
        node: Box<RawTerm>,
        id: u32,
        creation: u32,
    },
    Ref {
        node: Box<RawTerm>,
        id: Vec<u32>,
        creation: u8,
    },
    NewerRef {
        node: Box<RawTerm>,
        id: Vec<u32>,
        creation: u32,
    },
    Function {
        size: u32,
        arity: u8,
        uniq: [u8; 16],
        index: u32,
        module: Box<RawTerm>,
        old_index: Box<RawTerm>,
        old_uniq: Box<RawTerm>,
        pid: Box<RawTerm>,
        free_var: Vec<RawTerm>,
    },
    // NEWER_REFERENCE,
    // FUN,
    // EXPORT,
    // BIT_BINARY,
    Float(OrderedFloat<f64>),
    Atom(String),
    SmallAtom(String),
    // REFERENCE_EXT_DEPRECATED,
    AtomDeprecated(String),
    SmallAtomDeprecated(String),
}

impl RawTerm {
    pub fn from_bytes(input: &[u8]) -> Result<RawTerm, NomErr<Error<&[u8]>>> {
        crate::from_bytes(input)
    }

    pub fn to_bytes(self) -> Vec<u8> {
        crate::to_bytes(self)
    }

    #[cfg(feature = "zlib")]
    pub fn to_gzip_bytes(self, level: flate2::Compression) -> std::io::Result<Vec<u8>> {
        crate::to_gzip_bytes(self, level)
    }

    pub fn as_type(&self) -> RawTermType {
        RawTermType::from(self)
    }

    pub fn as_general_type(&self) -> RawTermGeneralType {
        RawTermGeneralType::from(self.as_type())
    }

    pub fn is_atom(&self) -> bool {
        use RawTerm::*;
        match self {
            Atom(_) | AtomDeprecated(_) | SmallAtom(_) | SmallAtomDeprecated(_) => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        use RawTerm::*;
        match self {
            Binary(_) | String(_) => true,
            _ => false,
        }
    }

    pub fn is_string_like(&self) -> bool {
        self.is_string() | self.is_atom()
    }

    pub fn is_integer(&self) -> bool {
        use RawTerm::*;
        match self {
            SmallInt(_) | Int(_) => true,
            _ => false,
        }
    }

    pub fn is_atom_pair(&self) -> bool {
        use RawTerm::*;
        match self {
            SmallTuple(x) if x.len() == 2 => x[0].is_atom(),
            _ => false,
        }
    }

    pub fn is_string_map(&self) -> bool {
        use RawTerm::*;
        match self {
            Map(x) => x.iter().all(|(a, _)| a.is_string_like()),
            _ => false,
        }
    }

    pub fn is_list(&self) -> bool {
        use RawTerm::*;
        match self {
            List(_) => true,
            _ => false,
        }
    }

    pub fn as_atom(self) -> Option<String> {
        use RawTerm::*;
        match self {
            Atom(x) | AtomDeprecated(x) | SmallAtom(x) | SmallAtomDeprecated(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_string(self) -> Option<String> {
        match self {
            RawTerm::Binary(x) | RawTerm::String(x) => {
                Some(String::from_utf8(x).expect("binary not utf-8"))
            }
            _ => None,
        }
    }

    pub fn as_string_like(self) -> Option<String> {
        if self.is_string() {
            self.as_string()
        } else if self.is_atom() {
            self.as_atom()
        } else {
            None
        }
    }

    pub fn as_atom_pair(self) -> Option<(String, RawTerm)> {
        use RawTerm::*;
        match self {
            SmallTuple(mut x) | LargeTuple(mut x) if x.len() == 2 => {
                let b = x.pop().unwrap();
                if let Some(a) = x.pop().unwrap().as_atom() {
                    return Some((a, b));
                }
            }
            _ => (),
        }
        None
    }
}

impl From<Term> for RawTerm {
    fn from(term: Term) -> RawTerm {
        match term {
            Term::Byte(x) => RawTerm::SmallInt(x),
            Term::Int(x) => RawTerm::Int(x),
            Term::Float(x) => RawTerm::Float(x),
            Term::String(x) => string_to_raw_term(x),
            Term::Atom(x) => atom_to_raw_term(x),
            Term::Bytes(x) => RawTerm::Binary(x),
            Term::Bool(x) => RawTerm::SmallAtom(x.to_string()),
            Term::Nil => RawTerm::SmallAtom("nil".to_string()),
            Term::BigInt(x) => big_int_to_raw_term(x),
            Term::Charlist(x) => RawTerm::String(x),
            Term::Keyword(x) => keyword_to_raw_term(x),
            Term::List(x) => list_to_raw_term(x),
            Term::Tuple(x) => tuple_to_raw_term(x),
            Term::Map(x) => map_arbitrary_to_raw_term(x),
            Term::Other(x) => x,
        }
    }
}

fn string_to_raw_term(string: String) -> RawTerm {
    RawTerm::Binary(string.as_bytes().to_vec())
}

fn keyword_to_raw_term(keyword: Keylist<String, Term>) -> RawTerm {
    let tmp: Vec<RawTerm> = keyword
        .into_iter()
        .map(|(a, b)| RawTerm::SmallTuple(vec![string_to_raw_term(a), RawTerm::from(b)]))
        .collect();
    RawTerm::List(tmp)
}

fn map_arbitrary_to_raw_term(map: HashMap<Term, Term>) -> RawTerm {
    let tmp = map
        .into_iter()
        .map(|(k, v)| (RawTerm::from(k), RawTerm::from(v)))
        .collect();
    RawTerm::Map(tmp)
}

fn list_to_raw_term(list: Vec<Term>) -> RawTerm {
    if list.is_empty() {
        RawTerm::Nil
    } else {
        RawTerm::List(list.into_iter().map(|x| RawTerm::from(x)).collect())
    }
}

fn tuple_to_raw_term(tuple: Vec<Term>) -> RawTerm {
    let len = tuple.len();
    let x = tuple.into_iter().map(|x| RawTerm::from(x)).collect();
    if len < 16 {
        RawTerm::SmallTuple(x)
    } else {
        RawTerm::LargeTuple(x)
    }
}

fn big_int_to_raw_term(input: BigInt) -> RawTerm {
    if input.bits() < (255 * 8) {
        RawTerm::SmallBigInt(input)
    } else {
        RawTerm::LargeBigInt(input)
    }
}

fn atom_to_raw_term(input: String) -> RawTerm {
    if input.len() < 256 {
        RawTerm::SmallAtom(input)
    } else {
        RawTerm::Atom(input)
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
/// enum use for implement Ordering in the raw terms
pub enum RawTermGeneralType {
    // Improper should not be used for ordering.
    Improper,
    Number,
    Atom,
    Reference,
    Fun,
    Port,
    Pid,
    Tuple,
    Map,
    Nil,
    List,
    BitString,
}

impl From<RawTermType> for RawTermGeneralType {
    fn from(item: RawTermType) -> RawTermGeneralType {
        RawTermGeneralType::from(&item)
    }
}

impl From<&RawTermType> for RawTermGeneralType {
    fn from(item: &RawTermType) -> RawTermGeneralType {
        use RawTermType::*;
        match item {
            SmallInt => RawTermGeneralType::Number,
            Int => RawTermGeneralType::Number,
            SmallTuple => RawTermGeneralType::Tuple,
            LargeTuple => RawTermGeneralType::Tuple,
            Map => RawTermGeneralType::Map,
            Nil => RawTermGeneralType::Nil,
            String => RawTermGeneralType::BitString,
            List => RawTermGeneralType::List,
            Improper => RawTermGeneralType::Improper,
            Binary => RawTermGeneralType::BitString,
            SmallBigInt => RawTermGeneralType::Number,
            LargeBigInt => RawTermGeneralType::Number,
            Pid => RawTermGeneralType::Pid,
            NewPid => RawTermGeneralType::Pid,
            Port => RawTermGeneralType::Port,
            NewPort => RawTermGeneralType::Port,
            Ref => RawTermGeneralType::Reference,
            NewerRef => RawTermGeneralType::Reference,
            Function => RawTermGeneralType::Fun,
            Float => RawTermGeneralType::Number,
            Atom => RawTermGeneralType::Atom,
            SmallAtom => RawTermGeneralType::Atom,
            AtomDeprecated => RawTermGeneralType::Atom,
            SmallAtomDeprecated => RawTermGeneralType::Atom,
        }
    }
}

#[cfg(test)]
mod from_term_tests {
    use crate::{from_bytes, read_binary, RawTerm};
    use num_bigint::{BigInt, BigUint};

    #[test]
    fn small_int() {
        let input = read_binary("bins/small_int.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(RawTerm::SmallInt(2), out);
    }

    #[test]
    fn small_negative_int() {
        let input = read_binary("bins/small_negative_int.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(RawTerm::Int(-2), out);
    }

    #[test]
    fn int() {
        let input = read_binary("bins/int.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(RawTerm::Int(1234578), out);
    }

    #[test]
    fn negative_int() {
        let input = read_binary("bins/negative_int.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(RawTerm::Int(-1234578), out);
    }

    #[test]
    fn nil() {
        let input = read_binary("bins/nil.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(RawTerm::AtomDeprecated("nil".to_string()), out);
    }

    #[test]
    fn false_test() {
        let input = read_binary("bins/false.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(RawTerm::AtomDeprecated("false".to_string()), out);
    }

    #[test]
    fn true_test() {
        let input = read_binary("bins/true.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(RawTerm::AtomDeprecated("true".to_string()), out);
    }

    #[test]
    fn odd_atom() {
        let input = read_binary("bins/odd_atom.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(RawTerm::SmallAtom("oddţ".to_string()), out);
    }

    #[test]
    fn module_name() {
        let input = read_binary("bins/module_name.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(
            RawTerm::AtomDeprecated("Elixir.TermGenerator".to_string()),
            out
        );
    }

    #[test]
    fn elixir_struct() {
        let input = read_binary("bins/struct.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        let expected = RawTerm::Map(
            vec![
                (
                    RawTerm::AtomDeprecated("__struct__".to_string()),
                    RawTerm::AtomDeprecated("Elixir.TestStruct".to_string()),
                ),
                (
                    RawTerm::AtomDeprecated("a".to_string()),
                    RawTerm::AtomDeprecated("nil".to_string()),
                ),
                (RawTerm::AtomDeprecated("b".to_string()), RawTerm::Int(1234)),
            ]
            .into_iter()
            .collect(),
        );

        assert_eq!(expected, out);
    }

    #[test]
    fn small_string() {
        let input = read_binary("bins/small_string.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(RawTerm::Binary(b"just some text".to_vec()), out);
    }

    #[test]
    fn binary() {
        let input = read_binary("bins/binary.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(RawTerm::Binary(vec![1, 2, 3, 4]), out);
    }

    #[test]
    fn large_string() {
        let input = read_binary("bins/large_string.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        if let RawTerm::Binary(x) = &out {
            assert!(x.starts_with(b"Lorem ipsum dolor sit"))
        } else {
            assert!(false)
        }
    }

    #[test]
    fn float() {
        let input = read_binary("bins/float.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(RawTerm::Float(12.515.into()), out);
    }

    #[test]
    fn empty_list() {
        let input = read_binary("bins/empty_list.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        // assert_eq!(vec![RawTerm::List(vec![])], out);
        assert_eq!(RawTerm::Nil, out);
    }

    #[test]
    fn number_list() {
        let input = read_binary("bins/number_list.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(RawTerm::String(vec![1, 2, 3, 4]), out);
    }

    #[test]
    fn mixed_list() {
        use crate::RawTerm::*;

        let input = read_binary("bins/mixed_list.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(
            List(vec![
                SmallInt(1),
                Binary(b"some".to_vec()),
                SmallInt(2),
                Binary(b"text".to_vec())
            ]),
            out
        );
    }

    #[test]
    fn improper_list() {
        use crate::RawTerm::*;

        let input = read_binary("bins/improper_list.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(
            List(vec![
                SmallInt(1),
                SmallInt(6),
                Improper(Box::new(SmallInt(2))),
            ]),
            out
        );
    }

    #[test]
    fn atom_map() {
        let input = read_binary("bins/atom_map.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        let mut map = Vec::new();

        map.push((
            RawTerm::AtomDeprecated("just".to_string()),
            RawTerm::Binary(b"some key".to_vec()),
        ));
        map.push((
            RawTerm::AtomDeprecated("other".to_string()),
            RawTerm::Binary(b"value".to_vec()),
        ));

        assert_eq!(RawTerm::Map(map), out);
    }

    #[test]
    fn map() {
        use RawTerm::*;

        let input = read_binary("bins/map.bin").unwrap();
        if let Map(out) = from_bytes(&input).unwrap() {
            let mut map = vec![
                (Binary(b"float".to_vec()), Float(3.14.into())),
                (
                    List(vec![Binary(b"list as a key".to_vec())]),
                    List(vec![
                        Binary(b"another".to_vec()),
                        Map(vec![(
                            AtomDeprecated("test".to_string()),
                            AtomDeprecated("false".to_string()),
                        )]
                        .into_iter()
                        .collect()),
                    ]),
                ),
                (SmallInt(1), Binary(b"one".to_vec())),
                (
                    AtomDeprecated("tuple".to_string()),
                    SmallTuple(vec![SmallInt(1), AtomDeprecated("more".to_string())]),
                ),
                (
                    Binary(b"large".to_vec()),
                    SmallBigInt(BigInt::parse_bytes(b"123456789123456789", 10).unwrap()),
                ),
                (
                    Binary(b"nested".to_vec()),
                    Map(vec![(Binary(b"ok".to_vec()), Nil)].into_iter().collect()),
                ),
            ];

            for item in out.iter() {
                if let Some(index) =
                    map.iter()
                        .enumerate()
                        .find_map(|(i, x)| if x == item { Some(i) } else { None })
                {
                    map.remove(index);
                } else {
                    panic!("input has more items then expected")
                }
            }

            assert!(!out.is_empty());
            assert!(map.is_empty());
        } else {
            assert!(false);
        }
    }

    #[test]
    fn keyword() {
        let input = read_binary("bins/keyword.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        let mut map = Vec::new();

        map.push(RawTerm::SmallTuple(vec![
            RawTerm::AtomDeprecated("just".to_string()),
            RawTerm::Binary(b"some key".to_vec()),
        ]));
        map.push(RawTerm::SmallTuple(vec![
            RawTerm::AtomDeprecated("other".to_string()),
            RawTerm::Binary(b"value".to_vec()),
        ]));
        map.push(RawTerm::SmallTuple(vec![
            RawTerm::AtomDeprecated("just".to_string()),
            RawTerm::Int(1234),
        ]));

        assert_eq!(RawTerm::List(map), out);
    }

    #[test]
    fn tuple() {
        let input = read_binary("bins/tuple.bin").unwrap();
        let out = from_bytes(&input).unwrap();
        assert_eq!(
            RawTerm::SmallTuple(vec![
                RawTerm::Binary(b"test".to_vec()),
                RawTerm::Binary(b"testing".to_vec())
            ]),
            out
        );
        let large_tuple = read_binary("bins/large_tuple.bin").unwrap();
        let out = from_bytes(&large_tuple).unwrap();
        let mut tuple = vec![];
        for i in 1..401 {
            if i < 256 {
                tuple.push(RawTerm::SmallInt(i as u8));
            } else {
                tuple.push(RawTerm::Int(i));
            }
        }
        let tuple = RawTerm::SmallTuple(vec![
            RawTerm::AtomDeprecated("row_data".into()),
            RawTerm::Binary(b"kkk".to_vec()),
            RawTerm::LargeTuple(tuple),
            RawTerm::SmallInt(10),
        ]);
        assert_eq!(tuple, out);
        let bin = tuple.to_bytes();
        assert_eq!(bin, large_tuple);
    }

    #[test]
    fn small_big_int() {
        let input = read_binary("bins/small_big_int.bin").unwrap();
        let out = from_bytes(&input).unwrap();
        assert_eq!(
            RawTerm::SmallBigInt(BigInt::parse_bytes(b"123456789123456789123456789", 10).unwrap()),
            out
        );
    }

    #[test]
    fn large_big_int() {
        use num_traits::pow::Pow;

        let input = read_binary("bins/large_big_int.bin").unwrap();
        let out = from_bytes(&input).unwrap();
        let nineninenine = BigUint::parse_bytes(b"999", 10).unwrap();

        assert_eq!(
            RawTerm::LargeBigInt(BigInt::from(nineninenine.clone().pow(&nineninenine))),
            out
        );
    }

    #[test]
    fn pid() {
        let input = read_binary("bins/pid.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(
            RawTerm::Pid {
                node: Box::new(RawTerm::AtomDeprecated("nonode@nohost".to_string())),
                id: 91,
                serial: 0,
                creation: 0
            },
            out
        );
    }
    #[test]
    fn new_pid() {
        let input = read_binary("bins/new_pid.bin").unwrap();
        let out = from_bytes(&input).unwrap();
        let expect = RawTerm::NewPid {
            node: Box::new(RawTerm::AtomDeprecated("nonode@nohost".to_string())),
            id: 79,
            serial: 0,
            creation: 0,
        };
        assert_eq!(expect, out);
        assert_eq!(&input, &expect.to_bytes());
    }
    #[test]
    fn function() {
        let input = read_binary("bins/function.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(
            RawTerm::Function {
                size: 85,
                arity: 2,
                uniq: [149, 84, 239, 178, 136, 29, 208, 62, 138, 103, 212, 245, 20, 90, 180, 225],
                index: 0,
                module: Box::new(RawTerm::AtomDeprecated("Elixir.TermGenerator".to_string())),
                old_index: Box::new(RawTerm::SmallInt(0)),
                old_uniq: Box::new(RawTerm::Int(78292861)),
                pid: Box::new(RawTerm::Pid {
                    node: Box::new(RawTerm::AtomDeprecated("nonode@nohost".to_string())),
                    id: 0,
                    serial: 0,
                    creation: 0
                }),
                free_var: Vec::new()
            },
            out
        );
        // function included new_pid
        // #Fun<big_data_redis_SUITE.0.96608257>
        let input = &[
            131, 112, 0, 0, 0, 88, 0, 184, 68, 0, 56, 201, 101, 109, 209, 140, 69, 18, 224, 71,
            189, 151, 33, 0, 0, 0, 0, 0, 0, 0, 0, 100, 0, 20, 98, 105, 103, 95, 100, 97, 116, 97,
            95, 114, 101, 100, 105, 115, 95, 83, 85, 73, 84, 69, 97, 0, 98, 5, 194, 32, 1, 88, 100,
            0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 3, 87, 0,
            0, 0, 0, 0, 0, 0, 0,
        ];
        let expected = RawTerm::Function {
            size: 88,
            arity: 0,
            uniq: [
                184, 68, 0, 56, 201, 101, 109, 209, 140, 69, 18, 224, 71, 189, 151, 33,
            ],
            index: 0,
            module: Box::new(RawTerm::AtomDeprecated("big_data_redis_SUITE".into())),
            old_index: Box::new(RawTerm::SmallInt(0)),
            old_uniq: Box::new(RawTerm::Int(96608257)),
            pid: Box::new(RawTerm::NewPid {
                node: Box::new(RawTerm::AtomDeprecated("nonode@nohost".into())),
                id: 855,
                serial: 0,
                creation: 0,
            }),
            free_var: Vec::new(),
        };
        let out = RawTerm::from_bytes(input).unwrap();
        assert_eq!(expected, out);
        assert_eq!(&expected.to_bytes(), input);
    }

    #[test]
    fn port() {
        let input = read_binary("bins/port.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(
            RawTerm::Port {
                node: Box::new(RawTerm::AtomDeprecated("nonode@nohost".to_string())),
                id: 3,
                creation: 0
            },
            out
        );
    }

    #[test]
    fn reference() {
        let input = read_binary("bins/ref.bin").unwrap();
        let out = from_bytes(&input).unwrap();

        assert_eq!(
            RawTerm::Ref {
                node: Box::new(RawTerm::AtomDeprecated("nonode@nohost".to_string())),
                id: vec![158726, 438566918, 237133],
                creation: 0
            },
            out
        );
    }

    #[test]
    #[cfg(feature = "zlib")]
    fn gzip() {
        let input = read_binary("bins/number_list_gzip.bin").unwrap();
        let out = from_bytes(&input).unwrap();
        assert!(out.is_list());
    }
}

#[cfg(test)]
mod as_type_tests {
    use crate::raw_term::RawTermType;
    use crate::RawTerm;

    #[test]
    fn as_type_binary() {
        let term = RawTerm::Binary(vec![1, 2, 3, 4]);

        assert_eq!(RawTermType::Binary, term.as_type())
    }

    #[test]
    fn as_type_float() {
        let term = RawTerm::Float(0.123.into());

        assert_eq!(RawTermType::Float, term.as_type())
    }

    #[test]
    fn as_type_nil() {
        let term = RawTerm::Nil;

        assert_eq!(RawTermType::Nil, term.as_type())
    }

    #[test]
    fn as_type_map() {
        let term = RawTerm::Map(
            vec![(RawTerm::Atom(String::from("test")), RawTerm::Nil)]
                .into_iter()
                .collect(),
        );

        assert_eq!(RawTermType::Map, term.as_type())
    }
}

#[test]
fn raw_sub_type_ordering() {
    use RawTermGeneralType::*;
    // number < atom < reference < fun < port < pid < tuple < map < nil < list < bit string

    // Improper should not be used for ordering.
    assert!(Improper < Number);

    assert!(Number < Atom);
    assert!(Atom < Reference);
    assert!(Reference < Fun);
    assert!(Fun < Port);
    assert!(Port < Pid);
    assert!(Pid < Tuple);
    assert!(Tuple < Map);
    assert!(Map < Nil);
    assert!(Nil < List);
    assert!(List < BitString);

    assert!(Number < BitString);
    assert!(!(Number > BitString));
}
