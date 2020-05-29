use crate::Term;
use keylist::Keylist;
use nom::error::ErrorKind;
use nom::Err as NomErr;
use num_bigint::BigInt;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
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
    Port {
        node: Box<RawTerm>,
        id: u32,
        creation: u8,
    },
    Ref {
        node: Box<RawTerm>,
        id: Vec<u32>,
        creation: u8,
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
    Float(f64),
    Atom(String),
    SmallAtom(String),
    // REFERENCE_EXT_DEPRECATED,
    AtomDeprecated(String),
    SmallAtomDeprecated(String),
}

impl RawTerm {
    pub fn from_bytes(input: &[u8]) -> Result<RawTerm, NomErr<(&[u8], ErrorKind)>> {
        crate::from_bytes(input)
    }

    pub fn to_bytes(self) -> Vec<u8> {
        crate::to_bytes(self)
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
            Term::Map(x) => map_to_raw_term(x),
            Term::Keyword(x) => keyword_to_raw_term(x),
            Term::List(x) => list_to_raw_term(x),
            Term::Tuple(x) => tuple_to_raw_term(x),
            Term::MapArbitrary(x) => map_arbitrary_to_raw_term(x),
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

fn map_arbitrary_to_raw_term(map: Keylist<Term, Term>) -> RawTerm {
    let tmp: Vec<(RawTerm, RawTerm)> = map
        .into_iter()
        .map(|(k, v)| (RawTerm::from(k), RawTerm::from(v)))
        .collect();
    RawTerm::Map(tmp)
}

fn map_to_raw_term(map: HashMap<String, Term>) -> RawTerm {
    let tmp: Vec<(RawTerm, RawTerm)> = map
        .into_iter()
        .map(|(k, v)| (string_to_raw_term(k), RawTerm::from(v)))
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

        assert_eq!(RawTerm::Float(12.515), out);
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
        if let Map(mut out) = from_bytes(&input).unwrap() {
            out.sort_by(|a, b| a.partial_cmp(b).unwrap());
            // let mut map = Vec::new();

            // map.push((
            //     RawTerm::AtomDeprecated("just".to_string()),
            //     RawTerm::Binary(b"some key".to_vec()),
            // ));
            // map.push((
            //     RawTerm::AtomDeprecated("other".to_string()),
            //     RawTerm::Binary(b"value".to_vec()),
            // ));
            let mut map = vec![
                (Binary(b"float".to_vec()), Float(3.14)),
                (
                    List(vec![Binary(b"list as a key".to_vec())]),
                    List(vec![
                        Binary(b"another".to_vec()),
                        Map(vec![(
                            AtomDeprecated("test".to_string()),
                            AtomDeprecated("false".to_string()),
                        )]),
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
                    Map(vec![(Binary(b"ok".to_vec()), Nil)]),
                ),
            ];
            map.sort_by(|a, b| a.partial_cmp(b).unwrap());

            assert_eq!(map, out);
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
            RawTerm::LargeBigInt(BigInt::from(nineninenine.pow(&nineninenine))),
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
}
