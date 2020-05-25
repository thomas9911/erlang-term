// http://erlang.org/doc/apps/erts/erl_ext_dist.html#distribution-header
// https://godoc.org/github.com/goerlang/etf#Term

const START: u8 = 131;
// const ATOM_CACHE_REF: u8 = 82;
const SMALL_INTEGER_EXT: u8 = 97;
const INTEGER_EXT: u8 = 98;
// const FLOAT_EXT: u8 = 99;
const PORT_EXT: u8 = 102;
// const NEW_PORT_EXT: u8 = 89;
const PID_EXT: u8 = 103;
// const NEW_PID_EXT: u8 = 88;
const SMALL_TUPLE_EXT: u8 = 104;
// const LARGE_TUPLE_EXT: u8 = 105;
const MAP_EXT: u8 = 116;
const NIL_EXT: u8 = 106;
const STRING_EXT: u8 = 107;
const LIST_EXT: u8 = 108;
const BINARY_EXT: u8 = 109;
const SMALL_BIG_EXT: u8 = 110;
const LARGE_BIG_EXT: u8 = 111;
const NEW_REFERENCE_EXT: u8 = 114;
// const NEWER_REFERENCE_EXT: u8 = 90;
// const FUN_EXT: u8 = 117;
const NEW_FUN_EXT: u8 = 112;
// const EXPORT_EXT: u8 = 113;
// const BIT_BINARY_EXT: u8 = 77;
const NEW_FLOAT_EXT: u8 = 70;
// const ATOM_UTF8_EXT: u8 = 118;
const SMALL_ATOM_UTF8_EXT: u8 = 119;
// const REFERENCE_EXT (deprecated) : u8= 101;
const ATOM_EXT_DEPRECATED: u8 = 100;
// const SMALL_ATOM_EXT (deprecated) : u8= 115;

// #[macro_use]
extern crate nom;
extern crate num_bigint;

use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::combinator::all_consuming;
use nom::error::ErrorKind;
use nom::multi::{many0, many_m_n};
use nom::sequence::{preceded, tuple};
use nom::{Err as NomErr, IResult};
use num_bigint::{BigInt, Sign};
use std::convert::TryInto;
use std::fs::File;
use std::io::{Read, Result as IoResult};

#[derive(Debug, Clone, PartialEq)]
pub enum RawTerm {
    // SmallInt(u8),
    // Int(u32),
    // Nil,
    // Atom(String),
    // List(Vec<RawTerm>),
    // Charlist(Vec<u8>),
    // String(String),
    // Float(f64),
    // Improper(Box<RawTerm>),
    // Map(HashMap<String, RawTerm>),
    // MapArbitrary(Vec<(RawTerm, RawTerm)>),
    // Keyword(HashMap<String, RawTerm>),
    // Pair(Box<RawTerm>, Box<RawTerm>),
    // Tuple(Vec<RawTerm>),

    // const ATOM_CACHE_REF: u8 = 82;
    SmallInt(u8),
    Int(u32),
    OldFloat(String),
    // PORT,
    // NEW_PORT,
    // PID,
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
        old_index: u32,
        old_uniq: u32,
        pid: Box<RawTerm>,
        free_var: Vec<RawTerm>,
    },
    // NEW_REFERENCE,
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
    // pub fn is_string_like(&self) -> bool {
    //     use RawTerm::*;
    //     match self {
    //         Atom(_) | String(_) => true,
    //         _ => false,
    //     }
    // }

    // pub fn is_pair(&self) -> bool {
    //     use RawTerm::*;
    //     match self {
    //         Pair(_, _) => true,
    //         _ => false,
    //     }
    // }

    // pub fn is_atom_pair(&self) -> bool {
    //     use RawTerm::*;
    //     match self {
    //         Pair(x, _) => {
    //             match *x.clone() {
    //                 Atom(_) => true,
    //                 _ => false,
    //             }
    //         },
    //         _ => false,
    //     }
    // }

    // pub fn as_string(&self) -> Option<String> {
    //     use RawTerm::*;
    //     match self {
    //         Atom(x) | String(x) => Some(x.to_owned()),
    //         _ => None,
    //     }
    // }

    // pub fn as_pair(&self) -> Option<(RawTerm, RawTerm)> {
    //     use RawTerm::*;
    //     match self {
    //         Pair(x, y) => Some((*x.clone(), *y.clone())),
    //         _ => None,
    //     }
    // }

    // pub fn as_atom_pair(&self) -> Option<(String, RawTerm)> {
    //     use RawTerm::*;
    //     match self {
    //         Pair(x, y) => {
    //             match *x.clone() {
    //                 Atom(x) => Some((x, *y.clone())),
    //                 _ => None,
    //             }
    //         },
    //         _ => None,
    //     }
    // }
}

// elixir formats lists with numbers below 32 as lists otherwise as charlists
// elixir formats binaries with numbers below 32 as lists otherwise as string

pub fn read_binary(input: &str) -> IoResult<Vec<u8>> {
    let mut file = File::open(input)?;
    let mut buffer = Vec::new();

    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}

pub fn from_term(input: &[u8]) -> Result<Vec<RawTerm>, NomErr<(&[u8], ErrorKind)>> {
    let (_, output) = parser(input)?;
    // many0(small_int)(input)

    Ok(output)
}

pub fn parser(input: &[u8]) -> IResult<&[u8], Vec<RawTerm>> {
    // pub fn parser(input: &[u8]) -> Result<Vec<RawTerm>, NomErr<(&[u8], ErrorKind)>> {
    // pub fn parser(input: &[u8]) -> Result<Vec<RawTerm>, &[u8]> {
    all_consuming(preceded(tag(&[START]), many0(term)))(input)
}

fn term(input: &[u8]) -> IResult<&[u8], RawTerm> {
    alt((
        small_int,
        int,
        float,
        atom_deprecated,
        small_atom,
        empty_list,
        binary,
        string,
        list,
        map,
        small_tuple,
        small_big_int,
        large_big_int,
        pid,
        port,
        reference,
        function,
    ))(input)
}

fn small_atom(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let (i, t) = preceded(tag(&[SMALL_ATOM_UTF8_EXT]), take(1usize))(input)?;
    let length = t[0] as usize;
    let (i, t) = take(length)(i)?;
    Ok((
        i,
        RawTerm::SmallAtom(String::from_utf8(t.to_vec()).expect("atom name was not valid")),
    ))
}

fn small_tuple(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let (i, t) = preceded(tag(&[SMALL_TUPLE_EXT]), take(1usize))(input)?;
    let length = t[0] as usize;

    let (i, t) = many_m_n(length, length, term)(i)?;

    // if t.len() == 2 {
    //     Ok((
    //         i,
    //         RawTerm::Pair(Box::new(t[0].clone()), Box::new(t[1].clone())),
    //     ))
    // } else {
    //     Ok((i, RawTerm::Tuple(t)))
    // }
    Ok((i, RawTerm::SmallTuple(t)))
}
fn atom_deprecated(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let (i, t) = preceded(tag(&[ATOM_EXT_DEPRECATED]), take(2usize))(input)?;

    let length = slice_to_u16(t) as usize;

    let (i, t) = take(length)(i)?;

    Ok((
        i,
        RawTerm::AtomDeprecated(String::from_utf8(t.to_vec()).expect("atom name was not valid")),
    ))
}

fn node_or_module(input: &[u8]) -> IResult<&[u8], RawTerm> {
    alt((
        small_atom,
        atom_deprecated,
        // also atom
        //      atom cache
    ))(input)
}

fn small_int_or_int(input: &[u8]) -> IResult<&[u8], u32> {
    let (i, t) = alt((small_int, int))(input)?;

    let x = match t {
        RawTerm::Int(x) => x,
        RawTerm::SmallInt(x) => x as u32,
        _ => unreachable!(),
    };

    Ok((i, x))
}

fn pid(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let get_data = tuple((node_or_module, take(4usize), take(4usize), take(1usize)));

    let (i, (node, id, serial, creation)) = preceded(tag(&[PID_EXT]), get_data)(input)?;
    let id = slice_to_u32(id);
    let serial = slice_to_u32(serial);
    let creation = slice_to_u8(creation);

    Ok((
        i,
        RawTerm::Pid {
            node: Box::new(node),
            id,
            serial,
            creation,
        },
    ))
}

fn port(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let get_data = tuple((node_or_module, take(4usize), take(1usize)));

    let (i, (node, id, creation)) = preceded(tag(&[PORT_EXT]), get_data)(input)?;
    let id = slice_to_u32(id);
    let creation = slice_to_u8(creation);

    Ok((
        i,
        RawTerm::Port {
            node: Box::new(node),
            id,
            creation,
        },
    ))
}

fn reference(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let get_data = tuple((take(2usize), node_or_module, take(1usize)));
    let (i, (length, node, creation)) = preceded(tag(&[NEW_REFERENCE_EXT]), get_data)(input)?;
    let length = slice_to_u16(length) as usize;
    let (i, id_bytes) = take(4 * length)(i)?;
    let creation = slice_to_u8(creation);
    let id: Vec<u32> = id_bytes.chunks(4).map(|x| slice_to_u32(x)).collect();
    Ok((
        i,
        RawTerm::Ref {
            node: Box::new(node),
            id,
            creation,
        },
    ))
}

fn function(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let get_data = tuple((
        take(4usize),  // Size
        take(1usize),  // Arity
        take(16usize), // Uniq
        take(4usize),  // Index
        take(4usize),  // NumFree
        node_or_module,
        small_int_or_int,
        small_int_or_int,
        pid,
    ));

    let (i, (size, arity, uniq, index, num_free, module, old_index, old_uniq, pid)) =
        preceded(tag(&[NEW_FUN_EXT]), get_data)(input)?;
    let size = slice_to_u32(size);
    let arity = slice_to_u8(arity);
    // let uniq = slice_to_u128(uniq);
    let index = slice_to_u32(index);
    let num_free = slice_to_u32(num_free) as usize;
    let uniq: [u8; 16] = uniq.try_into().unwrap();

    let (i, free_vars) = many_m_n(num_free, num_free, term)(i)?;

    Ok((
        i,
        RawTerm::Function {
            size,
            arity,
            uniq,
            index,
            module: Box::new(module),
            old_index,
            old_uniq,
            pid: Box::new(pid),
            free_var: free_vars,
        },
    ))
}

fn list(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let (i, t) = preceded(tag(&[LIST_EXT]), take(4usize))(input)?;
    let length = slice_to_u32(t) as usize;
    let (i, mut t) = many_m_n(length + 1, length + 1, term)(i)?;

    if let Some(x) = t.pop() {
        match x {
            RawTerm::Nil => (),
            // RawTerm::List(y) => {
            //     if !y.is_empty() {
            //         t.push(RawTerm::Improper(Box::new(RawTerm::List(y))))
            //     }
            // }
            y => t.push(RawTerm::Improper(Box::new(y))),
        }
    }

    // if t.iter().all(|x| x.is_atom_pair()) {
    //     let list = HashMap::from_iter(t.iter().map(|x| x.as_atom_pair().unwrap()));
    //     Ok((i, RawTerm::Keyword(list)))
    // } else {
    //     Ok((i, RawTerm::List(t)))
    // }
    Ok((i, RawTerm::List(t)))
}

fn binary(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let (i, t) = preceded(tag(&[BINARY_EXT]), take(4usize))(input)?;

    let length = slice_to_u32(t) as usize;

    let (i, t) = take(length)(i)?;
    Ok((i, RawTerm::Binary(t.to_vec())))
}

fn string(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let (i, t) = preceded(tag(&[STRING_EXT]), take(2usize))(input)?;

    let length = slice_to_u16(t) as usize;

    let (i, t) = take(length)(i)?;
    Ok((i, RawTerm::String(t.to_vec())))
}

fn small_int(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let (i, t) = preceded(tag(&[SMALL_INTEGER_EXT]), take(1usize))(input)?;
    Ok((i, RawTerm::SmallInt(t[0])))
}

fn int(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let (i, t) = preceded(tag(&[INTEGER_EXT]), take(4usize))(input)?;
    let new_int = slice_to_u32(t);

    Ok((i, RawTerm::Int(new_int)))
}

fn float(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let (i, t) = preceded(tag(&[NEW_FLOAT_EXT]), take(8usize))(input)?;
    let new_float: [u8; 8] = t.try_into().unwrap();
    Ok((i, RawTerm::Float(f64::from_be_bytes(new_float))))
}

fn empty_list(input: &[u8]) -> IResult<&[u8], RawTerm> {
    // alt((alpha1, digit1))(input)
    let (i, _) = tag(&[NIL_EXT])(input)?;

    // Ok((i, RawTerm::List(vec![])))
    Ok((i, RawTerm::Nil))
}

fn map(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let (i, t) = preceded(tag(&[MAP_EXT]), take(4usize))(input)?;
    let length = slice_to_u32(t) as usize;

    let (i, t) = many_m_n(2 * length, 2 * length, term)(i)?;

    let mut keyword: Vec<(RawTerm, RawTerm)> = Vec::new();

    // t.chunks(2).map(|[k, v]| (k.clone(), v.clone())).collect();
    // let mut all_strings = true;
    for ch in t.chunks(2) {
        // all_strings = all_strings && ch[0].is_string_like();
        keyword.push((ch[0].clone(), ch[1].clone()));
    }

    // if all_strings {
    //     let map = HashMap::from_iter(
    //         keyword
    //             .into_iter()
    //             .map(|(k, v)| (k.as_string().unwrap(), v)),
    //     );
    //     Ok((i, RawTerm::Map(map)))
    // } else {
    //     Ok((i, RawTerm::MapArbitrary(keyword)))
    // }
    Ok((i, RawTerm::Map(keyword)))
}

fn small_big_int(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let (i, (length, sign)) =
        preceded(tag(&[SMALL_BIG_EXT]), tuple((take(1usize), take(1usize))))(input)?;

    let (i, t) = take(length[0])(i)?;
    let sign = match sign[0] {
        1 => Sign::Minus,
        0 => Sign::Plus,
        _ => unreachable!(),
    };

    Ok((i, RawTerm::SmallBigInt(BigInt::from_bytes_le(sign, t))))
}

fn large_big_int(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let (i, (length, sign)) =
        preceded(tag(&[LARGE_BIG_EXT]), tuple((take(4usize), take(1usize))))(input)?;
    let length = slice_to_u32(length) as usize;

    let (i, t) = take(length)(i)?;
    let sign = match sign[0] {
        1 => Sign::Minus,
        0 => Sign::Plus,
        _ => unreachable!(),
    };

    Ok((i, RawTerm::LargeBigInt(BigInt::from_bytes_le(sign, t))))
}

// fn slice_to_u128(input: &[u8]) -> u128 {
//     let new_int: [u8; 16] = input.try_into().unwrap();
//     u128::from_be_bytes(new_int)
// }

fn slice_to_u32(input: &[u8]) -> u32 {
    let new_int: [u8; 4] = input.try_into().unwrap();
    u32::from_be_bytes(new_int)
}

fn slice_to_u16(input: &[u8]) -> u16 {
    let new_int: [u8; 2] = input.try_into().unwrap();
    u16::from_be_bytes(new_int)
}

fn slice_to_u8(input: &[u8]) -> u8 {
    let new_int: [u8; 1] = input.try_into().unwrap();
    new_int[0]
}

// fn skip_first(input: &[u8]) -> IResult<&[u8], &[u8]> {
//     // alt((alpha1, digit1))(input)
//     let (i, _) = tag(&[131u8])(input)?;

//     Ok((i, &[]))
// }

#[cfg(test)]
mod tests {
    use crate::{from_term, read_binary, RawTerm};
    use num_bigint::{BigInt, BigUint};

    #[test]
    fn small_int() {
        let input = read_binary("bins/small_int.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(vec![RawTerm::SmallInt(2)], out);
    }

    #[test]
    fn int() {
        let input = read_binary("bins/int.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(vec![RawTerm::Int(1234578)], out);
    }

    #[test]
    fn nil() {
        let input = read_binary("bins/nil.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(vec![RawTerm::AtomDeprecated("nil".to_string())], out);
    }

    #[test]
    fn false_test() {
        let input = read_binary("bins/false.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(vec![RawTerm::AtomDeprecated("false".to_string())], out);
    }

    #[test]
    fn true_test() {
        let input = read_binary("bins/true.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(vec![RawTerm::AtomDeprecated("true".to_string())], out);
    }

    #[test]
    fn odd_atom() {
        let input = read_binary("bins/odd_atom.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(vec![RawTerm::SmallAtom("oddţ".to_string())], out);
    }

    #[test]
    fn module_name() {
        let input = read_binary("bins/module_name.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(
            vec![RawTerm::AtomDeprecated("Elixir.TermGenerator".to_string())],
            out
        );
    }

    #[test]
    fn small_string() {
        let input = read_binary("bins/small_string.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(vec![RawTerm::Binary(b"just some text".to_vec())], out);
    }

    #[test]
    fn large_string() {
        let input = read_binary("bins/large_string.bin").unwrap();
        let out = from_term(&input).unwrap();

        if let RawTerm::Binary(x) = &out[0] {
            assert!(x.starts_with(b"Lorem ipsum dolor sit"))
        } else {
            assert!(false)
        }
    }

    #[test]
    fn float() {
        let input = read_binary("bins/float.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(vec![RawTerm::Float(12.515)], out);
    }

    #[test]
    fn empty_list() {
        let input = read_binary("bins/empty_list.bin").unwrap();
        let out = from_term(&input).unwrap();

        // assert_eq!(vec![RawTerm::List(vec![])], out);
        assert_eq!(vec![RawTerm::Nil], out);
    }

    #[test]
    fn number_list() {
        let input = read_binary("bins/number_list.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(vec![RawTerm::String(vec![1, 2, 3, 4])], out);
    }

    #[test]
    fn mixed_list() {
        use crate::RawTerm::*;

        let input = read_binary("bins/mixed_list.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(
            vec![List(vec![
                SmallInt(1),
                Binary(b"some".to_vec()),
                SmallInt(2),
                Binary(b"text".to_vec())
            ])],
            out
        );
    }

    #[test]
    fn improper_list() {
        use crate::RawTerm::*;

        let input = read_binary("bins/improper_list.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(
            vec![List(vec![
                SmallInt(1),
                SmallInt(6),
                Improper(Box::new(SmallInt(2))),
            ])],
            out
        );
    }

    #[test]
    fn atom_map() {
        let input = read_binary("bins/atom_map.bin").unwrap();
        let out = from_term(&input).unwrap();

        let mut map = Vec::new();

        map.push((
            RawTerm::AtomDeprecated("just".to_string()),
            RawTerm::Binary(b"some key".to_vec()),
        ));
        map.push((
            RawTerm::AtomDeprecated("other".to_string()),
            RawTerm::Binary(b"value".to_vec()),
        ));

        assert_eq!(vec![RawTerm::Map(map)], out);
    }

    #[test]
    fn keyword() {
        let input = read_binary("bins/keyword.bin").unwrap();
        println!("{:?}", input);
        let out = from_term(&input).unwrap();

        let mut map = Vec::new();

        map.push(RawTerm::SmallTuple(vec![
            RawTerm::AtomDeprecated("just".to_string()),
            RawTerm::Binary(b"some key".to_vec()),
        ]));
        map.push(RawTerm::SmallTuple(vec![
            RawTerm::AtomDeprecated("other".to_string()),
            RawTerm::Binary(b"value".to_vec()),
        ]));

        assert_eq!(vec![RawTerm::List(map)], out);
    }

    #[test]
    fn tuple() {
        let input = read_binary("bins/tuple.bin").unwrap();
        let out = from_term(&input).unwrap();
        assert_eq!(
            vec![RawTerm::SmallTuple(vec![
                RawTerm::Binary(b"test".to_vec()),
                RawTerm::Binary(b"testing".to_vec())
            ])],
            out
        );
    }

    #[test]
    fn small_big_int() {
        let input = read_binary("bins/small_big_int.bin").unwrap();
        let out = from_term(&input).unwrap();
        assert_eq!(
            vec![RawTerm::SmallBigInt(
                BigInt::parse_bytes(b"123456789123456789123456789", 10).unwrap()
            )],
            out
        );
    }

    #[test]
    fn large_big_int() {
        use num_traits::pow::Pow;

        let input = read_binary("bins/large_big_int.bin").unwrap();
        let out = from_term(&input).unwrap();
        let nineninenine = BigUint::parse_bytes(b"999", 10).unwrap();

        assert_eq!(
            vec![RawTerm::LargeBigInt(BigInt::from(
                nineninenine.pow(&nineninenine)
            ))],
            out
        );
    }

    #[test]
    fn pid() {
        let input = read_binary("bins/pid.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(
            vec![RawTerm::Pid {
                node: Box::new(RawTerm::AtomDeprecated("nonode@nohost".to_string())),
                id: 91,
                serial: 0,
                creation: 0
            }],
            out
        );
    }

    #[test]
    fn function() {
        let input = read_binary("bins/function.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(
            vec![RawTerm::Function {
                size: 85,
                arity: 2,
                uniq: [149, 84, 239, 178, 136, 29, 208, 62, 138, 103, 212, 245, 20, 90, 180, 225],
                index: 0,
                module: Box::new(RawTerm::AtomDeprecated("Elixir.TermGenerator".to_string())),
                old_index: 0,
                old_uniq: 78292861,
                pid: Box::new(RawTerm::Pid {
                    node: Box::new(RawTerm::AtomDeprecated("nonode@nohost".to_string())),
                    id: 0,
                    serial: 0,
                    creation: 0
                }),
                free_var: Vec::new()
            }],
            out
        );
    }

    #[test]
    fn port() {
        let input = read_binary("bins/port.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(
            vec![RawTerm::Port {
                node: Box::new(RawTerm::AtomDeprecated("nonode@nohost".to_string())),
                id: 3,
                creation: 0
            }],
            out
        );
    }

    #[test]
    fn reference() {
        let input = read_binary("bins/ref.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(
            vec![RawTerm::Ref {
                node: Box::new(RawTerm::AtomDeprecated("nonode@nohost".to_string())),
                id: vec![158726, 438566918, 237133],
                creation: 0
            }],
            out
        );
    }
}
