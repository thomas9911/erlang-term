// http://erlang.org/doc/apps/erts/erl_ext_dist.html#distribution-header
use std::collections::HashMap;
use std::iter::FromIterator;

const START: u8 = 131;
// const ATOM_CACHE_REF: u8 = 82;
const SMALL_INTEGER_EXT: u8 = 97;
const INTEGER_EXT: u8 = 98;
// const FLOAT_EXT: u8 = 99;
// const PORT_EXT: u8 = 102;
// const NEW_PORT_EXT: u8 = 89;
// const PID_EXT: u8 = 103;
// const NEW_PID_EXT: u8 = 88;
const SMALL_TUPLE_EXT: u8 = 104;
// const LARGE_TUPLE_EXT: u8 = 105;
const MAP_EXT: u8 = 116;
const NIL_EXT: u8 = 106;
const STRING_EXT: u8 = 107;
const LIST_EXT: u8 = 108;
const BINARY_EXT: u8 = 109;
// const SMALL_BIG_EXT: u8 = 110;
// const LARGE_BIG_EXT: u8 = 111;
// const NEW_REFERENCE_EXT: u8 = 114;
// const NEWER_REFERENCE_EXT: u8 = 90;
// const FUN_EXT: u8 = 117;
// const NEW_FUN_EXT: u8 = 112;
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

use nom::branch::alt;
use nom::bytes::complete::{tag, take};
// use nom::character::complete::{alpha1, digit1};
use nom::combinator::all_consuming;
use nom::error::ErrorKind;
use nom::multi::{many0, many_m_n};
use nom::sequence::{preceded, tuple};
use nom::{Err as NomErr, IResult};
use std::convert::TryInto;
use std::fs::File;
use std::io::{Read, Result as IoResult};
// use nom::character::streaming;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    SmallInt(u8),
    Int(u32),
    Nil,
    Atom(String),
    List(Vec<Value>),
    Charlist(Vec<u8>),
    String(String),
    Float(f64),
    Improper(Box<Value>),
    Map(HashMap<String, Value>),
    MapArbitrary(Vec<(Value, Value)>),
    Keyword(HashMap<String, Value>),
    Pair(Box<Value>, Box<Value>),
    Tuple(Vec<Value>),
}

impl Value {
    pub fn is_string_like(&self) -> bool {
        use Value::*;
        match self {
            Atom(_) | String(_) => true,
            _ => false,
        }
    }

    pub fn is_pair(&self) -> bool {
        use Value::*;
        match self {
            Pair(_, _) => true,
            _ => false,
        }
    }

    pub fn is_atom_pair(&self) -> bool {
        use Value::*;
        match self {
            Pair(x, _) => {
                match *x.clone() {
                    Atom(_) => true,
                    _ => false,
                }
            },
            _ => false,
        }
    }

    pub fn as_string(&self) -> Option<String> {
        use Value::*;
        match self {
            Atom(x) | String(x) => Some(x.to_owned()),
            _ => None,
        }
    }

    pub fn as_pair(&self) -> Option<(Value, Value)> {
        use Value::*;
        match self {
            Pair(x, y) => Some((*x.clone(), *y.clone())),
            _ => None,
        }
    }

    pub fn as_atom_pair(&self) -> Option<(String, Value)> {
        use Value::*;
        match self {
            Pair(x, y) => {
                match *x.clone() {
                    Atom(x) => Some((x, *y.clone())),
                    _ => None,
                }
            },
            _ => None,
        }
    }
}

// elixir formats lists with numbers below 32 as lists otherwise as charlists
// elixir formats binaries with numbers below 32 as lists otherwise as string

pub fn read_binary(input: &str) -> IoResult<Vec<u8>> {
    let mut file = File::open(input)?;
    let mut buffer = Vec::new();

    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}

pub fn from_term(input: &[u8]) -> Result<Vec<Value>, NomErr<(&[u8], ErrorKind)>> {
    let (_, output) = parser(input)?;
    // many0(small_int)(input)

    Ok(output)
}

pub fn parser(input: &[u8]) -> IResult<&[u8], Vec<Value>> {
    // pub fn parser(input: &[u8]) -> Result<Vec<Value>, NomErr<(&[u8], ErrorKind)>> {
    // pub fn parser(input: &[u8]) -> Result<Vec<Value>, &[u8]> {
    all_consuming(preceded(tag(&[START]), many0(term)))(input)
}

fn term(input: &[u8]) -> IResult<&[u8], Value> {
    alt((
        small_int,
        int,
        float,
        atom_deprecated,
        small_atom,
        empty_list,
        string,
        charlist,
        list,
        map,
        small_tuple,
    ))(input)
}

fn small_atom(input: &[u8]) -> IResult<&[u8], Value> {
    let (i, t) = preceded(tag(&[SMALL_ATOM_UTF8_EXT]), take(1usize))(input)?;
    let length = t[0] as usize;
    let (i, t) = take(length)(i)?;
    Ok((
        i,
        Value::Atom(String::from_utf8(t.to_vec()).expect("atom name was not valid")),
    ))
}

fn small_tuple(input: &[u8]) -> IResult<&[u8], Value> {
    let (i, t) = preceded(tag(&[SMALL_TUPLE_EXT]), take(1usize))(input)?;
    let length = t[0] as usize;

    let (i, t) = many_m_n(length, length, term)(i)?;

    if t.len() == 2 {
        Ok((
            i,
            Value::Pair(Box::new(t[0].clone()), Box::new(t[1].clone())),
        ))
    } else {
        Ok((i, Value::Tuple(t)))
    }
}
fn atom_deprecated(input: &[u8]) -> IResult<&[u8], Value> {
    let (i, t) = preceded(tag(&[ATOM_EXT_DEPRECATED]), take(2usize))(input)?;

    let new_int: [u8; 2] = t.try_into().unwrap();
    let length = u16::from_be_bytes(new_int) as usize;

    let (i, t) = take(length)(i)?;

    Ok((
        i,
        Value::Atom(String::from_utf8(t.to_vec()).expect("atom name was not valid")),
    ))
}
fn list(input: &[u8]) -> IResult<&[u8], Value> {
    let (i, t) = preceded(tag(&[LIST_EXT]), take(4usize))(input)?;

    let new_int: [u8; 4] = t.try_into().unwrap();
    let length = u32::from_be_bytes(new_int) as usize;

    let (i, mut t) = many_m_n(length + 1, length + 1, term)(i)?;

    if let Some(x) = t.pop() {
        match x {
            Value::List(y) => {
                if !y.is_empty() {
                    t.push(Value::Improper(Box::new(Value::List(y))))
                }
            }
            y => t.push(Value::Improper(Box::new(y))),
        }
    }

    if t.iter().all(|x| x.is_atom_pair()) {
        let list = HashMap::from_iter(t.iter().map(|x| x.as_atom_pair().unwrap()));
        Ok((i, Value::Keyword(list)))
    } else {
        Ok((i, Value::List(t)))
    }
}

fn string(input: &[u8]) -> IResult<&[u8], Value> {
    let (i, t) = preceded(tag(&[BINARY_EXT]), take(4usize))(input)?;

    let new_int: [u8; 4] = t.try_into().unwrap();
    let length = u32::from_be_bytes(new_int) as usize;

    let (i, t) = take(length)(i)?;
    Ok((
        i,
        Value::String(String::from_utf8(t.to_vec()).expect("invalid string")),
    ))
}

fn charlist(input: &[u8]) -> IResult<&[u8], Value> {
    let (i, t) = preceded(tag(&[STRING_EXT]), take(2usize))(input)?;

    let new_int: [u8; 2] = t.try_into().unwrap();
    let length = u16::from_be_bytes(new_int) as usize;

    let (i, t) = take(length)(i)?;
    Ok((i, Value::Charlist(t.to_vec())))
}

fn small_int(input: &[u8]) -> IResult<&[u8], Value> {
    let (i, t) = preceded(tag(&[SMALL_INTEGER_EXT]), take(1usize))(input)?;
    Ok((i, Value::SmallInt(t[0])))
}

fn int(input: &[u8]) -> IResult<&[u8], Value> {
    let (i, t) = preceded(tag(&[INTEGER_EXT]), take(4usize))(input)?;
    let new_int: [u8; 4] = t.try_into().unwrap();
    Ok((i, Value::Int(u32::from_be_bytes(new_int))))
}

fn float(input: &[u8]) -> IResult<&[u8], Value> {
    let (i, t) = preceded(tag(&[NEW_FLOAT_EXT]), take(8usize))(input)?;
    let new_float: [u8; 8] = t.try_into().unwrap();
    Ok((i, Value::Float(f64::from_be_bytes(new_float))))
}

fn empty_list(input: &[u8]) -> IResult<&[u8], Value> {
    // alt((alpha1, digit1))(input)
    let (i, _) = tag(&[NIL_EXT])(input)?;

    Ok((i, Value::List(vec![])))
}

fn map(input: &[u8]) -> IResult<&[u8], Value> {
    let (i, t) = preceded(tag(&[MAP_EXT]), take(4usize))(input)?;
    let new_int: [u8; 4] = t.try_into().unwrap();
    let length = u32::from_be_bytes(new_int) as usize;

    let (i, t) = many_m_n(2 * length, 2 * length, term)(i)?;

    println!("{:?}", t);

    let mut keyword: Vec<(Value, Value)> = Vec::new();

    // t.chunks(2).map(|[k, v]| (k.clone(), v.clone())).collect();
    let mut all_strings = true;
    for ch in t.chunks(2) {
        all_strings = all_strings && ch[0].is_string_like();
        keyword.push((ch[0].clone(), ch[1].clone()));
    }

    if all_strings {
        let map = HashMap::from_iter(
            keyword
                .into_iter()
                .map(|(k, v)| (k.as_string().unwrap(), v)),
        );
        Ok((i, Value::Map(map)))
    } else {
        Ok((i, Value::MapArbitrary(keyword)))
    }
}

// fn skip_first(input: &[u8]) -> IResult<&[u8], &[u8]> {
//     // alt((alpha1, digit1))(input)
//     let (i, _) = tag(&[131u8])(input)?;

//     Ok((i, &[]))
// }

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use crate::{from_term, read_binary, Value};

    #[test]
    fn small_int() {
        let input = read_binary("bins/small_int.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(vec![Value::SmallInt(2)], out);
    }

    #[test]
    fn int() {
        let input = read_binary("bins/int.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(vec![Value::Int(1234578)], out);
    }

    #[test]
    fn nil() {
        let input = read_binary("bins/nil.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(vec![Value::Atom("nil".to_string())], out);
    }

    #[test]
    fn false_test() {
        let input = read_binary("bins/false.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(vec![Value::Atom("false".to_string())], out);
    }

    #[test]
    fn true_test() {
        let input = read_binary("bins/true.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(vec![Value::Atom("true".to_string())], out);
    }

    #[test]
    fn odd_atom() {
        let input = read_binary("bins/odd_atom.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(vec![Value::Atom("oddţ".to_string())], out);
    }

    #[test]
    fn module_name() {
        let input = read_binary("bins/module_name.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(vec![Value::Atom("Elixir.TermGenerator".to_string())], out);
    }

    #[test]
    fn small_string() {
        let input = read_binary("bins/small_string.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(vec![Value::String("just some text".to_string())], out);
    }

    #[test]
    fn large_string() {
        let input = read_binary("bins/large_string.bin").unwrap();
        let out = from_term(&input).unwrap();

        if let Value::String(x) = &out[0] {
            assert!(x.starts_with("Lorem ipsum dolor sit"))
        } else {
            assert!(false)
        }
    }

    #[test]
    fn float() {
        let input = read_binary("bins/float.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(vec![Value::Float(12.515)], out);
    }

    #[test]
    fn empty_list() {
        let input = read_binary("bins/empty_list.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(vec![Value::List(vec![])], out);
    }

    #[test]
    fn number_list() {
        let input = read_binary("bins/number_list.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(vec![Value::Charlist(vec![1, 2, 3, 4])], out);
    }

    #[test]
    fn mixed_list() {
        use crate::Value::*;

        let input = read_binary("bins/mixed_list.bin").unwrap();
        let out = from_term(&input).unwrap();

        assert_eq!(
            vec![List(vec![
                SmallInt(1),
                String("some".to_string()),
                SmallInt(2),
                String("text".to_string())
            ])],
            out
        );
    }

    #[test]
    fn improper_list() {
        use crate::Value::*;

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

        let mut map = HashMap::new();

        map.insert(String::from("just"), Value::String("some key".to_string()));
        map.insert(String::from("other"), Value::String("value".to_string()));

        assert_eq!(vec![Value::Map(map)], out);
    }

    #[test]
    fn keyword() {
        let input = read_binary("bins/keyword.bin").unwrap();
        println!("{:?}", input);
        let out = from_term(&input).unwrap();

        let mut map = HashMap::new();

        map.insert(String::from("just"), Value::String("some key".to_string()));
        map.insert(String::from("other"), Value::String("value".to_string()));

        assert_eq!(vec![Value::Keyword(map)], out);
    }

    #[test]
    fn tuple() {
        let input = read_binary("bins/tuple.bin").unwrap();
        let out = from_term(&input).unwrap();
        assert_eq!(
            vec![Value::Pair(
                Box::new(Value::String("test".to_string())),
                Box::new(Value::String("testing".to_string()))
            )],
            out
        );
    }
}
