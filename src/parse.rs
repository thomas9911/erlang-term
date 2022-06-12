use crate::consts::*;
use crate::RawTerm;
use nom::Err;

#[cfg(feature = "zlib")]
use flate2::Decompress;
use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::combinator::all_consuming;
use nom::error::Error;
use nom::error::ErrorKind;
use nom::multi::{many0, many_m_n};
use nom::sequence::{preceded, tuple};
use nom::{Err as NomErr, IResult};
use num_bigint::{BigInt, Sign};

use std::collections::BTreeMap;
use std::convert::TryInto;

pub fn from_bytes(input: &[u8]) -> Result<RawTerm, NomErr<Error<&[u8]>>> {
    let (_, mut output) = parser(input)?;

    Ok(output.remove(0))
}

pub fn parser(input: &[u8]) -> IResult<&[u8], Vec<RawTerm>> {
    all_consuming(preceded(tag(&[REVISION]), many0(term)))(input)
}

#[cfg(not(feature = "zlib"))]
fn term(input: &[u8]) -> IResult<&[u8], RawTerm> {
    if input.is_empty() {
        return Err(Err::Error(nom::error::Error::new(input, ErrorKind::Eof)));
    }
    let funcs = match input[0] {
        SMALL_INTEGER_EXT => (small_int),
        INTEGER_EXT => (int),
        NEW_FLOAT_EXT => (float),
        ATOM_EXT_DEPRECATED => (atom_deprecated),
        SMALL_ATOM_UTF8_EXT => (small_atom),
        NIL_EXT => (empty_list),
        BINARY_EXT => (binary),
        STRING_EXT => (string),
        LIST_EXT => (list),
        MAP_EXT => (map),
        SMALL_TUPLE_EXT => (small_tuple),
        LARGE_TUPLE_EXT => (large_tuple),
        SMALL_BIG_EXT => (small_big_int),
        LARGE_BIG_EXT => (large_big_int),
        PID_EXT => (pid),
        NEW_PID_EXT => (new_pid),
        PORT_EXT => (port),
        NEW_PORT_EXT => (new_port),
        NEW_REFERENCE_EXT => (reference_new),
        NEWER_REFERENCE_EXT => (reference_newer),
        NEW_FUN_EXT => (function),
        _ => {
            return Err(Err::Error(nom::error::Error::new(input, ErrorKind::NoneOf)));
        }
    };
    funcs(input)
}

#[cfg(feature = "zlib")]
fn term(input: &[u8]) -> IResult<&[u8], RawTerm> {
    if input.is_empty() {
        return Err(Err::Error(nom::error::Error::new(input, ErrorKind::Eof)));
    }
    let funcs = match input[0] {
        SMALL_INTEGER_EXT => (small_int),
        INTEGER_EXT => (int),
        NEW_FLOAT_EXT => (float),
        ATOM_EXT_DEPRECATED => (atom_deprecated),
        SMALL_ATOM_UTF8_EXT => (small_atom),
        NIL_EXT => (empty_list),
        BINARY_EXT => (binary),
        STRING_EXT => (string),
        LIST_EXT => (list),
        MAP_EXT => (map),
        SMALL_TUPLE_EXT => (small_tuple),
        LARGE_TUPLE_EXT => (large_tuple),
        SMALL_BIG_EXT => (small_big_int),
        LARGE_BIG_EXT => (large_big_int),
        PID_EXT => (pid),
        NEW_PID_EXT => (new_pid),
        PORT_EXT => (port),
        NEW_PORT_EXT => (new_port),
        NEW_REFERENCE_EXT => (reference_new),
        NEWER_REFERENCE_EXT => (reference_newer),
        NEW_FUN_EXT => (function),
        ZLIB => (gzip),
        _ => {
            return Err(Err::Error(nom::error::Error::new(input, ErrorKind::NoneOf)));
        }
    };
    funcs(input)
}

#[cfg(feature = "zlib")]
fn gzip(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let (i, t) = preceded(tag(&[ZLIB]), take(4usize))(input)?;
    let amount = slice_to_u32(t) as usize;
    let mut decompressor = Decompress::new(true);

    let data = {
        let mut data = Vec::with_capacity(amount);
        decompressor
            .decompress_vec(i, &mut data, flate2::FlushDecompress::None)
            .map_err(|_| NomErr::Incomplete(nom::Needed::Unknown))?;
        data
    };

    // error returns a reference to data owned by this function, which is not allowed
    // map it to a blank error
    let (_a, x) = term(&data).map_err(|_| NomErr::Incomplete(nom::Needed::Unknown))?;
    Ok((&[], x))
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
    Ok((i, RawTerm::SmallTuple(t)))
}
fn large_tuple(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let (i, t) = preceded(tag(&[LARGE_TUPLE_EXT]), take(4usize))(input)?;
    let length = slice_to_u32(t) as usize;
    let (i, t) = many_m_n(length, length, term)(i)?;
    Ok((i, RawTerm::LargeTuple(t)))
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

fn small_int_or_int(input: &[u8]) -> IResult<&[u8], RawTerm> {
    alt((small_int, int))(input)
}
fn pid_or_new_pid(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let func = match input[0] {
        NEW_PID_EXT => new_pid,
        PID_EXT => pid,
        _ => {
            return Err(Err::Error(nom::error::Error::new(input, ErrorKind::NoneOf)));
        }
    };
    func(input)
}
fn new_pid(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let get_data = tuple((node_or_module, take(4usize), take(4usize), take(4usize)));
    let (i, (node, id, serial, creation)) = preceded(tag(&[NEW_PID_EXT]), get_data)(input)?;
    let id = slice_to_u32(id);
    let serial = slice_to_u32(serial);
    let creation = slice_to_u32(creation);

    Ok((
        i,
        RawTerm::NewPid {
            node: Box::new(node),
            id,
            serial,
            creation,
        },
    ))
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

fn new_port(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let get_data = tuple((node_or_module, take(4usize), take(4usize)));

    let (i, (node, id, creation)) = preceded(tag(&[NEW_PORT_EXT]), get_data)(input)?;
    let id = slice_to_u32(id);
    let creation = slice_to_u32(creation);

    Ok((
        i,
        RawTerm::NewPort {
            node: Box::new(node),
            id,
            creation,
        },
    ))
}

fn reference_new(input: &[u8]) -> IResult<&[u8], RawTerm> {
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
            creation: creation,
        },
    ))
}

fn reference_newer(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let get_data = tuple((take(2usize), node_or_module, take(4usize)));
    let (i, (length, node, creation)) = preceded(tag(&[NEWER_REFERENCE_EXT]), get_data)(input)?;
    let length = slice_to_u16(length) as usize;
    let (i, id_bytes) = take(4 * length)(i)?;
    let creation = slice_to_u32(creation);
    let id: Vec<u32> = id_bytes.chunks(4).map(|x| slice_to_u32(x)).collect();
    Ok((
        i,
        RawTerm::NewerRef {
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
        pid_or_new_pid,
    ));

    let (i, (size, arity, uniq, index, num_free, module, old_index, old_uniq, pid)) =
        preceded(tag(&[NEW_FUN_EXT]), get_data)(input)?;
    let size = slice_to_u32(size);
    let arity = slice_to_u8(arity);
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
            old_index: Box::new(old_index),
            old_uniq: Box::new(old_uniq),
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
            y => t.push(RawTerm::Improper(Box::new(y))),
        }
    }

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
    let new_int = slice_to_i32(t);

    Ok((i, RawTerm::Int(new_int)))
}

fn float(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let (i, t) = preceded(tag(&[NEW_FLOAT_EXT]), take(8usize))(input)?;
    let new_float: [u8; 8] = t.try_into().unwrap();
    Ok((i, RawTerm::Float(f64::from_be_bytes(new_float))))
}

fn empty_list(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let (i, _) = tag(&[NIL_EXT])(input)?;

    Ok((i, RawTerm::Nil))
}

fn map(input: &[u8]) -> IResult<&[u8], RawTerm> {
    let (i, t) = preceded(tag(&[MAP_EXT]), take(4usize))(input)?;
    let length = slice_to_u32(t) as usize;

    let (i, t) = many_m_n(2 * length, 2 * length, term)(i)?;

    let mut keyword = BTreeMap::new();

    for ch in t.chunks(2) {
        // all_strings = all_strings && ch[0].is_string_like();
        keyword.insert(ch[0].clone(), ch[1].clone());
    }

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

fn slice_to_i32(input: &[u8]) -> i32 {
    let new_int: [u8; 4] = input.try_into().unwrap();
    i32::from_be_bytes(new_int)
}

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
