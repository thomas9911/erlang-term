use crate::consts::*;
use crate::RawTerm;

use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::combinator::all_consuming;
use nom::error::ErrorKind;
use nom::multi::{many0, many_m_n};
use nom::sequence::{preceded, tuple};
use nom::{Err as NomErr, IResult};
use num_bigint::{BigInt, Sign};

use std::convert::TryInto;

pub fn from_term(input: &[u8]) -> Result<RawTerm, NomErr<(&[u8], ErrorKind)>> {
    let (_, mut output) = parser(input)?;
    // many0(small_int)(input)

    Ok(output.remove(0))
}

pub fn parser(input: &[u8]) -> IResult<&[u8], Vec<RawTerm>> {
    // pub fn parser(input: &[u8]) -> Result<Vec<RawTerm>, NomErr<(&[u8], ErrorKind)>> {
    // pub fn parser(input: &[u8]) -> Result<Vec<RawTerm>, &[u8]> {
    all_consuming(preceded(tag(&[REVISION]), many0(term)))(input)
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

fn small_int_or_int(input: &[u8]) -> IResult<&[u8], RawTerm> {
    alt((small_int, int))(input)
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
    let new_int = slice_to_i32(t);

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
