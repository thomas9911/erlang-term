use crate::{RawTerm, Term};
use rustler::types::tuple::{get_tuple, make_tuple};
use rustler::{Decoder, Encoder, Error, TermType};

impl<'a> Decoder<'a> for Term {
    fn decode(term: rustler::Term<'a>) -> Result<Self, Error> {
        Ok(Term::from(term.decode::<RawTerm>()?))
    }
}

impl<'a> Decoder<'a> for RawTerm {
    fn decode(term: rustler::Term<'a>) -> Result<Self, Error> {
        use TermType::*;

        let result = match term.get_type() {
            Atom => RawTerm::Atom(term.atom_to_string()?),
            Binary => {
                let binary = term.into_binary()?;
                RawTerm::Binary(binary.as_slice().to_vec())
            }
            EmptyList => RawTerm::Nil,
            List => {
                let list: Vec<_> = term
                    .into_list_iterator()?
                    .map(|x| x.decode())
                    .collect::<Result<Vec<Self>, Error>>()?;
                RawTerm::List(list)
            }
            Map | Exception => {
                let map = term
                    .decode::<rustler::types::MapIterator>()?
                    .map(|(k, v)| Ok((k.decode()?, v.decode()?)))
                    .collect::<Result<Vec<(Self, Self)>, Error>>()?;
                RawTerm::Map(map)
            }
            Number => raw_term_decode_number(term)?,
            Tuple => {
                let tuple: Vec<_> = get_tuple(term)?
                    .into_iter()
                    .map(|x| x.decode())
                    .collect::<Result<Vec<Self>, Error>>()?;
                RawTerm::LargeTuple(tuple)
            }
            Fun | Pid | Port | Ref | Unknown => return Err(Error::Atom("invalid_term")),
        };

        Ok(result)
    }
}

fn raw_term_decode_number<'a>(term: rustler::Term<'a>) -> Result<RawTerm, Error> {
    // if let Ok(bit) = term.decode::<u8>() {
    //     Ok(RawTerm::SmallInt(bit))
    // } else 
    if let Ok(integer) = term.decode::<i32>() {
        if integer < 256 && integer >= 0{
            Ok(RawTerm::SmallInt(integer as u8))
        } else {
            Ok(RawTerm::Int(integer))
        }
    } else if let Ok(float) = term.decode::<f64>() {
        Ok(RawTerm::Float(float))
    } else {
        Ok(RawTerm::LargeBigInt(
            format!("{:?}", term)
                .parse()
                .map_err(|_| Error::Atom("invalid_number"))?,
        ))
    }
}
