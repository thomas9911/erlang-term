// http://erlang.org/doc/apps/erts/erl_ext_dist.html#distribution-header
// https://godoc.org/github.com/goerlang/etf#Term

// #[macro_use]
extern crate nom;
extern crate num_bigint;
extern crate num_traits;
#[macro_use]
extern crate serde_derive;

use std::fs::File;
use std::io::{Read, Result as IoResult};

pub mod consts;
pub mod dump;
pub mod parse;
pub mod raw_term;
pub mod serde;
pub mod term;

pub use dump::to_binary;
pub use parse::from_term;
pub use raw_term::RawTerm;
pub use term::Term;

pub fn read_binary(input: &str) -> IoResult<Vec<u8>> {
    let mut file = File::open(input)?;
    let mut buffer = Vec::new();

    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}
