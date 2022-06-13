//! Library to convert Erlang External Term Format to Rust objects, without using erlang NIFs.
//!
//!
//! ```sh
//! elixir -e ":erlang.term_to_binary([1,2,3,4]) |> IO.inspect()" | sed -e 's/<</[/g' | sed -e 's/>>/]/g'
//! ```
//!
//! Copy that over to rust
//!
//! ```
//! use erlang_term::Term;
//!
//! let input = &[131, 107, 0, 4, 1, 2, 3, 4];
//! let term = Term::from_bytes(input);
//! assert_eq!(Ok(Term::Charlist([1, 2, 3, 4].to_vec())), term);
//! ```
//!
//! Or if you want more controle over the external term format:
//! ```
//! use erlang_term::RawTerm;
//!
//! let input = &[131, 107, 0, 4, 1, 2, 3, 4];
//!
//! let term = RawTerm::from_bytes(input);
//! assert_eq!(Ok(RawTerm::String([1, 2, 3, 4].to_vec())), term);
//! ```
//!
//! The `Term` enum is more what you will find in elixir.
//! The `RawTerm` enum takes the naming from the external term format spec.
//!
//! This is already apparent in the above example. In Erlang a string is just a list of characters and in Elixir this is called a Charlist.
//!
//! In this library I tried to split logic and convertion, so in the `RawTerm` there is only conversion between binary and rust enum and in the `Term` there is logic to convert that to a usable interface.
//! Therefore `RawTerm` to binary is one-to-one and onto. But `Term` to `RawTerm` there will be information thrown away.
//!
//! There is an optional `serde` feature.
//!
//! More examples:
//!
//! ```
//! use erlang_term::RawTerm;
//! use std::iter::FromIterator;
//!
//! let map = RawTerm::Map(vec![
//!     (RawTerm::SmallAtom(String::from("test")), RawTerm::SmallTuple(vec![RawTerm::SmallAtom(String::from("ok")), RawTerm::SmallInt(15)])),    
//!     (RawTerm::SmallAtom(String::from("another_key")), RawTerm::Binary(b"this is a string".to_vec())),
//!     (RawTerm::SmallAtom(String::from("number")), RawTerm::Float(3.1415)),
//! ]);
//!
//! let binary = vec![
//!     131, 116, 0, 0, 0, 3, 119, 4, 116, 101, 115, 116, 104, 2, 119, 2, 111, 107, 97, 15, 119, 11, 97, 110, 111, 116, 104, 101, 114, 95, 107, 101, 121, 109, 0, 0, 0, 16, 116, 104, 105, 115, 32, 105, 115, 32, 97, 32, 115, 116, 114, 105, 110, 103, 119, 6, 110, 117, 109, 98, 101, 114, 70, 64, 9, 33, 202, 192, 131, 18, 111
//! ];
//!
//! assert_eq!(map.to_bytes(), binary);
//! ```
//!

// http://erlang.org/doc/apps/erts/erl_ext_dist.html#distribution-header
// https://godoc.org/github.com/goerlang/etf#Term

extern crate nom;
extern crate num_bigint;
extern crate num_traits;
#[cfg(feature = "serde_impl")]
#[macro_use]
extern crate serde_derive;

use std::fs::File;
use std::io::{Read, Result as IoResult};

pub mod consts;
pub mod dump;
pub mod parse;
pub mod raw_term;
pub mod term;

pub use dump::to_bytes;
#[cfg(feature = "zlib")]
pub use dump::to_gzip_bytes;
pub use parse::from_bytes;
pub use raw_term::RawTerm;
pub use term::Term;

pub fn read_binary(input: &str) -> IoResult<Vec<u8>> {
    let mut file = File::open(input)?;
    let mut buffer = Vec::new();

    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}
