# erlang-term

Library to convert Erlang External Term Format to Rust objects, without using erlang NIFs.

## Installation

```toml
[dependencies]
erlang-term = "1.0.0"
```

## Usage

```sh
elixir -e ":erlang.term_to_binary([1,2,3,4]) |> IO.inspect()" | sed -e 's/<</[/g' | sed -e 's/>>/]/g'
```

Copy that over to rust

```rust
use erlang_term::Term;

let input = &[131, 107, 0, 4, 1, 2, 3, 4];
let term = Term::from_bytes(input);
assert_eq!(Ok(Term::Charlist([1, 2, 3, 4].to_vec())), term);
```

Or if you want more controle over the external term format:

```rust
use erlang_term::RawTerm;

let input = &[131, 107, 0, 4, 1, 2, 3, 4];

let term = RawTerm::from_bytes(input);
assert_eq!(Ok(RawTerm::String([1, 2, 3, 4].to_vec())), term);
```

The `Term` enum is more what you will find in elixir.
The `RawTerm` enum takes the naming from the external term format spec.

This is already apparent in the above example. In Erlang a string is just a list of characters and in Elixir this is called a Charlist.

In this library I tried to split logic and convertion, so in the `RawTerm` there is only conversion between binary and rust enum and in the `Term` there is logic to convert that to a usable interface.
Therefore `RawTerm` to binary is one-to-one and onto. But `Term` to `RawTerm` there will be information thrown away.

## Features

There is an optional `serde` feature.

```toml
erlang-term = {version = "1.0.0", features = ["serde_impl"]}
```

There is an optional `zlib` feature, that allows the etf to be compressed. In Elixir:

```elixir
:erlang.term_to_binary(t, [:compressed])
# or
:erlang.term_to_binary(t, compressed: 6)
```

```toml
erlang-term = {version = "1.0.0", features = ["zlib"]}
```

## More examples

```rust
use erlang_term::RawTerm;
use std::iter::FromIterator;

let map = RawTerm::Map(vec![
    (RawTerm::SmallAtom(String::from("test")), RawTerm::SmallTuple(vec![RawTerm::SmallAtom(String::from("ok")), RawTerm::SmallInt(15)])),
    (RawTerm::SmallAtom(String::from("another_key")), RawTerm::Binary(b"this is a string".to_vec())),
    (RawTerm::SmallAtom(String::from("number")), RawTerm::Float(3.1415.into())),
]);

let binary = vec![
    131, 116, 0, 0, 0, 3, 119, 4, 116, 101, 115, 116, 104, 2, 119, 2, 111, 107, 97, 15, 119, 11, 97, 110, 111, 116, 104, 101, 114, 95, 107, 101, 121, 109, 0, 0, 0, 16, 116, 104, 105, 115, 32, 105, 115, 32, 97, 32, 115, 116, 114, 105, 110, 103, 119, 6, 110, 117, 109, 98, 101, 114, 70, 64, 9, 33, 202, 192, 131, 18, 111
];

assert_eq!(map.to_bytes(), binary);
```

## Use Cases

- Filter out non-data (such as references and functions) from stored etf files

- Convert stored etf files to json

See the [filter example](./examples/filter.rs)

License: Unlicense
