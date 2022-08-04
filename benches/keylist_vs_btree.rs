// #[macro_use]
// extern crate bencher;

// use bencher::Bencher;

use brunch::Bench;

use std::iter::FromIterator;

use erlang_term::Term;
use keylist::Keylist;
use std::collections::{BTreeMap, HashMap};
use std::time::Duration;

// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
// enum SubTerm {
//     String(String),
//     Int(u32),
//     Tuple(Box<SubTerm>, Box<SubTerm>),
// }

// impl SubTerm {
//     fn from_term(term: Term) -> Option<SubTerm> {
//         match term {
//             Term::Byte(x) => Some(SubTerm::Int(x as u32)),
//             Term::Int(x) => Some(SubTerm::Int(x as u32)),
//             Term::String(x) => Some(SubTerm::String(x)),
//             Term::Atom(x) => Some(SubTerm::String(x)),
//             Term::Tuple(mut x) if x.len() == 2 => {
//                 let b = x.pop()?;
//                 let a = x.pop()?;

//                 Some(SubTerm::Tuple(
//                     Box::new(SubTerm::from_term(a)?),
//                     Box::new(SubTerm::from_term(b)?),
//                 ))
//             }
//             _ => None,
//         }
//     }
// }

// impl From<u32> for SubTerm {
//     fn from(input: u32) -> SubTerm {
//         SubTerm::Int(input)
//     }
// }

// impl From<String> for SubTerm {
//     fn from(input: String) -> SubTerm {
//         SubTerm::String(input)
//     }
// }

// impl From<(String, u32)> for SubTerm {
//     fn from((a, b): (String, u32)) -> SubTerm {
//         SubTerm::Tuple(Box::new(SubTerm::from(a)), Box::new(SubTerm::from(b)))
//     }
// }

fn data() -> Vec<(Term, Term)> {
    let mut data = Vec::new();

    for i in 100..12000 {
        data.push((Term::from((i.to_string(), i)), Term::from(i.to_string())))
    }

    for i in 100..12000 {
        data.push((Term::from(i), Term::from(i.to_string())))
    }

    for i in 100..12000 {
        data.push((Term::from(i.to_string()), Term::from(i.to_string())))
    }

    for i in 100..12000 {
        data.push((one_elem_map(i), Term::from("data")))
    }

    data
}

fn one_elem_map(i: u32) -> Term {
    Term::from(vec![(Term::from(i), Term::from(i.to_string()))])
}

fn data_btreemap() -> BTreeMap<Term, Term> {
    BTreeMap::from_iter(data())
}

fn data_keylist() -> Keylist<Term, Term> {
    Keylist::from(data())
}

fn data_hashmap() -> HashMap<Term, Term> {
    HashMap::from_iter(data())
}

// fn keylist_integer(bench: &mut Bencher) {
//     let data = data_keylist();
//     bench.iter(|| {
//         data.get(&Term::from(123))
//     })
// }

// fn map_integer(bench: &mut Bencher) {
//     let data = data_btreemap();
//     bench.iter(|| {
//         data.get(&Term::from(123))
//     });
// }

// fn keylist_string(bench: &mut Bencher) {
//     let data = data_keylist();
//     bench.iter(|| {
//         data.get(&Term::from("123"))
//     })
// }

// fn map_string(bench: &mut Bencher) {
//     let data = data_btreemap();
//     bench.iter(|| {
//         data.get(&Term::from("123"))
//     });
// }

// benchmark_group!(integer, keylist_integer, map_integer);
// benchmark_group!(string, keylist_string, map_string);
// benchmark_main!(integer, string);

// brunch::benches!(
//     Bench::new("integer", "keylist")
//         .timed(Duration::from_secs(1))
//         .with_setup_ref(data_keylist(), |data| {
//             data.get(&Term::from(123));
//         }),
//     Bench::new("integer", "map")
//         .timed(Duration::from_secs(1))
//         .with_setup_ref(data_btreemap(), |data| {
//             data.get(&Term::from(123));
//         }),
//     Bench::new("integer", "hashmap")
//         .timed(Duration::from_secs(1))
//         .with_setup_ref(data_hashmap(), |data| {
//             data.get(&Term::from(123));
//         }),
//     Bench::spacer(),
//     Bench::new("string", "keylist")
//         .timed(Duration::from_secs(1))
//         .with_setup_ref(data_keylist(), |data| {
//             data.get(&Term::from("123"));
//         }),
//     Bench::new("string", "map")
//         .timed(Duration::from_secs(1))
//         .with_setup_ref(data_btreemap(), |data| {
//             data.get(&Term::from("123"));
//         }),
//     Bench::new("string", "hashmap")
//         .timed(Duration::from_secs(1))
//         .with_setup_ref(data_hashmap(), |data| {
//             data.get(&Term::from("123"));
//         })
// );

fn main() {
    // println!("{:?}", data_hashmap());

    // This can take a while; give 'em a message of hope.
    ::std::print!("\x1b[1;38;5;199mStarting:\x1b[0m Running benchmark(s). Stand by!\n\n");

    // Run the benches.
    let mut benches = vec![
        Bench::new("integer", "keylist")
            .timed(Duration::from_secs(1))
            .with_setup_ref(data_keylist(), |data| {
                data.get(&Term::from(123));
            }),
        Bench::new("integer", "btreemap")
            .timed(Duration::from_secs(1))
            .with_setup_ref(data_btreemap(), |data| {
                data.get(&Term::from(123));
            }),
        Bench::new("integer", "hashmap")
            .timed(Duration::from_secs(1))
            .with_setup_ref(data_hashmap(), |data| {
                data.get(&Term::from(123));
            }),
        Bench::spacer(),
        Bench::new("string", "keylist")
            .timed(Duration::from_secs(1))
            .with_setup_ref(data_keylist(), |data| {
                data.get(&Term::from("123"));
            }),
        Bench::new("string", "btreemap")
            .timed(Duration::from_secs(1))
            .with_setup_ref(data_btreemap(), |data| {
                data.get(&Term::from("123"));
            }),
        Bench::new("string", "hashmap")
            .timed(Duration::from_secs(1))
            .with_setup_ref(data_hashmap(), |data| {
                data.get(&Term::from("123"));
            }),
        Bench::spacer(),
        Bench::new("map", "keylist")
            .timed(Duration::from_secs(1))
            .with_setup_ref(data_keylist(), |data| {
                data.get(&one_elem_map(123));
            }),
        Bench::new("map", "btreemap")
            .timed(Duration::from_secs(1))
            .with_setup_ref(data_btreemap(), |data| {
                data.get(&one_elem_map(123));
            }),
        Bench::new("map", "hashmap")
            .timed(Duration::from_secs(1))
            .with_setup_ref(data_hashmap(), |data| {
                data.get(&one_elem_map(123));
            }),
    ];

    brunch::analyze(&mut benches);
}
