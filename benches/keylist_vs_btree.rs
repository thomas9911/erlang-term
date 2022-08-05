use brunch::Bench;

use std::iter::FromIterator;

use erlang_term::Term;
use keylist::Keylist;
use std::collections::{BTreeMap, HashMap};
use std::time::Duration;

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

brunch::benches!(
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
);
