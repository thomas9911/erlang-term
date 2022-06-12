use crate::raw_term::{RawTermGeneralType, RawTermType};
use crate::RawTerm;

use std::cmp::{Ord, Ordering};

use num_bigint::BigInt;

// impl Ord for Term {
//     fn cmp(&self, other: &Self) -> Ordering {
//         if self == other {
//             return Ordering::Equal
//         }

//     }
// }

// #[test]
// fn xd() {
//     let x = vec![1, 2] > vec![1, 1, 1];
//     dbg!(x);

//     let x = vec![1, 2] < vec![1, 1, 1];

//     dbg!(x);

//     panic!()
// }

impl RawTermType {
    ///
    /// number < atom < reference < fun < port < pid < tuple < map < nil < list < bit string
    ///
    pub fn type_cmp(&self, other: &Self) -> Option<Ordering> {
        if self == other {
            return Some(Ordering::Equal);
        }

        let left_type = RawTermGeneralType::from(self);
        let right_type = RawTermGeneralType::from(other);

        left_type.partial_cmp(&right_type)
    }
}

impl Eq for RawTerm {}

impl Ord for RawTerm {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).expect("total order not found")
    }
}

impl PartialOrd for RawTerm {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self == other {
            return Some(Ordering::Equal);
        }

        let left_type = self.as_general_type();
        let right_type = other.as_general_type();

        if left_type != right_type {
            return left_type.partial_cmp(&right_type);
        }

        // if left_type == RawTermGeneralType::Atom {
        //     return atom_cmp(self, other).or_else(|| atom_cmp(other, self).map(Ordering::reverse));
        // }

        match left_type {
            RawTermGeneralType::Atom => {
                atom_cmp(self, other).or_else(|| atom_cmp(other, self).map(Ordering::reverse))
            }
            RawTermGeneralType::Number => {
                number_cmp(self, other).or_else(|| number_cmp(other, self).map(Ordering::reverse))
            }
            RawTermGeneralType::Reference => ref_cmp(self, other),
            RawTermGeneralType::Pid => pid_cmp(self, other),
            RawTermGeneralType::Port => port_cmp(self, other),
            RawTermGeneralType::BitString => bitstring_cmp(self, other),
            RawTermGeneralType::List => list_cmp(self, other),
            RawTermGeneralType::Tuple => tuple_cmp(self, other),
            RawTermGeneralType::Map => map_cmp(self, other),
            RawTermGeneralType::Fun => func_cmp(self, other),
            RawTermGeneralType::Nil => Some(Ordering::Equal),
            _ => panic!(),
        }
    }
}

fn atom_cmp(left: &RawTerm, right: &RawTerm) -> Option<Ordering> {
    use RawTerm::*;

    match (left, right) {
        (Atom(a), Atom(b)) => a.partial_cmp(b),
        (Atom(a), SmallAtom(b)) => a.partial_cmp(b),
        (Atom(a), AtomDeprecated(b)) => a.partial_cmp(b),
        (Atom(a), SmallAtomDeprecated(b)) => a.partial_cmp(b),
        (SmallAtom(a), SmallAtom(b)) => a.partial_cmp(b),
        (SmallAtom(a), AtomDeprecated(b)) => a.partial_cmp(b),
        (SmallAtom(a), SmallAtomDeprecated(b)) => a.partial_cmp(b),
        (AtomDeprecated(a), AtomDeprecated(b)) => a.partial_cmp(b),
        (AtomDeprecated(a), SmallAtomDeprecated(b)) => a.partial_cmp(b),
        (SmallAtomDeprecated(a), SmallAtomDeprecated(b)) => a.partial_cmp(b),
        _ => None,
    }
}

fn number_cmp(left: &RawTerm, right: &RawTerm) -> Option<Ordering> {
    use std::convert::TryInto;
    use RawTerm::*;

    match (left, right) {
        (SmallInt(a), SmallInt(b)) => a.partial_cmp(b),

        (SmallInt(_a), Int(b)) if b > &(u8::MAX as i32) => Some(Ordering::Less),
        (SmallInt(_a), Int(b)) if b < &(u8::MIN as i32) => Some(Ordering::Greater),
        (SmallInt(a), Int(b)) => a.partial_cmp(&(*b as u8)),

        (SmallInt(_a), SmallBigInt(b)) if b > &BigInt::from(u8::MAX) => Some(Ordering::Less),
        (SmallInt(_a), SmallBigInt(b)) if b < &BigInt::from(u8::MIN) => Some(Ordering::Greater),
        (SmallInt(a), SmallBigInt(b)) => a.partial_cmp(&(b.clone().try_into().unwrap())),

        (SmallInt(_a), LargeBigInt(b)) if b > &BigInt::from(u8::MAX) => Some(Ordering::Less),
        (SmallInt(_a), LargeBigInt(b)) if b < &BigInt::from(u8::MIN) => Some(Ordering::Greater),
        (SmallInt(a), LargeBigInt(b)) => a.partial_cmp(&(b.clone().try_into().unwrap())),

        (Int(a), Int(b)) => a.partial_cmp(b),

        (Int(_a), SmallBigInt(b)) if b > &BigInt::from(i32::MAX) => Some(Ordering::Less),
        (Int(_a), SmallBigInt(b)) if b < &BigInt::from(i32::MIN) => Some(Ordering::Greater),
        (Int(a), SmallBigInt(b)) => a.partial_cmp(&(b.clone().try_into().unwrap())),

        (Int(_a), LargeBigInt(b)) if b > &BigInt::from(i32::MAX) => Some(Ordering::Less),
        (Int(_a), LargeBigInt(b)) if b < &BigInt::from(i32::MIN) => Some(Ordering::Greater),
        (Int(a), LargeBigInt(b)) => a.partial_cmp(&(b.clone().try_into().unwrap())),

        (SmallBigInt(a), SmallBigInt(b)) => a.partial_cmp(b),
        (SmallBigInt(a), LargeBigInt(b)) => a.partial_cmp(b),
        (LargeBigInt(a), LargeBigInt(b)) => a.partial_cmp(b),

        (Float(a), Float(b)) => a.partial_cmp(b),
        _ => None,
    }
}

fn tuple_cmp(left: &RawTerm, right: &RawTerm) -> Option<Ordering> {
    use RawTerm::*;

    match (left, right) {
        (SmallTuple(a), SmallTuple(b)) => a.partial_cmp(b),
        (LargeTuple(a), SmallTuple(b)) => a.partial_cmp(b),
        (SmallTuple(a), LargeTuple(b)) => a.partial_cmp(b),
        (LargeTuple(a), LargeTuple(b)) => a.partial_cmp(b),
        _ => None,
    }
}

fn ref_cmp(left: &RawTerm, right: &RawTerm) -> Option<Ordering> {
    use RawTerm::*;

    match (left, right) {
        (
            Ref {
                creation: creation_a,
                id: id_a,
                ..
            },
            Ref {
                creation: creation_b,
                id: id_b,
                ..
            },
        ) => (creation_a, id_a).partial_cmp(&(creation_b, id_b)),
        (
            NewerRef {
                creation: creation_a,
                id: id_a,
                ..
            },
            NewerRef {
                creation: creation_b,
                id: id_b,
                ..
            },
        ) => (creation_a, id_a).partial_cmp(&(creation_b, id_b)),
        (
            NewerRef {
                creation: creation_a,
                id: id_a,
                ..
            },
            Ref {
                creation: creation_b,
                id: id_b,
                ..
            },
        ) => (creation_a, id_a).partial_cmp(&(&(*creation_b as u32), id_b)),
        (
            Ref {
                creation: creation_a,
                id: id_a,
                ..
            },
            NewerRef {
                creation: creation_b,
                id: id_b,
                ..
            },
        ) => (&(*creation_a as u32), id_a).partial_cmp(&(creation_b, id_b)),
        _ => None,
    }
}

fn pid_cmp(left: &RawTerm, right: &RawTerm) -> Option<Ordering> {
    use RawTerm::*;

    match (left, right) {
        (
            NewPid {
                id: id_a,
                serial: serial_a,
                creation: creation_a,
                ..
            },
            NewPid {
                id: id_b,
                serial: serial_b,
                creation: creation_b,
                ..
            },
        ) => (creation_a, id_a, serial_a).partial_cmp(&(creation_b, id_b, serial_b)),
        (
            Pid {
                id: id_a,
                serial: serial_a,
                creation: creation_a,
                ..
            },
            Pid {
                id: id_b,
                serial: serial_b,
                creation: creation_b,
                ..
            },
        ) => (creation_a, id_a, serial_a).partial_cmp(&(creation_b, id_b, serial_b)),
        (
            NewPid {
                id: id_a,
                serial: serial_a,
                creation: creation_a,
                ..
            },
            Pid {
                id: id_b,
                serial: serial_b,
                creation: creation_b,
                ..
            },
        ) => (creation_a, id_a, serial_a).partial_cmp(&(&(*creation_b as u32), id_b, serial_b)),
        (
            Pid {
                id: id_a,
                serial: serial_a,
                creation: creation_a,
                ..
            },
            NewPid {
                id: id_b,
                serial: serial_b,
                creation: creation_b,
                ..
            },
        ) => (&(*creation_a as u32), id_a, serial_a).partial_cmp(&(creation_b, id_b, serial_b)),
        _ => None,
    }
}

fn port_cmp(left: &RawTerm, right: &RawTerm) -> Option<Ordering> {
    use RawTerm::*;

    match (left, right) {
        (
            NewPort {
                id: id_a,
                creation: creation_a,
                ..
            },
            NewPort {
                id: id_b,
                creation: creation_b,
                ..
            },
        ) => (creation_a, id_a).partial_cmp(&(creation_b, id_b)),
        (
            Port {
                id: id_a,
                creation: creation_a,
                ..
            },
            Port {
                id: id_b,
                creation: creation_b,
                ..
            },
        ) => (creation_a, id_a).partial_cmp(&(creation_b, id_b)),
        (
            NewPort {
                id: id_a,
                creation: creation_a,
                ..
            },
            Port {
                id: id_b,
                creation: creation_b,
                ..
            },
        ) => (creation_a, id_a).partial_cmp(&(&(*creation_b as u32), id_b)),
        (
            Port {
                id: id_a,
                creation: creation_a,
                ..
            },
            NewPort {
                id: id_b,
                creation: creation_b,
                ..
            },
        ) => (&(*creation_a as u32), id_a).partial_cmp(&(creation_b, id_b)),
        _ => None,
    }
}

fn bitstring_cmp(left: &RawTerm, right: &RawTerm) -> Option<Ordering> {
    use RawTerm::*;

    match (left, right) {
        (String(a), String(b)) => a.partial_cmp(b),
        (Binary(a), String(b)) => a.partial_cmp(b),
        (Binary(a), Binary(b)) => a.partial_cmp(b),
        (String(a), Binary(b)) => a.partial_cmp(b),
        _ => None,
    }
}

fn list_cmp(left: &RawTerm, right: &RawTerm) -> Option<Ordering> {
    use RawTerm::*;

    match (left, right) {
        (List(a), List(b)) => a.partial_cmp(b),
        _ => None,
    }
}

fn map_cmp(left: &RawTerm, right: &RawTerm) -> Option<Ordering> {
    use RawTerm::*;

    match (left, right) {
        (Map(a), Map(b)) => a.partial_cmp(b),
        _ => None,
    }
}

fn func_cmp(left: &RawTerm, right: &RawTerm) -> Option<Ordering> {
    // note that this is not the way erlang does it, but it is something
    // If you know how erlang compares functions let me know.
    match (left, right) {
        (
            RawTerm::Function {
                size: a1,
                arity: b1,
                uniq: c1,
                index: d1,
                module: e1,
                old_index: f1,
                old_uniq: g1,
                pid: h1,
                free_var: i1,
            },
            RawTerm::Function {
                size: a2,
                arity: b2,
                uniq: c2,
                index: d2,
                module: e2,
                old_index: f2,
                old_uniq: g2,
                pid: h2,
                free_var: i2,
            },
        ) => {
            (a1, b1, c1, d1, e1, f1, g1, h1, i1).partial_cmp(&(a2, b2, c2, d2, e2, f2, g2, h2, i2))
        }

        _ => None,
    }
}

// fn part(left: &RawTerm, right: &RawTerm) -> Option<Ordering> {
//     use RawTerm::*;

//     match (left, right) {
//         (SmallInt(a), SmallInt(b)) => None,
//         (Int(a), Int(b)) => None,
//         (Float(a), Float(b)) => None,
//         (SmallBigInt(a), SmallBigInt(b)) => None,
//         (LargeBigInt(a), LargeBigInt(b)) => None,

//         (SmallTuple(a), SmallTuple(b)) => None,
//         (LargeTuple(a), LargeTuple(b)) => None,

//         (Map(a), Map(b)) => None,

//         (String(a), String(b)) => None,

//         (List(a), List(b)) => None,
//         (Improper(a), Improper(b)) => None,
//         (Binary(a), Binary(b)) => None,

//         (Pid { id: a, .. }, Pid { id: b, .. }) => a.partial_cmp(b),
//         (Pid { id: a, .. }, NewPid { id: b, .. }) => a.partial_cmp(b),
//         (NewPid { id: a, .. }, Pid { id: b, .. }) => a.partial_cmp(b),
//         (NewPid { id: a, .. }, NewPid { id: b, .. }) => a.partial_cmp(b),

//         (Atom(a), Atom(b)) => a.partial_cmp(b),
//         (Atom(a), SmallAtom(b)) => a.partial_cmp(b),
//         (Atom(a), AtomDeprecated(b)) => a.partial_cmp(b),
//         (Atom(a), SmallAtomDeprecated(b)) => a.partial_cmp(b),
//         (SmallAtom(a), Atom(b)) => a.partial_cmp(b),
//         (SmallAtom(a), SmallAtom(b)) => a.partial_cmp(b),
//         (SmallAtom(a), AtomDeprecated(b)) => a.partial_cmp(b),
//         (SmallAtom(a), SmallAtomDeprecated(b)) => a.partial_cmp(b),
//         (AtomDeprecated(a), Atom(b)) => a.partial_cmp(b),
//         (AtomDeprecated(a), SmallAtom(b)) => a.partial_cmp(b),
//         (AtomDeprecated(a), AtomDeprecated(b)) => a.partial_cmp(b),
//         (AtomDeprecated(a), SmallAtomDeprecated(b)) => a.partial_cmp(b),
//         (SmallAtomDeprecated(a), Atom(b)) => a.partial_cmp(b),
//         (SmallAtomDeprecated(a), SmallAtom(b)) => a.partial_cmp(b),
//         (SmallAtomDeprecated(a), AtomDeprecated(b)) => a.partial_cmp(b),
//         (SmallAtomDeprecated(a), SmallAtomDeprecated(b)) => a.partial_cmp(b),

//         _ => None,
//     }
// }

// #[test]
// fn xd() {
//     let bytes = [
//         131, 88, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0,
//         0, 110, 0, 0, 0, 1, 0, 0, 0, 0,
//     ];
//     let term = RawTerm::from_bytes(&bytes);

//     dbg!(term);

//     // dbg!(1.partial_cmp(&0));
//     // dbg!(u8::MIN);
//     // dbg!((1, 3usize, 1).partial_cmp(&(1, 3usize, 0)));
//     // let a = RawTerm::NewPid{creation: 1, id: 2, serial: 3, node: Box::new(RawTerm::AtomDeprecated("test".to_string()))};
//     // let b = RawTerm::Pid{creation: 1, id: 2, serial: 4, node: Box::new(RawTerm::AtomDeprecated("test".to_string()))};
//     // dbg!(a.partial_cmp(&b));

//     panic!()
// }

#[test]
fn compare_pids() {
    let a = RawTerm::NewPid {
        creation: 1,
        id: 2,
        serial: 3,
        node: Box::new(RawTerm::AtomDeprecated("test".to_string())),
    };
    let b = RawTerm::Pid {
        creation: 1,
        id: 2,
        serial: 4,
        node: Box::new(RawTerm::AtomDeprecated("test".to_string())),
    };
    let c = RawTerm::NewPid {
        creation: 1,
        id: 3,
        serial: 3,
        node: Box::new(RawTerm::AtomDeprecated("test".to_string())),
    };
    let d = RawTerm::Pid {
        creation: 4,
        id: 2,
        serial: 4,
        node: Box::new(RawTerm::AtomDeprecated("test".to_string())),
    };

    assert_eq!(Some(Ordering::Less), a.partial_cmp(&b));
    assert_eq!(Some(Ordering::Greater), c.partial_cmp(&a));
    assert_eq!(Some(Ordering::Greater), d.partial_cmp(&b));
}

#[test]
fn ordering_test() {
    // (RawTerm::Int(2), RawTerm::List(vec![RawTerm::Binary]))

    let mut map = std::collections::BTreeMap::new();

    map.insert(
        RawTerm::Int(2),
        RawTerm::List(vec![RawTerm::Atom(String::from("test"))]),
    );
    map.insert(
        RawTerm::Int(4),
        RawTerm::List(vec![RawTerm::Atom(String::from("testing"))]),
    );
    map.insert(
        RawTerm::Binary(vec![123, 50, 90]),
        RawTerm::List(vec![RawTerm::Atom(String::from("ordering"))]),
    );
    map.insert(
        RawTerm::NewPort {
            creation: 123,
            id: 1,
            node: Box::new(RawTerm::AtomDeprecated(String::from("localhost"))),
        },
        RawTerm::List(vec![RawTerm::Atom(String::from("ordering"))]),
    );

    assert_eq!(
        &RawTerm::List(vec![RawTerm::Atom(String::from("ordering"))]),
        map.get(&RawTerm::Binary(vec![123, 50, 90])).unwrap()
    )
}
