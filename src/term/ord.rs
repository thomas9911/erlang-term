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
            RawTermGeneralType::Pid => {
                pid_cmp(self, other).or_else(|| pid_cmp(other, self).map(Ordering::reverse))
            }
            RawTermGeneralType::BitString => bitstring_cmp(self, other),
            RawTermGeneralType::List => list_cmp(self, other),
            // Fun,
            // Port,
            // Tuple,
            // Map,
            // Nil,
            // List,
            // BitString,
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

fn ref_cmp(left: &RawTerm, right: &RawTerm) -> Option<Ordering> {
    use RawTerm::*;

    match (left, right) {
        (
            Ref {
                creation: creation_a,
                id: a,
                ..
            },
            Ref {
                creation: creation_b,
                id: b,
                ..
            },
        ) => {
            if creation_a == creation_b {
                a.partial_cmp(b)
            } else {
                creation_a.partial_cmp(creation_b)
            }
        }
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
        ) if (creation_a == creation_b) && (id_a == id_b) => serial_a.partial_cmp(serial_b),
        (
            NewPid {
                id: id_a,
                creation: creation_a,
                ..
            },
            NewPid {
                id: id_b,
                creation: creation_b,
                ..
            },
        ) if (creation_a == creation_b) => id_a.partial_cmp(id_b),
        (
            NewPid {
                creation: creation_a,
                ..
            },
            NewPid {
                creation: creation_b,
                ..
            },
        ) => creation_a.partial_cmp(creation_b),
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
        ) if (&(*creation_a as u32) == creation_b) && (id_a == id_b) => {
            serial_a.partial_cmp(serial_b)
        }
        (
            Pid {
                id: id_a,
                creation: creation_a,
                ..
            },
            NewPid {
                id: id_b,
                creation: creation_b,
                ..
            },
        ) if (&(*creation_a as u32) == creation_b) => id_a.partial_cmp(id_b),
        (
            Pid {
                creation: creation_a,
                ..
            },
            NewPid {
                creation: creation_b,
                ..
            },
        ) => (*creation_a as u32).partial_cmp(creation_b),
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
        ) if (creation_a == creation_b) && (id_a == id_b) => serial_a.partial_cmp(serial_b),
        (
            Pid {
                id: id_a,
                creation: creation_a,
                ..
            },
            Pid {
                id: id_b,
                creation: creation_b,
                ..
            },
        ) if (creation_a == creation_b) => id_a.partial_cmp(id_b),
        (
            Pid {
                creation: creation_a,
                ..
            },
            Pid {
                creation: creation_b,
                ..
            },
        ) => creation_a.partial_cmp(creation_b),
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

#[test]
fn xd() {
    let bytes = [
        131, 88, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0,
        0, 110, 0, 0, 0, 1, 0, 0, 0, 0,
    ];
    let term = RawTerm::from_bytes(&bytes);

    dbg!(term);

    // dbg!(1.partial_cmp(&0));
    // dbg!(u8::MIN);

    panic!()
}
