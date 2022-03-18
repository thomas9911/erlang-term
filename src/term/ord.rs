use crate::raw_term::{RawTermGeneralType, RawTermType};
use crate::RawTerm;

use std::cmp::{Ord, Ordering};

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

        if left_type == RawTermGeneralType::Atom {
            return atom_cmp(self, other).or_else(|| atom_cmp(other, self).map(Ordering::reverse));
        }

        panic!()
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

fn part(left: &RawTerm, right: &RawTerm) -> Option<Ordering> {
    use RawTerm::*;

    match (left, right) {
        (SmallInt(a), SmallInt(b)) => None,
        (Int(a), Int(b)) => None,
        (Float(a), Float(b)) => None,
        (SmallBigInt(a), SmallBigInt(b)) => None,
        (LargeBigInt(a), LargeBigInt(b)) => None,

        (SmallTuple(a), SmallTuple(b)) => None,
        (LargeTuple(a), LargeTuple(b)) => None,

        (Map(a), Map(b)) => None,

        (String(a), String(b)) => None,

        (List(a), List(b)) => None,
        (Improper(a), Improper(b)) => None,
        (Binary(a), Binary(b)) => None,

        (Pid { id: a, .. }, Pid { id: b, .. }) => a.partial_cmp(b),
        (Pid { id: a, .. }, NewPid { id: b, .. }) => a.partial_cmp(b),
        (NewPid { id: a, .. }, Pid { id: b, .. }) => a.partial_cmp(b),
        (NewPid { id: a, .. }, NewPid { id: b, .. }) => a.partial_cmp(b),

        (Atom(a), Atom(b)) => a.partial_cmp(b),
        (Atom(a), SmallAtom(b)) => a.partial_cmp(b),
        (Atom(a), AtomDeprecated(b)) => a.partial_cmp(b),
        (Atom(a), SmallAtomDeprecated(b)) => a.partial_cmp(b),
        (SmallAtom(a), Atom(b)) => a.partial_cmp(b),
        (SmallAtom(a), SmallAtom(b)) => a.partial_cmp(b),
        (SmallAtom(a), AtomDeprecated(b)) => a.partial_cmp(b),
        (SmallAtom(a), SmallAtomDeprecated(b)) => a.partial_cmp(b),
        (AtomDeprecated(a), Atom(b)) => a.partial_cmp(b),
        (AtomDeprecated(a), SmallAtom(b)) => a.partial_cmp(b),
        (AtomDeprecated(a), AtomDeprecated(b)) => a.partial_cmp(b),
        (AtomDeprecated(a), SmallAtomDeprecated(b)) => a.partial_cmp(b),
        (SmallAtomDeprecated(a), Atom(b)) => a.partial_cmp(b),
        (SmallAtomDeprecated(a), SmallAtom(b)) => a.partial_cmp(b),
        (SmallAtomDeprecated(a), AtomDeprecated(b)) => a.partial_cmp(b),
        (SmallAtomDeprecated(a), SmallAtomDeprecated(b)) => a.partial_cmp(b),

        _ => None,
    }
}

#[test]
fn xd() {
    let bytes = [
        131, 88, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0,
        0, 105, 0, 0, 0, 3, 0, 0, 0, 0,
    ];
    let term = RawTerm::from_bytes(&bytes);

    dbg!(term);

    panic!()
}
