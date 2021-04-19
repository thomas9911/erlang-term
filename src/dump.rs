use crate::consts::*;
use crate::RawTerm;
use num_bigint::{BigInt, Sign};
pub fn to_bytes(raw: RawTerm) -> Vec<u8> {
    internal_to_binary(raw, true)
}

pub fn internal_to_binary(raw: RawTerm, add_prefix: bool) -> Vec<u8> {
    use RawTerm::*;
    match raw {
        Atom(x) => atom(x, add_prefix),
        SmallAtom(x) => small_atom(x, add_prefix),
        AtomDeprecated(x) => atom_deprecated(x, add_prefix),
        SmallAtomDeprecated(x) => small_atom_deprecated(x, add_prefix),
        Float(x) => float(x, add_prefix),
        Nil => nil(add_prefix),
        SmallInt(x) => small_int(x, add_prefix),
        Int(x) => int(x, add_prefix),
        String(x) => string(x, add_prefix),
        Binary(x) => binary(x, add_prefix),
        SmallBigInt(x) => small_big_int(x, add_prefix),
        LargeBigInt(x) => large_big_int(x, add_prefix),
        SmallTuple(x) => small_tuple(x, add_prefix),
        LargeTuple(x) => large_tuple(x, add_prefix),
        List(x) => list(x, add_prefix),
        Map(x) => map(x, add_prefix),
        Port { node, id, creation } => port(*node, id, creation, add_prefix),
        Ref { node, id, creation } => reference(*node, id, creation, add_prefix),
        Pid {
            node,
            id,
            serial,
            creation,
        } => pid(*node, id, serial, creation, add_prefix),
        Function {
            size,
            arity,
            uniq,
            index,
            module,
            old_index,
            old_uniq,
            pid,
            free_var,
        } => function(
            size, arity, uniq, index, *module, *old_index, *old_uniq, *pid, free_var, add_prefix,
        ),
        Improper(_) => unreachable!(),
    }
}

fn insert_prefix(bytes: &mut Vec<u8>) {
    bytes.insert(0, REVISION);
}

fn push_prefix(bytes: &mut Vec<u8>) {
    bytes.push(REVISION);
}

fn small_atom(raw: String, add_prefix: bool) -> Vec<u8> {
    let mut bytes = raw.into_bytes();
    let length = bytes.len() as u8;
    bytes.insert(0, length);
    bytes.insert(0, SMALL_ATOM_UTF8_EXT);
    if add_prefix {
        insert_prefix(&mut bytes)
    };
    bytes
}

fn atom(raw: String, add_prefix: bool) -> Vec<u8> {
    let bytes = raw.into_bytes();
    let length = bytes.len();
    let mut buffer = Vec::with_capacity(length + 4);

    if add_prefix {
        push_prefix(&mut buffer)
    };
    buffer.push(ATOM_UTF8_EXT);
    buffer.extend(&(length as u16).to_be_bytes());
    buffer.extend(bytes);
    buffer
}

fn small_atom_deprecated(raw: String, add_prefix: bool) -> Vec<u8> {
    let mut bytes = raw.into_bytes();
    let length = bytes.len() as u8;
    bytes.insert(0, length);
    bytes.insert(0, SMALL_ATOM_EXT_DEPRECATED);
    if add_prefix {
        insert_prefix(&mut bytes)
    };
    bytes
}

fn atom_deprecated(raw: String, add_prefix: bool) -> Vec<u8> {
    let bytes = raw.into_bytes();
    let length = bytes.len();
    let mut buffer = Vec::with_capacity(length + 4);

    if add_prefix {
        push_prefix(&mut buffer)
    };
    buffer.push(ATOM_EXT_DEPRECATED);
    buffer.extend(&(length as u16).to_be_bytes());
    buffer.extend(bytes);
    buffer
}

fn float(raw: f64, add_prefix: bool) -> Vec<u8> {
    let mut buffer = Vec::with_capacity(10);

    if add_prefix {
        push_prefix(&mut buffer)
    };
    buffer.push(NEW_FLOAT_EXT);
    buffer.extend(&raw.to_be_bytes());
    buffer
}

fn nil(add_prefix: bool) -> Vec<u8> {
    if add_prefix {
        vec![REVISION, NIL_EXT]
    } else {
        vec![NIL_EXT]
    }
}

fn small_int(raw: u8, add_prefix: bool) -> Vec<u8> {
    if add_prefix {
        vec![REVISION, SMALL_INTEGER_EXT, raw]
    } else {
        vec![SMALL_INTEGER_EXT, raw]
    }
}

fn int(raw: i32, add_prefix: bool) -> Vec<u8> {
    let mut buffer = Vec::with_capacity(6);

    if add_prefix {
        push_prefix(&mut buffer)
    };
    buffer.push(INTEGER_EXT);
    buffer.extend(&raw.to_be_bytes());
    buffer
}

fn string(mut raw: Vec<u8>, add_prefix: bool) -> Vec<u8> {
    let length = raw.len();
    let mut buffer = Vec::with_capacity(length + 4);

    if add_prefix {
        push_prefix(&mut buffer)
    };
    buffer.push(STRING_EXT);
    buffer.extend(&(length as u16).to_be_bytes());
    buffer.extend(raw.drain(..));
    buffer
}

fn binary(mut raw: Vec<u8>, add_prefix: bool) -> Vec<u8> {
    let length = raw.len();
    let mut buffer = Vec::with_capacity(length + 6);

    if add_prefix {
        push_prefix(&mut buffer)
    };
    buffer.push(BINARY_EXT);
    buffer.extend(&(length as u32).to_be_bytes());
    buffer.extend(raw.drain(..));
    buffer
}

fn small_big_int(raw: BigInt, add_prefix: bool) -> Vec<u8> {
    let (sign, mut bytes) = raw.to_bytes_le();
    let sign = sign_to_byte(sign);

    let mut buffer = Vec::with_capacity(bytes.len() + 4);

    if add_prefix {
        push_prefix(&mut buffer)
    };
    buffer.push(SMALL_BIG_EXT);
    buffer.push(bytes.len() as u8);
    buffer.push(sign);
    buffer.extend(bytes.drain(..));
    buffer
}

fn large_big_int(raw: BigInt, add_prefix: bool) -> Vec<u8> {
    let (sign, mut bytes) = raw.to_bytes_le();
    let sign = sign_to_byte(sign);
    let length = bytes.len();

    let mut buffer = Vec::with_capacity(length + 7);

    if add_prefix {
        push_prefix(&mut buffer)
    };
    buffer.push(LARGE_BIG_EXT);
    buffer.extend(&(length as u32).to_be_bytes());
    buffer.push(sign);
    buffer.extend(bytes.drain(..));
    buffer
}

fn small_tuple(raw: Vec<RawTerm>, add_prefix: bool) -> Vec<u8> {
    let arity = raw.len();
    let mut bytes: Vec<u8> = raw
        .into_iter()
        .map(|x| internal_to_binary(x, false))
        .flatten()
        .collect();

    let mut buffer = Vec::with_capacity(bytes.len() + 3);

    if add_prefix {
        push_prefix(&mut buffer)
    };
    buffer.push(SMALL_TUPLE_EXT);
    buffer.push(arity as u8);
    buffer.extend(bytes.drain(..));
    buffer
}

fn large_tuple(raw: Vec<RawTerm>, add_prefix: bool) -> Vec<u8> {
    let arity = raw.len();
    let mut bytes: Vec<u8> = raw
        .into_iter()
        .map(|x| internal_to_binary(x, false))
        .flatten()
        .collect();

    let mut buffer = Vec::with_capacity(bytes.len() + 6);

    if add_prefix {
        push_prefix(&mut buffer)
    };
    buffer.push(LARGE_TUPLE_EXT);
    buffer.extend(&(arity as u32).to_be_bytes());
    buffer.extend(bytes.drain(..));
    buffer
}

fn list(mut raw: Vec<RawTerm>, add_prefix: bool) -> Vec<u8> {
    let tail = raw.pop();
    let tail = if let Some(RawTerm::Improper(x)) = tail {
        vec![*x]
    } else if let Some(x) = tail {
        vec![x, RawTerm::Nil]
    } else {
        vec![RawTerm::Nil]
    };

    raw.extend(tail);
    let arity = raw.len() - 1;
    let mut bytes: Vec<u8> = raw
        .into_iter()
        .map(|x| internal_to_binary(x, false))
        .flatten()
        .collect();

    let mut buffer = Vec::with_capacity(bytes.len() + 6);
    if add_prefix {
        push_prefix(&mut buffer)
    };
    buffer.push(LIST_EXT);
    buffer.extend(&(arity as u32).to_be_bytes());
    buffer.extend(bytes.drain(..));
    buffer
}

fn map(raw: Vec<(RawTerm, RawTerm)>, add_prefix: bool) -> Vec<u8> {
    let arity = raw.len();
    let mut bytes: Vec<u8> = raw
        .into_iter()
        .map(|(a, b)| {
            internal_to_binary(a, false)
                .into_iter()
                .chain(internal_to_binary(b, false))
        })
        .flatten()
        .collect();

    let mut buffer = Vec::with_capacity(bytes.len() + 6);

    if add_prefix {
        push_prefix(&mut buffer)
    };
    buffer.push(MAP_EXT);
    buffer.extend(&(arity as u32).to_be_bytes());
    buffer.extend(bytes.drain(..));
    buffer
}

fn port(node: RawTerm, id: u32, creation: u8, add_prefix: bool) -> Vec<u8> {
    let node_binary = internal_to_binary(node, false);

    let mut buffer = Vec::with_capacity(node_binary.len() + 7);

    if add_prefix {
        push_prefix(&mut buffer)
    };
    buffer.push(PORT_EXT);
    buffer.extend(node_binary);
    buffer.extend(&(id as u32).to_be_bytes());
    buffer.push(creation);
    buffer
}

fn reference(node: RawTerm, id: Vec<u32>, creation: u8, add_prefix: bool) -> Vec<u8> {
    let id_length = id.len();
    let node_binary = internal_to_binary(node, false);
    let mut id_bytes: Vec<u8> = Vec::new();
    for part in id {
        id_bytes.extend(&(part as u32).to_be_bytes());
    }

    let mut buffer = Vec::with_capacity(node_binary.len() + id_bytes.len() + 5);

    if add_prefix {
        push_prefix(&mut buffer)
    };
    buffer.push(NEW_REFERENCE_EXT);
    buffer.extend(&(id_length as u16).to_be_bytes());
    buffer.extend(node_binary);
    buffer.push(creation);
    buffer.extend(id_bytes);
    buffer
}

fn pid(node: RawTerm, id: u32, serial: u32, creation: u8, add_prefix: bool) -> Vec<u8> {
    let node_binary = internal_to_binary(node, false);

    let mut buffer = Vec::with_capacity(node_binary.len() + 11);

    if add_prefix {
        push_prefix(&mut buffer)
    };
    buffer.push(PID_EXT);
    buffer.extend(node_binary);
    buffer.extend(&(id as u32).to_be_bytes());
    buffer.extend(&(serial as u32).to_be_bytes());
    buffer.push(creation);
    buffer
}

fn function(
    size: u32,
    arity: u8,
    uniq: [u8; 16],
    index: u32,
    module: RawTerm,
    old_index: RawTerm,
    old_uniq: RawTerm,
    pid: RawTerm,
    free_var: Vec<RawTerm>,
    add_prefix: bool,
) -> Vec<u8> {
    let free_var_length = free_var.len();
    let module_binary = internal_to_binary(module, false);
    let pid_binary = internal_to_binary(pid, false);
    let old_index = internal_to_binary(old_index, false);
    let old_uniq = internal_to_binary(old_uniq, false);
    let free_var_bytes: Vec<u8> = free_var
        .into_iter()
        .flat_map(|x| internal_to_binary(x, false))
        .collect();

    let mut buffer =
        Vec::with_capacity(module_binary.len() + pid_binary.len() + free_var_bytes.len() + 31);
    if add_prefix {
        push_prefix(&mut buffer)
    };
    buffer.push(NEW_FUN_EXT);
    buffer.extend(&size.to_be_bytes());
    buffer.push(arity);
    buffer.extend(&uniq);
    buffer.extend(&index.to_be_bytes());
    buffer.extend(&(free_var_length as u32).to_be_bytes());
    buffer.extend(module_binary);
    buffer.extend(old_index);
    buffer.extend(old_uniq);
    buffer.extend(pid_binary);
    buffer.extend(free_var_bytes);
    buffer
}

fn sign_to_byte(sign: Sign) -> u8 {
    match sign {
        Sign::Minus => 1,
        _ => 0,
    }
}

#[cfg(test)]
mod binary_tests {
    use crate::consts::REVISION;
    use crate::{read_binary, to_bytes, RawTerm};
    use num_bigint::{BigInt, BigUint};

    #[test]
    fn small_atom() {
        let out = to_bytes(RawTerm::SmallAtom("test".to_string()));
        assert_eq!(out, vec![REVISION, 119, 4, 116, 101, 115, 116])
    }

    #[test]
    fn atom() {
        let out = to_bytes(RawTerm::Atom("test".to_string()));
        assert_eq!(out, vec![REVISION, 118, 0, 4, 116, 101, 115, 116])
    }

    #[test]
    fn small_atom_deprecated() {
        let out = to_bytes(RawTerm::SmallAtomDeprecated("test".to_string()));
        assert_eq!(out, vec![REVISION, 115, 4, 116, 101, 115, 116])
    }

    #[test]
    fn atom_deprecated() {
        let out = to_bytes(RawTerm::AtomDeprecated("test".to_string()));
        assert_eq!(out, vec![REVISION, 100, 0, 4, 116, 101, 115, 116])
    }

    #[test]
    fn float() {
        let out = to_bytes(RawTerm::Float(3.14));
        assert_eq!(out, vec![REVISION, 70, 64, 9, 30, 184, 81, 235, 133, 31])
    }

    #[test]
    fn nil() {
        let out = to_bytes(RawTerm::Nil);
        assert_eq!(out, vec![REVISION, 106])
    }

    #[test]
    fn small_int() {
        let out = to_bytes(RawTerm::SmallInt(4));
        assert_eq!(out, vec![REVISION, 97, 4])
    }

    #[test]
    fn int() {
        let out = to_bytes(RawTerm::Int(-13));
        assert_eq!(out, vec![REVISION, 98, 255, 255, 255, 243]);

        let out = to_bytes(RawTerm::Int(12345));
        assert_eq!(out, vec![REVISION, 98, 0, 0, 48, 57])
    }

    #[test]
    fn string() {
        let out = to_bytes(RawTerm::String(b"testing".to_vec()));
        assert_eq!(
            out,
            vec![REVISION, 107, 0, 7, 116, 101, 115, 116, 105, 110, 103]
        )
    }

    #[test]
    fn binary() {
        let out = to_bytes(RawTerm::Binary(b"testing".to_vec()));
        assert_eq!(
            out,
            vec![REVISION, 109, 0, 0, 0, 7, 116, 101, 115, 116, 105, 110, 103]
        )
    }

    #[test]
    fn small_big_int() {
        let out = to_bytes(RawTerm::SmallBigInt(
            BigInt::parse_bytes(b"123456789123456789123456789", 10).unwrap(),
        ));
        assert_eq!(
            out,
            vec![REVISION, 110, 11, 0, 21, 95, 4, 124, 159, 177, 227, 242, 253, 30, 102]
        )
    }

    #[test]
    fn large_big_int() {
        use num_traits::pow::Pow;

        let expected = read_binary("bins/large_big_int.bin").unwrap();
        let nineninenine = BigUint::parse_bytes(b"999", 10).unwrap();
        let out = to_bytes(RawTerm::LargeBigInt(BigInt::from(
            nineninenine.clone().pow(&nineninenine),
        )));

        assert_eq!(expected, out);
    }

    #[test]
    fn small_tuple() {
        let out = to_bytes(RawTerm::SmallTuple(vec![
            RawTerm::String(b"testing".to_vec()),
            RawTerm::SmallInt(1),
        ]));

        assert_eq!(
            out,
            vec![REVISION, 104, 2, 107, 0, 7, 116, 101, 115, 116, 105, 110, 103, 97, 1]
        )
    }

    #[test]
    fn large_tuple() {
        let out = to_bytes(RawTerm::LargeTuple(vec![
            RawTerm::String(b"testing".to_vec()),
            RawTerm::SmallInt(1),
        ]));

        assert_eq!(
            out,
            vec![REVISION, 105, 0, 0, 0, 2, 107, 0, 7, 116, 101, 115, 116, 105, 110, 103, 97, 1]
        )
    }

    #[test]
    fn list() {
        let out = to_bytes(RawTerm::List(vec![RawTerm::String(b"testing".to_vec())]));

        assert_eq!(
            out,
            vec![REVISION, 108, 0, 0, 0, 1, 107, 0, 7, 116, 101, 115, 116, 105, 110, 103, 106]
        )
    }

    #[test]
    fn improper_list() {
        let out = to_bytes(RawTerm::List(vec![
            RawTerm::String(b"testing".to_vec()),
            RawTerm::Improper(Box::new(RawTerm::SmallInt(3))),
        ]));

        assert_eq!(
            out,
            vec![REVISION, 108, 0, 0, 0, 1, 107, 0, 7, 116, 101, 115, 116, 105, 110, 103, 97, 3]
        )
    }

    #[test]
    fn atom_map() {
        let out = to_bytes(RawTerm::Map(vec![
            (
                RawTerm::AtomDeprecated("other".to_string()),
                RawTerm::Binary(b"test".to_vec()),
            ),
            (
                RawTerm::AtomDeprecated("some".to_string()),
                RawTerm::SmallInt(3),
            ),
        ]));

        assert_eq!(
            out,
            vec![
                REVISION, 116, 0, 0, 0, 2, 100, 0, 5, 111, 116, 104, 101, 114, 109, 0, 0, 0, 4,
                116, 101, 115, 116, 100, 0, 4, 115, 111, 109, 101, 97, 3
            ]
        )
    }

    #[test]
    fn map() {
        use RawTerm::*;
        let mut map = vec![
            (Binary(b"float".to_vec()), Float(3.14)),
            (
                List(vec![Binary(b"list as a key".to_vec())]),
                List(vec![
                    Binary(b"another".to_vec()),
                    Map(vec![(
                        AtomDeprecated("test".to_string()),
                        AtomDeprecated("false".to_string()),
                    )]),
                ]),
            ),
            (SmallInt(1), Binary(b"one".to_vec())),
            (
                AtomDeprecated("tuple".to_string()),
                SmallTuple(vec![SmallInt(1), AtomDeprecated("more".to_string())]),
            ),
            (
                Binary(b"large".to_vec()),
                SmallBigInt(BigInt::parse_bytes(b"123456789123456789", 10).unwrap()),
            ),
            (
                Binary(b"nested".to_vec()),
                Map(vec![(Binary(b"ok".to_vec()), Nil)]),
            ),
        ];
        map.sort_by(|a, b| a.partial_cmp(b).unwrap());
        let out = to_bytes(RawTerm::Map(map));

        assert_eq!(
            out,
            vec![
                REVISION, 116, 0, 0, 0, 6, 97, 1, 109, 0, 0, 0, 3, 111, 110, 101, 108, 0, 0, 0, 1,
                109, 0, 0, 0, 13, 108, 105, 115, 116, 32, 97, 115, 32, 97, 32, 107, 101, 121, 106,
                108, 0, 0, 0, 2, 109, 0, 0, 0, 7, 97, 110, 111, 116, 104, 101, 114, 116, 0, 0, 0,
                1, 100, 0, 4, 116, 101, 115, 116, 100, 0, 5, 102, 97, 108, 115, 101, 106, 109, 0,
                0, 0, 5, 102, 108, 111, 97, 116, 70, 64, 9, 30, 184, 81, 235, 133, 31, 109, 0, 0,
                0, 5, 108, 97, 114, 103, 101, 110, 8, 0, 21, 95, 208, 172, 75, 155, 182, 1, 109, 0,
                0, 0, 6, 110, 101, 115, 116, 101, 100, 116, 0, 0, 0, 1, 109, 0, 0, 0, 2, 111, 107,
                106, 100, 0, 5, 116, 117, 112, 108, 101, 104, 2, 97, 1, 100, 0, 4, 109, 111, 114,
                101
            ]
        );
    }

    #[test]
    fn port() {
        let out = to_bytes(RawTerm::Port {
            node: Box::new(RawTerm::SmallAtom("something@something".to_string())),
            id: 123,
            creation: 2,
        });

        assert_eq!(
            out,
            vec![
                REVISION, 102, 119, 19, 115, 111, 109, 101, 116, 104, 105, 110, 103, 64, 115, 111,
                109, 101, 116, 104, 105, 110, 103, 0, 0, 0, 123, 2
            ]
        )
    }

    #[test]
    fn reference() {
        let out = to_bytes(RawTerm::Ref {
            node: Box::new(RawTerm::AtomDeprecated("something@something".to_string())),
            id: vec![158726, 438566918, 237133],
            creation: 2,
        });

        assert_eq!(
            out,
            vec![
                REVISION, 114, 0, 3, 100, 0, 19, 115, 111, 109, 101, 116, 104, 105, 110, 103, 64,
                115, 111, 109, 101, 116, 104, 105, 110, 103, 2, 0, 2, 108, 6, 26, 36, 0, 6, 0, 3,
                158, 77
            ]
        )
    }

    #[test]
    fn pid() {
        let out = to_bytes(RawTerm::Pid {
            node: Box::new(RawTerm::SmallAtom("something@something".to_string())),
            id: 123,
            serial: 654,
            creation: 2,
        });

        assert_eq!(
            out,
            vec![
                REVISION, 103, 119, 19, 115, 111, 109, 101, 116, 104, 105, 110, 103, 64, 115, 111,
                109, 101, 116, 104, 105, 110, 103, 0, 0, 0, 123, 0, 0, 2, 142, 2
            ]
        )
    }

    #[test]
    fn function() {
        let input = RawTerm::Function {
            size: 134,
            arity: 0,
            uniq: [
                241, 72, 50, 109, 70, 84, 198, 45, 20, 94, 42, 25, 184, 243, 5, 100,
            ],
            index: 21,
            module: Box::new(RawTerm::AtomDeprecated("erl_eval".to_string())),
            old_index: Box::new(RawTerm::SmallInt(21)),
            old_uniq: Box::new(RawTerm::Int(126501267)),
            pid: Box::new(RawTerm::Pid {
                node: Box::new(RawTerm::AtomDeprecated("nonode@nohost".to_string())),
                id: 102,
                serial: 0,
                creation: 0,
            }),
            free_var: vec![RawTerm::SmallTuple(vec![
                RawTerm::Nil,
                RawTerm::AtomDeprecated("none".to_string()),
                RawTerm::AtomDeprecated("none".to_string()),
                RawTerm::List(vec![RawTerm::SmallTuple(vec![
                    RawTerm::AtomDeprecated("clause".to_string()),
                    RawTerm::SmallInt(3),
                    RawTerm::Nil,
                    RawTerm::Nil,
                    RawTerm::List(vec![RawTerm::SmallTuple(vec![
                        RawTerm::AtomDeprecated("atom".to_string()),
                        RawTerm::SmallInt(0),
                        RawTerm::AtomDeprecated("nil".to_string()),
                    ])]),
                ])]),
            ])],
        };
        let out = to_bytes(input);

        assert_eq!(
            out,
            vec![
                REVISION, 112, 0, 0, 0, 134, 0, 241, 72, 50, 109, 70, 84, 198, 45, 20, 94, 42, 25,
                184, 243, 5, 100, 0, 0, 0, 21, 0, 0, 0, 1, 100, 0, 8, 101, 114, 108, 95, 101, 118,
                97, 108, 97, 21, 98, 7, 138, 65, 147, 103, 100, 0, 13, 110, 111, 110, 111, 100,
                101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 102, 0, 0, 0, 0, 0, 104, 4, 106,
                100, 0, 4, 110, 111, 110, 101, 100, 0, 4, 110, 111, 110, 101, 108, 0, 0, 0, 1, 104,
                5, 100, 0, 6, 99, 108, 97, 117, 115, 101, 97, 3, 106, 106, 108, 0, 0, 0, 1, 104, 3,
                100, 0, 4, 97, 116, 111, 109, 97, 0, 100, 0, 3, 110, 105, 108, 106, 106
            ]
        )
    }
}
