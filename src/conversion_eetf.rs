use crate::RawTerm;

fn convert_from(term: eetf::Term) -> RawTerm {
    match term {
        eetf::Term::Atom(bin) => RawTerm::Atom(bin.name),
        eetf::Term::Binary(bin) => RawTerm::Binary(bin.bytes),
        eetf::Term::FixInteger(int) => RawTerm::Int(int.value),
        eetf::Term::BigInteger(int) => RawTerm::LargeBigInt(int.value),
        eetf::Term::Map(items) => RawTerm::Map(
            items
                .map
                .into_iter()
                .map(|(k, v)| (convert_from(k), convert_from(v)))
                .collect(),
        ),
        eetf::Term::Tuple(item) => {
            RawTerm::LargeTuple(item.elements.into_iter().map(convert_from).collect())
        }
        eetf::Term::List(item) => {
            RawTerm::List(item.elements.into_iter().map(convert_from).collect())
        }
        eetf::Term::Float(float) => RawTerm::Float(float.value.into()),
        eetf::Term::BitBinary(mut bin) => {
            let bit = if bin.bytes.len() > 1 {
                bin.bytes.pop().expect("is there")
            } else {
                0
            };
            RawTerm::BitBinary {
                binary: bin.bytes,
                bit,
                bits: bin.tail_bits_size,
            }
        }
        eetf::Term::ByteList(bin) => RawTerm::String(bin.bytes),
        eetf::Term::ExternalFun(func) => panic!("not implemented"),
        eetf::Term::InternalFun(internal_func) => match *internal_func {
            eetf::InternalFun::New {
                module,
                arity,
                pid,
                free_vars,
                index,
                uniq,
                old_index,
                old_uniq,
            } => RawTerm::Function {
                size: free_vars.len() as u32,
                arity,
                uniq,
                index,
                module: Box::new(convert_from(eetf::Term::Atom(module))),
                old_index: Box::new(convert_from(eetf::Term::FixInteger(old_index.into()))),
                old_uniq: Box::new(convert_from(eetf::Term::FixInteger(old_uniq.into()))),
                pid: Box::new(convert_from(eetf::Term::Pid(pid))),
                free_var: free_vars.into_iter().map(|x| convert_from(x)).collect(),
            },
            eetf::InternalFun::Old { .. } => panic!("not supported"),
        },
        eetf::Term::ImproperList(improper_list) => {
            let mut list: Vec<_> = improper_list
                .elements
                .into_iter()
                .map(convert_from)
                .collect();
            list.push(convert_from(*improper_list.last));
            RawTerm::List(list)
        }
        eetf::Term::Pid(pid) => RawTerm::NewPid {
            node: Box::new(convert_from(eetf::Term::Atom(pid.node))),
            id: pid.id,
            serial: pid.serial,
            creation: pid.creation,
        },
        // the ID field is only four bytes. Only 28 bits are significant; the rest are to be 0.
        eetf::Term::Port(port) => RawTerm::NewPort {
            node: Box::new(convert_from(eetf::Term::Atom(port.node))),
            id: port.id as u32,
            creation: port.creation,
        },
        eetf::Term::Reference(reference) => RawTerm::NewerRef {
            node: Box::new(convert_from(eetf::Term::Atom(reference.node))),
            id: reference.id,
            creation: reference.creation,
        },
    }
}
