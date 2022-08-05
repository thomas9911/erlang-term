use crate::Term;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde_impl", derive(Serialize, Deserialize))]
pub struct ImproperList {
    head: Vec<Term>,
    tail: Option<Term>,
}

impl ImproperList {
    pub fn new() -> ImproperList {
        ImproperList::from_list(Vec::new())
    }

    pub fn from_list(head: Vec<Term>) -> ImproperList {
        ImproperList { head, tail: None }
    }

    pub fn from_parts(head: Vec<Term>, tail: Term) -> ImproperList {
        ImproperList {
            head,
            tail: Some(tail),
        }
    }

    pub fn set_tail(&mut self, term: Term) {
        if term.is_list() {
            self.head.extend(term.as_list().unwrap());
        } else {
            self.tail = Some(term)
        }
    }

    pub fn put_tail(mut self, tail: Term) -> Self {
        self.set_tail(tail);
        self
    }

    pub fn get_tail(&self) -> Option<&Term> {
        self.tail.as_ref()
    }

    pub fn get_mut_tail(&mut self) -> Option<&mut Term> {
        self.tail.as_mut()
    }
}

impl std::ops::Deref for ImproperList {
    type Target = Vec<Term>;

    fn deref(&self) -> &Self::Target {
        &self.head
    }
}

impl std::ops::DerefMut for ImproperList {
    fn deref_mut(&mut self) -> &mut Vec<Term> {
        &mut self.head
    }
}

#[test]
fn extend_list() {
    let list = ImproperList::from_list(vec![Term::Int(123), Term::Int(124), Term::Int(125)]);

    let new_list = list.put_tail(Term::List(vec![
        Term::Int(256),
        Term::Int(257),
        Term::Int(258),
    ]));
    let expected = ImproperList::from_list(vec![
        Term::Int(123),
        Term::Int(124),
        Term::Int(125),
        Term::Int(256),
        Term::Int(257),
        Term::Int(258),
    ]);

    assert_eq!(new_list, expected);
}

#[test]
fn deref() {
    let list = ImproperList::from_list(vec![Term::Int(123), Term::Int(124), Term::Int(125)]);

    assert_eq!(Some(&Term::Int(124)), list.get(1))
}

#[test]
fn deref_mut() {
    let mut list = ImproperList::from_list(vec![Term::Int(123), Term::Int(124), Term::Int(125)]);

    list.push(Term::Int(555));
    assert_eq!(Some(&Term::Int(555)), list.last())
}

#[test]
fn get_tail() {
    let list = ImproperList::from_list(vec![Term::Int(123), Term::Int(124), Term::Int(125)]);
    assert_eq!(None, list.get_tail());

    let list = ImproperList::from_parts(
        vec![Term::Int(123), Term::Int(124), Term::Int(125)],
        Term::Int(999),
    );
    assert_eq!(Some(&Term::Int(999)), list.get_tail());
}

#[test]
fn get_mut_tail() {
    let mut expected = std::collections::HashMap::new();
    expected.insert(Term::from(String::from("test")), Term::Int(16));

    let mut list = ImproperList::from_parts(
        vec![Term::Int(123), Term::Int(124), Term::Int(125)],
        Term::Map(Default::default()),
    );

    if let Some(tail) = list.get_mut_tail() {
        match tail {
            Term::Map(map) => {
                map.insert(Term::from(String::from("test")), Term::Int(16));
            }
            _ => unreachable!(),
        }
    }

    assert_eq!(Some(&Term::Map(expected)), list.get_tail());
}
