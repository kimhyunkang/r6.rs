use numeric::LNumeric;
use std::fmt;
use std::rc::Rc;

#[deriving(PartialEq)]
pub enum LDatum<T> {
    LIdent(String),
    LString(String),
    LChar(char),
    LBool(bool),
    LNum(LNumeric),
    LCons(Rc<LDatum<T>>, Rc<LDatum<T>>),
    LNil,
    LVector(Rc<Vec<Rc<LDatum<T>>>>),
    LExt(T),
}

#[deriving(PartialEq)]
pub enum Quotation
{
    Quote,
    QuasiQuote,
    Unquote,
    UnquoteSplicing,
}

impl<T: fmt::Show> fmt::Show for LDatum<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &LIdent(ref s) => write!(f, "{}", s),
            &LString(ref s) => write!(f, "\"{}\"", s.escape_default()),
            &LChar(' ') => write!(f, "#\\space"),
            &LChar('\n') => write!(f, "#\\newline"),
            &LChar(c) => write!(f, "#\\{:c}", c),
            &LBool(true) => write!(f, "#t"),
            &LBool(false) => write!(f, "#f"),
            &LNum(ref x) => write!(f, "{}", x),
            &LCons(ref head, ref tail) => {
                match is_quote(head.deref(), tail.deref()) {
                    Some((name, v)) => {
                        let prefix = match name {
                            Quote => "\'",
                            Unquote => ",",
                            UnquoteSplicing => ",@",
                            QuasiQuote => "`",
                        };
                        write!(f, "{}{}", prefix, v)
                    },
                    None => {
                        try!(write!(f, "({}", *head));
                        write_list(f, tail.deref())
                    },
                }
            },
            &LNil => write!(f, "()"),
            &LVector(ref v) => match v.as_slice() {
                [] => write!(f, "#()"),
                slice => {
                    try!(write!(f, "{}", slice.head()));
                    for x in slice.tail().iter() {
                        try!(write!(f, " {}", x))
                    }
                    Ok(())
                }
            },
            &LExt(ref v) => {
                write!(f, "{}", v)
            },
        }
    }
}

impl<T> LDatum<T> {
    pub fn to_list(&self) -> Option<Vec<Rc<LDatum<T>>>> {
        match self {
            &LCons(ref h, ref t) => datum_to_list(h.clone(), t.clone()),
            &LNil => Some(vec![]),
            _ => None,
        }
    }

    pub fn from_list(list: &[Rc<LDatum<T>>]) -> Rc<LDatum<T>> {
        match list {
            [] => Rc::new(LNil),
            _ => Rc::new(LCons(list.head().unwrap().clone(), LDatum::from_list(list.tail()))),
        }
    }

    pub fn append(&self, other: Rc<LDatum<T>>) -> Option<Rc<LDatum<T>>> {
        match self {
            &LNil => Some(other),
            &LCons(ref h, ref t) => t.append(other).map(|ts| { Rc::new(LCons(h.clone(), ts)) }),
            _ => None,
        }
    }
}

fn datum_to_list<T>(head: Rc<LDatum<T>>, tail: Rc<LDatum<T>>) -> Option<Vec<Rc<LDatum<T>>>> {
    let mut x = &tail;
    let mut list = vec![head.clone()];

    loop {
        match x.deref() {
            &LCons(ref h, ref t) => {
                list.push(h.clone());
                x = t;
            },
            &LNil => break,
            _ => return None,
        }
    }

    Some(list)
}

pub fn is_quote<T>(head: &LDatum<T>, tail: &LDatum<T>) -> Option<(Quotation, Rc<LDatum<T>>)> {
    match head {
        &LIdent(ref name) =>
            match tail {
                &LCons(ref v, ref t) => match *t.deref() {
                    LNil => 
                        if name.as_slice() == "quote" {
                            Some((Quote, v.clone()))
                        } else if name.as_slice() == "quasiquote" {
                            Some((QuasiQuote, v.clone()))
                        } else if name.as_slice() == "unquote" {
                            Some((Unquote, v.clone()))
                        } else if name.as_slice() == "unquote-splicing" {
                            Some((UnquoteSplicing, v.clone()))
                        } else {
                            None
                        },
                    _ => None
                },
                _ => None
            },
        _ =>
            None,
    }
}

fn write_list<T: fmt::Show>(f: &mut fmt::Formatter, v: &LDatum<T>) -> fmt::Result {
    match v {
        &LCons(ref head, ref tail) => {
            try!(write!(f, " {}", head));
            write_list(f, tail.deref())
        },
        &LNil => write!(f, ")"),
        _ => write!(f, " . {})", v)
    }
}
