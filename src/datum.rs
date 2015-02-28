use std::ops::Deref;
use std::rc::Rc;
use std::cell::RefCell;
use std::iter::{FromIterator, IntoIterator};
use std::fmt;
use std::borrow::Cow;

use number::Number;
use runtime::{Closure, NativeProc};

/// Datum is the primary data type of Scheme
/// Datum is a generic type here to make parser somewhat independent from runtime
/// Ext can hold runtime data not representable in datum syntax, such as primitive function or I/O
/// ports
#[derive(Clone)]
pub enum Datum {
    /// Symbol
    Sym(Cow<'static, str>),
    /// Boolean
    Bool(bool),
    /// Character
    Char(char),
    /// String
    String(String),
    /// Vector
    Vector(Rc<RefCell<Vec<Datum>>>),
    /// Byte vector
    Bytes(Rc<RefCell<Vec<u8>>>),
    /// Numeric value
    Num(Number),
    /// `()`
    Nil,
    /// `undefined`
    Undefined,
    /// Pair
    Cons(Rc<RefCell<(Datum, Datum)>>),
    /// Pointer values
    Ptr(Rc<Box<Object>>)
}

impl PartialEq for Datum {
    fn eq(&self, rhs: &Datum) -> bool {
        match (self, rhs) {
            (&Datum::Sym(ref l), &Datum::Sym(ref r)) => l == r,
            (&Datum::Bool(l), &Datum::Bool(r)) => l == r,
            (&Datum::Char(l), &Datum::Char(r)) => l == r,
            (&Datum::String(ref l), &Datum::String(ref r)) => l == r,
            (&Datum::Vector(ref l), &Datum::Vector(ref r)) => l == r,
            (&Datum::Bytes(ref l), &Datum::Bytes(ref r)) => l == r,
            (&Datum::Num(ref l), &Datum::Num(ref r)) => l == r,
            (&Datum::Nil, &Datum::Nil) => true,
            (&Datum::Undefined, &Datum::Undefined) => true,
            (&Datum::Cons(ref l), &Datum::Cons(ref r)) => l == r,
            (&Datum::Ptr(ref l), &Datum::Ptr(ref r)) => l.obj_eq(r.deref().deref()),
            _ => false
        }
    }
}

pub trait Object: fmt::Debug {
    fn get_primfunc(&self) -> Option<&NativeProc>;
    fn get_closure(&self) -> Option<&Closure>;
    fn obj_eq(&self, &Object) -> bool;
}

fn write_cons(tail: &Datum, f: &mut fmt::Formatter) -> fmt::Result {
    match tail {
        &Datum::Nil => {
            write!(f, ")")
        },
        &Datum::Cons(ref ptr) => {
            let pair = ptr.borrow();
            try!(write!(f, " {:?}", pair.0));
            write_cons(&pair.1, f)
        },
        _ => {
            write!(f, " . {:?})", tail)
        }
    }
}

fn format_char(c: char, f: &mut fmt::Formatter) -> fmt::Result {
    try!(write!(f, "#\\"));
    match c {
        '\0' => write!(f, "nul"),
        '\x08' => write!(f, "backspace"),
        '\t' => write!(f, "tab"),
        '\x0c' => write!(f, "page"),
        '\r' => write!(f, "return"),
        ' ' => write!(f, "space"),
        '!'...'~' => write!(f, "{}", c),
        _ => write!(f, "x{:x}", c as usize)
    }
}

impl fmt::Debug for Datum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Datum::Sym(ref s) => write!(f, "{}", s),
            Datum::Bool(true) => write!(f, "#t"),
            Datum::Bool(false) => write!(f, "#f"),
            Datum::Char(c) => format_char(c, f),
            Datum::String(ref s) => write!(f, "{:?}", s),
            Datum::Vector(ref ptr) => {
                let vec = ptr.borrow();
                if vec.is_empty() {
                    write!(f, "#()")
                } else {
                    try!(write!(f, "#({:?}", vec[0]));
                    for x in vec[1..].iter() {
                        try!(write!(f, " {:?}", x));
                    }
                    write!(f, ")")
                }
            },
            Datum::Bytes(ref ptr) => {
                let vec = ptr.borrow();
                if vec.is_empty() {
                    write!(f, "#vu8()")
                } else {
                    try!(write!(f, "#vu8({}", vec[0]));
                    for x in vec[1..].iter() {
                        try!(write!(f, " {}", x));
                    }
                    write!(f, ")")
                }
            },
            Datum::Num(ref n) => write!(f, "{}", n),
            Datum::Nil => write!(f, "()"),
            Datum::Undefined => write!(f, "#<undefined>"),
            Datum::Cons(ref ptr) => {
                let pair = ptr.borrow();
                if let Datum::Sym(ref s) = pair.0 {
                    if *s == Cow::Borrowed("quote") {
                        if let Datum::Cons(ref tail) = pair.1 {
                            let tail_ptr = tail.borrow();
                            if let Datum::Nil = tail_ptr.1 {
                                return write!(f, "'{:?}", tail_ptr.0);
                            }
                        }
                    }
                }
                try!(write!(f, "({:?}", pair.0));
                write_cons(&pair.1, f)
            },
            Datum::Ptr(ref ptr) => 
                write!(f, "{:?}", *ptr)
        }
    }
}

impl Datum {
    /// Iterate the values if it's a proper list
    pub fn iter(&self) -> DatumIter {
        DatumIter { ptr: self.clone() }
    }
}

/// If the datum is a proper list, iterate the values in the list.
/// If it's not a list, returns Err(()) when the iterator meets non-null cdr
pub struct DatumIter {
    ptr: Datum
}

impl Iterator for DatumIter {
    type Item = Result<Datum, ()>;

    fn next(&mut self) -> Option<Result<Datum, ()>> {
        let (val, next) = match self.ptr {
            Datum::Nil => return None,
            Datum::Cons(ref ptr) => {
                let pair = ptr.borrow();
                (pair.0.clone(), pair.1.clone())
            }
            _ => return Some(Err(()))
        };

        self.ptr = next;

        Some(Ok(val))
    }
}

impl FromIterator<Datum> for Datum {
    fn from_iter<Iter: IntoIterator<Item=Datum> >(iterator: Iter) -> Datum {
        let list:Vec<Datum> = FromIterator::from_iter(iterator);
        let mut res = Datum::Nil;
        for d in list.into_iter().rev() {
            res = cons(d, res);
        }
        return res;
    }
}

/// `cons` the values into a pair
pub fn cons(head: Datum, tail: Datum) -> Datum {
    Datum::Cons(Rc::new(RefCell::new((head, tail))))
}

#[cfg(test)]
mod test {
    use super::{Datum, cons};
    use number::Number;
    use std::borrow::Cow;
    use std::num::FromPrimitive;
    use std::rc::Rc;
    use std::cell::RefCell;

    fn compare_fmt(s: &str, datum: Datum) {
        assert_eq!(s.to_string(), format!("{:?}", datum))
    }

    #[test]
    fn test_fmt() {
        compare_fmt("a", sym!("a"));
        compare_fmt("()", list!());
        compare_fmt("(a)", list!(sym!("a")));
        compare_fmt("(a b)", list!(sym!("a"), sym!("b")));
        compare_fmt("(a . b)", cons(sym!("a"), sym!("b")));
    }

    #[test]
    fn test_vec_fmt() {
        compare_fmt("#(a b)", Datum::Vector(Rc::new(RefCell::new(vec![sym!("a"), sym!("b")]))));
        compare_fmt("#()", Datum::Vector(Rc::new(RefCell::new(Vec::new()))));
    }

    #[test]
    fn test_bytes_fmt() {
        compare_fmt("#vu8(1 2 3)", Datum::Bytes(Rc::new(RefCell::new(vec![1, 2, 3]))));
        compare_fmt("#vu8()", Datum::Bytes(Rc::new(RefCell::new(Vec::new()))));
    }

    #[test]
    fn test_quote_abbrev() {
        compare_fmt("'a", list!(sym!("quote"), sym!("a")));
        compare_fmt("'(a b)", list!(sym!("quote"), list!(sym!("a"), sym!("b"))));
    }

    #[test]
    fn test_iter() {
        let n1 = FromPrimitive::from_int(1).unwrap();
        let n2 = FromPrimitive::from_int(2).unwrap();
        let list: Datum = list!(num!(1), num!(2));

        assert_eq!(Ok(vec![Datum::Num(n1), Datum::Num(n2)]), list.iter().collect());
    }
}
