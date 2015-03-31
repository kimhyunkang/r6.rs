use std::ops::Deref;
use std::rc::Rc;
use std::iter::{FromIterator, IntoIterator};
use std::fmt;
use std::borrow::Cow;

use num_trait;
use runtime::{Closure, NativeProc};

/// Datum is the primary data type of Scheme
/// Datum is a generic type here to make parser somewhat independent from runtime
/// Ext can hold runtime data not representable in datum syntax, such as primitive function or I/O
/// ports
#[derive(Clone)]
pub enum Datum {
    /// Boolean
    Bool(bool),
    /// Character
    Char(char),
    /// `()`
    Nil,
    /// `undefined`
    Undefined,
    /// Pointer values
    Ptr(Rc<Box<Object>>)
}

/// Type representation of Datum
#[derive(Debug, Copy, PartialEq)]
pub enum DatumType {
    Sym,
    Bool,
    Char,
    String,
    Vector,
    Bytes,
    Num,
    Pair,
    Null,
    Callable,
    Undefined
}

impl DatumType {
    /// Get the type of datum
    pub fn get_type(datum: &Datum) -> DatumType {
        match datum {
            &Datum::Bool(_) => DatumType::Bool,
            &Datum::Char(_) => DatumType::Char,
            &Datum::Nil => DatumType::Null,
            &Datum::Undefined => DatumType::Undefined,
            &Datum::Ptr(ref p) => p.get_type()
        }
    }
}


impl PartialEq for Datum {
    fn eq(&self, rhs: &Datum) -> bool {
        match (self, rhs) {
            (&Datum::Bool(l), &Datum::Bool(r)) => l == r,
            (&Datum::Char(l), &Datum::Char(r)) => l == r,
            (&Datum::Nil, &Datum::Nil) => true,
            (&Datum::Undefined, &Datum::Undefined) => true,
            (&Datum::Ptr(ref l), &Datum::Ptr(ref r)) => l.obj_eq(r.deref().deref()),
            _ => false
        }
    }
}

pub trait UpcastObject {
    fn upcast(self: Box<Self>) -> Box<Object>;
}

pub trait Object: fmt::Display {
    fn get_primfunc(&self) -> Option<&NativeProc> { None }
    fn get_closure(&self) -> Option<&Closure> { None }
    fn get_sym(&self) -> Option<&str> { None }
    fn get_string(&self) -> Option<&str> { None }
    fn get_vector(&self) -> Option<&[Datum]> { None }
    fn get_bytes(&self) -> Option<&[u8]> { None }
    fn get_pair(&self) -> Option<&(Datum, Datum)> { None }
    fn get_number(&self) -> Option<&num_trait::Number> { None }
    fn obj_eq(&self, &Object) -> bool;
    fn get_type(&self) -> DatumType;
}

/// Symbol
impl Object for Cow<'static, str> {
    fn get_sym(&self) -> Option<&str> {
        Some(self.deref())
    }

    fn obj_eq(&self, rhs: &Object) -> bool {
        if let Some(s) = rhs.get_sym() {
            self.deref() == s
        } else {
            false
        }
    }

    fn get_type(&self) -> DatumType {
        DatumType::Sym
    }
}

impl Object for String {
    fn get_string(&self) -> Option<&str> {
        Some(self.deref())
    }

    fn obj_eq(&self, rhs: &Object) -> bool {
        if let Some(s) = rhs.get_string() {
            self.deref() == s
        } else {
            false
        }
    }

    fn get_type(&self) -> DatumType {
        DatumType::String
    }
}

impl Object for Vec<Datum> {
    fn get_vector(&self) -> Option<&[Datum]> {
        Some(self.deref())
    }

    fn obj_eq(&self, rhs: &Object) -> bool {
        if let Some(s) = rhs.get_vector() {
            self.deref() == s
        } else {
            false
        }
    }

    fn get_type(&self) -> DatumType {
        DatumType::Vector
    }
}

impl fmt::Display for Vec<Datum> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_empty() {
            write!(f, "#()")
        } else {
            try!(write!(f, "#({:?}", self[0]));
            for x in self[1..].iter() {
                try!(write!(f, " {:?}", x));
            }
            write!(f, ")")
        }
    }
}

#[derive(Clone)]
pub struct Bytes {
    pub bytes: Vec<u8>
}

impl Bytes {
    pub fn new(data: Vec<u8>) -> Bytes {
        Bytes { bytes: data }
    }
}

impl Object for Bytes {
    fn get_bytes(&self) -> Option<&[u8]> {
        Some(self.bytes.deref())
    }

    fn obj_eq(&self, rhs: &Object) -> bool {
        if let Some(s) = rhs.get_bytes() {
            self.bytes.deref() == s
        } else {
            false
        }
    }

    fn get_type(&self) -> DatumType {
        DatumType::Bytes
    }
}

impl fmt::Display for Bytes {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.bytes.is_empty() {
            write!(f, "#vu8()")
        } else {
            try!(write!(f, "#vu8({}", self.bytes[0]));
            for x in self.bytes[1..].iter() {
                try!(write!(f, " {}", x));
            }
            write!(f, ")")
        }
    }
}

impl Object for (Datum, Datum) {
    fn get_pair(&self) -> Option<&(Datum, Datum)> {
        Some(self)
    }

    fn obj_eq(&self, rhs: &Object) -> bool {
        if let Some(s) = rhs.get_pair() {
            self == s
        } else {
            false
        }
    }

    fn get_type(&self) -> DatumType {
        DatumType::Pair
    }
}

impl fmt::Display for (Datum, Datum) {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Datum::Ptr(ref p) = self.0 {
            if let Some("quote") = p.get_sym() {
                if let Datum::Ptr(ref tail) = self.1 {
                    if let Some(tail_pair) = tail.get_pair() {
                        if let Datum::Nil = tail_pair.1 {
                            return write!(f, "'{:?}", tail_pair.0);
                        }
                    }
                }
            }
        }
        try!(write!(f, "({:?}", self.0));
        write_cons(&self.1, f)
    }
}

fn write_cons(tail: &Datum, f: &mut fmt::Formatter) -> fmt::Result {
    match tail {
        &Datum::Nil => {
            write!(f, ")")
        },
        &Datum::Ptr(ref ptr) => {
            if let Some(pair) = ptr.get_pair() {
                try!(write!(f, " {:?}", pair.0));
                write_cons(&pair.1, f)
            } else {
                write!(f, " . {})", *ptr)
            }
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
            Datum::Bool(true) => write!(f, "#t"),
            Datum::Bool(false) => write!(f, "#f"),
            Datum::Char(c) => format_char(c, f),
            Datum::Nil => write!(f, "()"),
            Datum::Undefined => write!(f, "#<undefined>"),
            Datum::Ptr(ref ptr) => 
                if let Some(s) = ptr.get_string() {
                    write!(f, "{:?}", s)
                } else {
                    write!(f, "{}", *ptr)
                }
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
            Datum::Ptr(ref ptr) =>
                if let Some(pair) = ptr.get_pair() {
                    pair.clone()
                } else {
                    return Some(Err(()));
                },
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
    Datum::Ptr(Rc::new(Box::new((head, tail))))
}

#[cfg(test)]
mod test {
    use super::{Datum, Object, Bytes, cons};
    use real::Real;
    use std::borrow::Cow;
    use std::rc::Rc;

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
        compare_fmt("#(a b)", Datum::Ptr(Rc::new(Box::new(vec![sym!("a"), sym!("b")]))));
        compare_fmt("#()", Datum::Ptr(Rc::new(Box::new(Vec::new()))));
    }

    #[test]
    fn test_bytes_fmt() {
        compare_fmt("#vu8(1 2 3)", Datum::Ptr(Rc::new(box Bytes::new(vec![1, 2, 3]))));
        compare_fmt("#vu8()", Datum::Ptr(Rc::new(box Bytes::new(Vec::new()))));
    }

    #[test]
    fn test_quote_abbrev() {
        compare_fmt("'a", list!(sym!("quote"), sym!("a")));
        compare_fmt("'(a b)", list!(sym!("quote"), list!(sym!("a"), sym!("b"))));
    }

    #[test]
    fn test_iter() {
        let n1:Box<Object> = box Real::Fixnum(1);
        let n2:Box<Object> = box Real::Fixnum(2);
        let list: Datum = list!(num!(1), num!(2));

        assert_eq!(Ok(vec![Datum::Ptr(Rc::new(n1)), Datum::Ptr(Rc::new(n2))]), list.iter().collect());
    }
}
