use std::borrow::Cow;
use std::fmt;
use std::iter::{FromIterator, IntoIterator};
use std::rc::Rc;

use number::Number;
use parser::SPECIAL_TOKEN_MAP;

/// Datum is the primary data type of Scheme
/// Datum is a generic type here to make parser somewhat independent from runtime
/// Ext can hold runtime data not representable in datum syntax, such as primitive function or I/O
/// ports
#[derive(PartialEq, Clone)]
pub enum Datum<T> {
    /// Symbol
    Sym(Cow<'static, str>),
    /// Boolean
    Bool(bool),
    /// Character
    Char(char),
    /// String
    String(Rc<String>),
    /// Vector
    Vector(Rc<Vec<Datum<T>>>),
    /// Byte vector
    Bytes(Rc<Vec<u8>>),
    /// Numeric value
    Num(Number),
    /// `()`
    Nil,
    /// Pair
    Cons(Rc<(Datum<T>, Datum<T>)>),
    /// Extra values
    Ext(T)
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

trait DatumFormatter<T> {
    fn ext_fmt(&self, &T, &mut fmt::Formatter) -> fmt::Result;

    fn datum_fmt(&self, datum: &Datum<T>, f: &mut fmt::Formatter) -> fmt::Result {
        match *datum {
            Datum::Sym(ref s) => write!(f, "{}", s),
            Datum::Bool(true) => write!(f, "#t"),
            Datum::Bool(false) => write!(f, "#f"),
            Datum::Char(c) => format_char(c, f),
            Datum::String(ref s) => write!(f, "{:?}", s),
            Datum::Vector(ref vec) => {
                if vec.is_empty() {
                    write!(f, "#()")
                } else {
                    try!(write!(f, "#("));
                    try!(self.datum_fmt(&vec[0], f));
                    for x in vec[1..].iter() {
                        try!(f.write_str(" "));
                        try!(self.datum_fmt(x, f));
                    }
                    write!(f, ")")
                }
            },
            Datum::Bytes(ref vec) => {
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
            Datum::Ext(ref x) => self.ext_fmt(x, f),
            Datum::Nil => write!(f, "()"),
            Datum::Cons(ref pair) => {
                if let Datum::Sym(ref s) = pair.0 {
                    match SPECIAL_TOKEN_MAP.get(s.as_ref()) {
                        Some(ch) => if let Datum::Cons(ref tail) = pair.1 {
                            if let Datum::Nil = tail.1 {
                                try!(write!(f, "{}", ch));
                                return self.datum_fmt(&tail.0, f);
                            }
                        },
                        _ => ()
                    }
                }
                try!(write!(f, "("));
                try!(self.datum_fmt(&pair.0, f));
                self.write_cons(&pair.1, f)
            }
        }
    }

    fn write_cons(&self, tail: &Datum<T>, f: &mut fmt::Formatter) -> fmt::Result {
        match tail {
            &Datum::Nil => {
                write!(f, ")")
            },
            &Datum::Cons(ref pair) => {
                try!(write!(f, " "));
                try!(self.datum_fmt(&pair.0, f));
                self.write_cons(&pair.1, f)
            },
            _ => {
                try!(write!(f, " . "));
                try!(self.datum_fmt(&tail, f));
                write!(f, ")")
            }
        }
    }
}

struct DebugFormatter;

impl<T: fmt::Debug> DatumFormatter<T> for DebugFormatter {
    fn ext_fmt(&self, ext: &T, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", ext)
    }
}

impl<T: fmt::Debug> fmt::Debug for Datum<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let debug_fmt = DebugFormatter;

        debug_fmt.datum_fmt(self, f)
    }
}

struct DisplayFormatter;

impl<T: fmt::Display> DatumFormatter<T> for DisplayFormatter {
    fn ext_fmt(&self, ext: &T, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", ext)
    }
}

impl<T: fmt::Display> fmt::Display for Datum<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let debug_fmt = DisplayFormatter;

        debug_fmt.datum_fmt(self, f)
    }
}

impl<T: Clone> Datum<T> {
    /// Iterate the values if it's a proper list
    pub fn iter(&self) -> DatumIter<T> {
        DatumIter { ptr: self.clone() }
    }
}

pub trait TryConv<T, E> {
    fn try_conv(&self) -> Result<T, E>;
}

impl<E> TryConv<(), E> for () {
    fn try_conv(&self) -> Result<(), E> {
        Ok(())
    }
}

impl<S, T, E> TryConv<Datum<T>, E> for Datum<S> where S: TryConv<T, E> {
    fn try_conv(&self) -> Result<Datum<T>, E> {
        match self {
            &Datum::Sym(ref v) => Ok(Datum::Sym(v.clone())),
            &Datum::Bool(ref v) => Ok(Datum::Bool(*v)),
            &Datum::Char(ref v) => Ok(Datum::Char(*v)),
            &Datum::String(ref v) => Ok(Datum::String(v.clone())),
            &Datum::Vector(ref v) => {
                let res: Result<Vec<Datum<T>>, E> = v.iter().map(|x| x.try_conv()).collect();
                res.map(|v| Datum::Vector(Rc::new(v)))
            },
            &Datum::Bytes(ref v) => Ok(Datum::Bytes(v.clone())),
            &Datum::Num(ref v) => Ok(Datum::Num(v.clone())),
            &Datum::Nil => Ok(Datum::Nil),
            &Datum::Cons(ref v) => {
                let h = try!(v.0.try_conv());
                let t = try!(v.1.try_conv());
                Ok(Datum::Cons(Rc::new((h, t))))
            },
            &Datum::Ext(ref v) => v.try_conv().map(Datum::Ext)
        }
    }
}

/// If the datum is a proper list, iterate the values in the list.
/// If it's not a list, returns Err(()) when the iterator meets non-null cdr
pub struct DatumIter<T> {
    ptr: Datum<T>
}

impl<T: Clone> Iterator for DatumIter<T> {
    type Item = Result<Datum<T>, ()>;

    fn next(&mut self) -> Option<Result<Datum<T>, ()>> {
        let (val, next) = match self.ptr {
            Datum::Nil => return None,
            Datum::Cons(ref pair) => {
                (pair.0.clone(), pair.1.clone())
            }
            _ => return Some(Err(()))
        };

        self.ptr = next;

        Some(Ok(val))
    }
}

impl<T> FromIterator<Datum<T>> for Datum<T> {
    fn from_iter<Iter: IntoIterator<Item=Datum<T>> >(iterator: Iter) -> Datum<T> {
        let list:Vec<Datum<T>> = FromIterator::from_iter(iterator);
        let mut res = Datum::Nil;
        for d in list.into_iter().rev() {
            res = cons(d, res);
        }
        return res;
    }
}

/// `cons` the values into a pair
pub fn cons<T>(head: Datum<T>, tail: Datum<T>) -> Datum<T> {
    Datum::Cons(Rc::new((head, tail)))
}

pub fn concat<T: Clone>(x: Datum<T>, y: Datum<T>) -> Result<Datum<T>, ()> {
    match x {
        Datum::Nil => Ok(y),
        Datum::Cons(pair) => {
            concat(pair.1.clone(), y).map(|new_y| cons(pair.0.clone(), new_y))
        },
        _ => Err(())
    }
}

#[cfg(test)]
mod test {
    use super::{Datum, cons};
    use number::Number;
    use std::borrow::Cow;
    use std::rc::Rc;

    use num::FromPrimitive;

    fn compare_fmt(s: &str, datum: Datum<()>) {
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
        compare_fmt("#(a b)", Datum::Vector(Rc::new(vec![sym!("a"), sym!("b")])));
        compare_fmt("#()", Datum::Vector(Rc::new(Vec::new())));
    }

    #[test]
    fn test_bytes_fmt() {
        compare_fmt("#vu8(1 2 3)", Datum::Bytes(Rc::new(vec![1, 2, 3])));
        compare_fmt("#vu8()", Datum::Bytes(Rc::new(Vec::new())));
    }

    #[test]
    fn test_quote_abbrev() {
        compare_fmt("'a", list!(sym!("quote"), sym!("a")));
        compare_fmt("'(a b)", list!(sym!("quote"), list!(sym!("a"), sym!("b"))));
        compare_fmt("`a", list!(sym!("quasiquote"), sym!("a")));
        compare_fmt(",a", list!(sym!("unquote"), sym!("a")));
        compare_fmt(",@a", list!(sym!("unquote-splicing"), sym!("a")));

        compare_fmt("#'a", list!(sym!("syntax"), sym!("a")));
        compare_fmt("#'(a b)", list!(sym!("syntax"), list!(sym!("a"), sym!("b"))));
        compare_fmt("#`a", list!(sym!("quasisyntax"), sym!("a")));
        compare_fmt("#,a", list!(sym!("unsyntax"), sym!("a")));
        compare_fmt("#,@a", list!(sym!("unsyntax-splicing"), sym!("a")));
    }

    #[test]
    fn test_iter() {
        let n1 = FromPrimitive::from_isize(1).unwrap();
        let n2 = FromPrimitive::from_isize(2).unwrap();
        let list: Datum<()> = list!(num!(1), num!(2));

        assert_eq!(Ok(vec![Datum::Num(n1), Datum::Num(n2)]), list.iter().collect());
    }
}
