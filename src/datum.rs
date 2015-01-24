use std::string::CowString;
use std::rc::Rc;
use std::cell::RefCell;
use std::iter::FromIterator;
use std::fmt;

/// Datum is the primary data type of Scheme
/// Datum is a generic type here to make parser somewhat independent from runtime
/// Ext can hold runtime data not representable in datum syntax, such as primitive function or I/O
/// ports
#[derive(PartialEq, Clone)]
pub enum Datum<T> {
    /// Symbol
    Sym(CowString<'static>),
    /// Boolean
    Bool(bool),
    /// Character
    Char(char),
    /// Numeric value
    Num(isize),
    /// `()`
    Nil,
    /// Pair
    Cons(Rc<RefCell<(Datum<T>, Datum<T>)>>),
    /// Extra values
    Ext(T)
}

fn write_cons<T: fmt::Debug>(tail: &Datum<T>, f: &mut fmt::Formatter) -> fmt::Result {
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

impl<T: fmt::Debug> fmt::Debug for Datum<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Datum::Sym(ref s) => write!(f, "{}", s),
            Datum::Bool(true) => write!(f, "#t"),
            Datum::Bool(false) => write!(f, "#f"),
            Datum::Char(c) => format_char(c, f),
            Datum::Num(n) => n.fmt(f),
            Datum::Ext(ref x) => x.fmt(f),
            Datum::Nil => write!(f, "()"),
            Datum::Cons(ref ptr) => {
                let pair = ptr.borrow();
                try!(write!(f, "({:?}", pair.0));
                write_cons(&pair.1, f)
            }
        }
    }
}

impl<T: Clone> Datum<T> {
    /// Iterate the values if it's a proper list
    pub fn iter(&self) -> DatumIter<T> {
        DatumIter { ptr: self.clone() }
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

impl<T> FromIterator<Datum<T>> for Datum<T> {
    fn from_iter<Iter: Iterator<Item=Datum<T>> >(iterator: Iter) -> Datum<T> {
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
    Datum::Cons(Rc::new(RefCell::new((head, tail))))
}

#[cfg(test)]
mod test {
    use super::{Datum, cons};
    use std::borrow::Cow;
    use std::rc::Rc;
    use std::cell::RefCell;

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
    fn test_iter() {
        let list: Datum<()> = Datum::Cons(Rc::new(RefCell::new((
            Datum::Num(1),
            Datum::Cons(Rc::new(RefCell::new((
                Datum::Num(2),
                Datum::Nil
            ))))
        ))));

        assert_eq!(Ok(vec![Datum::Num(1), Datum::Num(2)]), list.iter().collect());
    }
}
