use std::string::CowString;
use std::rc::Rc;
use std::cell::RefCell;
use std::iter::FromIterator;
use std::ops::Deref;
use std::fmt;

#[derive(PartialEq, Clone)]
pub enum Datum<T> {
    Sym(CowString<'static>),
    Bool(bool),
    Char(char),
    Num(isize),
    Nil,
    Cons(Rc<RefCell<Datum<T>>>, Rc<RefCell<Datum<T>>>),
    Ext(T)
}

fn write_cons<T: fmt::Show>(tail: &Datum<T>, f: &mut fmt::Formatter) -> fmt::Result {
    match *tail {
        Datum::Nil => {
            write!(f, ")")
        },
        Datum::Cons(ref ht, ref tt) => {
            try!(write!(f, " {:?}", ht.borrow()));
            write_cons(tt.borrow().deref(), f)
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

impl<T: fmt::Show> fmt::Show for Datum<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Datum::Sym(ref s) => write!(f, "{}", s),
            Datum::Bool(true) => write!(f, "#t"),
            Datum::Bool(false) => write!(f, "#f"),
            Datum::Char(c) => format_char(c, f),
            Datum::Num(n) => n.fmt(f),
            Datum::Ext(ref x) => x.fmt(f),
            Datum::Nil => write!(f, "()"),
            Datum::Cons(ref h, ref t) => {
                try!(write!(f, "({:?}", h.borrow()));
                write_cons(t.borrow().deref(), f)
            }
        }
    }
}

impl<T: Clone> Datum<T> {
    pub fn iter(&self) -> DatumIter<T> {
        DatumIter { ptr: self.clone() }
    }
}

pub struct DatumIter<T> {
    ptr: Datum<T>
}

impl<T: Clone> Iterator for DatumIter<T> {
    type Item = Result<Datum<T>, ()>;

    fn next(&mut self) -> Option<Result<Datum<T>, ()>> {
        let (val, next) = match self.ptr {
            Datum::Nil => return None,
            Datum::Cons(ref h, ref t) => (h.borrow().deref().clone(), t.borrow().deref().clone()),
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

pub fn cons<T>(head: Datum<T>, tail: Datum<T>) -> Datum<T> {
    Datum::Cons(Rc::new(RefCell::new(head)), Rc::new(RefCell::new(tail)))
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
        let list: Datum<()> = Datum::Cons(
            Rc::new(RefCell::new(Datum::Num(1))),
            Rc::new(RefCell::new(Datum::Cons(
                Rc::new(RefCell::new(Datum::Num(2))),
                Rc::new(RefCell::new(Datum::Nil))
            )
        )));

        assert_eq!(Ok(vec![Datum::Num(1), Datum::Num(2)]), list.iter().collect());
    }
}
