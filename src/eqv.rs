use std::rc::Rc;

use datum::Datum;

pub trait DatumEqv {
    fn eqv(&self, other: &Self) -> bool;
}

impl<T: DatumEqv> DatumEqv for Datum<T> {
    fn eqv(&self, other: &Datum<T>) -> bool {
        match self {
            &Datum::Bool(self_v) => if let &Datum::Bool(other_v) = other {
                    self_v == other_v
                } else {
                    false
                },
            &Datum::Char(self_v) => if let &Datum::Char(other_v) = other {
                    self_v == other_v
                } else {
                    false
                },
            &Datum::Num(ref self_v) => if let &Datum::Num(ref other_v) = other {
                    self_v.eqv(other_v)
                } else {
                    false
                },
            &Datum::Nil => if let &Datum::Nil = other {
                    true
                } else {
                    false
                },
            &Datum::Sym(ref self_v) => if let &Datum::Sym(ref other_v) = other {
                    self_v == other_v
                } else {
                    false
                },
            &Datum::Bytes(ref self_v) => if let &Datum::Bytes(ref other_v) = other {
                    self_v.eqv(other_v)
                } else {
                    false
                },
            &Datum::Cons(ref self_v) => if let &Datum::Cons(ref other_v) = other {
                    self_v.eqv(other_v)
                } else {
                    false
                },
            &Datum::String(ref self_v) => if let &Datum::String(ref other_v) = other {
                    self_v.eqv(other_v)
                } else {
                    false
                },
            &Datum::Vector(ref self_v) => if let &Datum::Vector(ref other_v) = other {
                    self_v.eqv(other_v)
                } else {
                    false
                },
            &Datum::Ext(ref self_v) => if let &Datum::Ext(ref other_v) = other {
                    self_v.eqv(other_v)
                } else {
                    false
                }
        }
    }
}

impl <T> DatumEqv for Rc<T> {
    fn eqv(&self, other: &Rc<T>) -> bool {
        (self.as_ref() as *const T) == (other.as_ref() as *const T)
    }
}

impl <T: DatumEqv> DatumEqv for Option<T> {
    fn eqv(&self, other: &Option<T>) -> bool {
        match self {
            &Some(ref x) => if let &Some(ref y) = other {
                x.eqv(y)
            } else {
                false
            },
            &None => other.is_none()
        }
    }
}
