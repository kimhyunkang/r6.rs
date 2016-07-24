use std::borrow::Cow;
use std::rc::Rc;

use num::{BigInt, FromPrimitive};
use num::rational::Ratio;

use datum::{Datum, SimpleDatum};
use error::{RuntimeError, RuntimeErrorKind};
use number::Number;
use real::Real;
use runtime::{DatumType, RDatum};

/// Types with implementing DatumCast trait can cast from/to Datum
pub trait DatumCast: Sized {
    /// Casts Datum into Self, possibly raising error
    fn unwrap(datum: RDatum) -> Result<Self, RuntimeError>;
    /// Casts Self into Datum
    fn wrap(self) -> RDatum;
}

impl DatumCast for Number {
    fn unwrap(datum: RDatum) -> Result<Number, RuntimeError> {
        match datum {
            Datum::Num(n) => Ok(n),
            _ => Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidType,
                desc: format!("expected Num, but received {:?}", DatumType::get_type(&datum))
            })
        }
    }

    fn wrap(self) -> RDatum{
        Datum::Num(self)
    }
}

impl DatumCast for usize {
    fn unwrap(datum: RDatum) -> Result<usize, RuntimeError> {
        let datumtype = DatumType::get_type(&datum);

        if let Datum::Num(n) = datum {
            if let Number::Real(r) = n.reduce() {
                if let Real::Fixnum(f) = r.reduce() {
                    if f >= 0 {
                        return Ok(f as usize);
                    }
                }
            }
        }

        Err(RuntimeError {
            kind: RuntimeErrorKind::InvalidType,
            desc: format!("expected unsigned integer, but received {:?}", datumtype)
        })
    }

    fn wrap(self) -> RDatum {
        let r: BigInt = FromPrimitive::from_usize(self).unwrap();
        let n: Real = Real::Rational(Ratio::from_integer(r)).reduce();
        Datum::Num(Number::Real(n))
    }
}

impl DatumCast for Real {
    fn unwrap(datum: RDatum) -> Result<Real, RuntimeError> {
        match datum {
            Datum::Num(Number::Real(n)) => Ok(n),
            _ => Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidType,
                desc: format!("expected Real, but received {:?}", DatumType::get_type(&datum))
            })
        }
    }

    fn wrap(self) -> RDatum {
        Datum::Num(Number::Real(self))
    }
}

impl DatumCast for bool {
    fn unwrap(datum: RDatum) -> Result<bool, RuntimeError> {
        match datum {
            Datum::Bool(b) => Ok(b),
            _ => Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidType,
                desc: format!("expected Bool, but received {:?}", DatumType::get_type(&datum))
            })
        }
    }

    fn wrap(self) -> RDatum{
        Datum::Bool(self)
    }
}

impl DatumCast for (RDatum, RDatum) {
    fn unwrap(datum: RDatum) -> Result<(RDatum, RDatum), RuntimeError> {
        match datum {
            Datum::Cons(c) => Ok(c.as_ref().clone()),
            _ => Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidType,
                desc: format!("expected Pair, but received {:?}", DatumType::get_type(&datum))
            })
        }
    }

    fn wrap(self) -> RDatum {
        Datum::Cons(Rc::new(self))
    }
}

impl DatumCast for Cow<'static, str> {
    fn unwrap(datum: RDatum) -> Result<Cow<'static, str>, RuntimeError> {
        match datum {
            Datum::Sym(c) => Ok(c.clone()),
            _ => Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidType,
                desc: format!("expected Symbol, but received {:?}", DatumType::get_type(&datum))
            })
        }
    }

    fn wrap(self) -> RDatum {
        Datum::Sym(self)
    }
}

impl DatumCast for String {
    fn unwrap(datum: RDatum) -> Result<String, RuntimeError> {
        match datum {
            Datum::String(s) => Ok(s.as_ref().clone()),
            _ => Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidType,
                desc: format!("expected String, but received {:?}", DatumType::get_type(&datum))
            })
        }
    }

    fn wrap(self) -> RDatum {
        Datum::String(Rc::new(self))
    }
}

impl <T: DatumCast> DatumCast for Vec<T> {
    fn unwrap(datum: RDatum) -> Result<Vec<T>, RuntimeError> {
        match datum {
            Datum::Vector(v) => v.iter().cloned().map(DatumCast::unwrap).collect(),
            _ => Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidType,
                desc: format!("expected Vector, but received {:?}", DatumType::get_type(&datum))
            })
        }
    }

    fn wrap(self) -> RDatum {
        Datum::Vector(Rc::new(self.into_iter().map(DatumCast::wrap).collect()))
    }
}

impl DatumCast for RDatum {
    fn unwrap(datum: RDatum) -> Result<RDatum, RuntimeError> {
        Ok(datum)
    }

    fn wrap(self) -> RDatum {
        self
    }
}

impl DatumCast for SimpleDatum {
    fn unwrap(datum: RDatum) -> Result<SimpleDatum, RuntimeError> {
        match SimpleDatum::from_datum(datum) {
            Some(v) => Ok(v),
            None => Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidType,
                desc: "Trying to cast non-simple datum to const type".to_string()
            })
        }
    }

    fn wrap(self) -> RDatum {
        match self {
            SimpleDatum::Sym(s) => Datum::Sym(s),
            SimpleDatum::Bool(b) => Datum::Bool(b),
            SimpleDatum::Char(c) => Datum::Char(c),
            SimpleDatum::String(s) => Datum::String(s),
            SimpleDatum::Bytes(v) => Datum::Bytes(v),
            SimpleDatum::Num(n) => Datum::Num(n),
            SimpleDatum::Nil => Datum::Nil
        }
    }
}
