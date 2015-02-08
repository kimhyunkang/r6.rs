use std::iter::FromIterator;
use std::num::Float;

use num::{Zero, One};

use number::Number;
use real::Real;
use datum::Datum;
use error::{RuntimeError, RuntimeErrorKind};
use runtime::{DatumCast, RDatum, DatumType};

pub trait PrimFunc {
    fn call(&self, &[RDatum]) -> Result<RDatum, RuntimeError>;
}

pub struct Fold<P> {
    fold: fn(&[P]) -> P
}

pub struct Fold1<P> {
    fold1: fn(&P, &[P]) -> P
}

pub struct Fold1Err<P> {
    fold1: fn(&P, &[P]) -> Result<P, RuntimeError>
}

pub struct F1<T0, R> {
    f1: fn(T0) -> R
}

pub struct R1<R> {
    f1: fn(&RDatum) -> R
}

impl<T> PrimFunc for Fold<T> where T: DatumCast {
    fn call(&self, args: &[RDatum]) -> Result<RDatum, RuntimeError> {
        let p_args:Result<Vec<T>, RuntimeError> = args.iter().map(DatumCast::unwrap).collect();
        let f = self.fold;
        p_args.map(|v| f(v.as_slice()).wrap())
    }
}

impl PrimFunc for Fold<RDatum> {
    fn call(&self, args: &[RDatum]) -> Result<RDatum, RuntimeError> {
        let f = self.fold;
        Ok(f(args))
    }
}

impl<T> PrimFunc for Fold1<T> where T: DatumCast {
    fn call(&self, args: &[RDatum]) -> Result<RDatum, RuntimeError> {
        let p_args:Result<Vec<T>, RuntimeError> = args.iter().map(DatumCast::unwrap).collect();
        let f = self.fold1;
        p_args.and_then(|v|
            if v.len() < 1 {
                Err(RuntimeError {
                    kind: RuntimeErrorKind::NumArgs,
                    desc: "Expected at least 1 arguments, received 0".to_string()
                })
            } else {
                Ok(f(&v[0], &v[1..]).wrap())
            }
        )
    }
}

impl<T> PrimFunc for Fold1Err<T> where T: DatumCast {
    fn call(&self, args: &[RDatum]) -> Result<RDatum, RuntimeError> {
        let p_args:Result<Vec<T>, RuntimeError> = args.iter().map(DatumCast::unwrap).collect();
        let f = self.fold1;
        p_args.and_then(|v|
            if v.len() < 1 {
                Err(RuntimeError {
                    kind: RuntimeErrorKind::NumArgs,
                    desc: "Expected at least 1 arguments, received 0".to_string()
                })
            } else {
                f(&v[0], &v[1..]).map(|res| res.wrap())
            }
        )
    }
}

impl<R: DatumCast> PrimFunc for R1<R> {
    fn call(&self, args: &[RDatum]) -> Result<RDatum, RuntimeError> {
        if args.len() != 1 {
            return Err(RuntimeError {
                kind: RuntimeErrorKind::NumArgs,
                desc: format!("Expected 1 argument, received {:?}", args.len())
            });
        }
        let f = self.f1;

        Ok(f(&args[0]).wrap())
    }
}

impl<T0: DatumCast> PrimFunc for F1<T0, RDatum> {
    fn call(&self, args: &[RDatum]) -> Result<RDatum, RuntimeError> {
        if args.len() != 1 {
            return Err(RuntimeError {
                kind: RuntimeErrorKind::NumArgs,
                desc: format!("Expected 1 argument, received {:?}", args.len())
            });
        }
        DatumCast::unwrap(&args[0]).map(self.f1)
    }
}

impl<T0: DatumCast, R: DatumCast> PrimFunc for F1<T0, R> {
    fn call(&self, args: &[RDatum]) -> Result<RDatum, RuntimeError> {
        if args.len() != 1 {
            return Err(RuntimeError {
                kind: RuntimeErrorKind::NumArgs,
                desc: format!("Expected 1 argument, received {:?}", args.len())
            });
        }
        let f = self.f1;
        DatumCast::unwrap(&args[0]).map(|v| f(v).wrap())
    }
}

fn add(args: &[Number]) -> Number {
    let mut sum:Number = Zero::zero();
    for a in args.iter() {
        sum = &sum + a;
    }
    return sum;
}

/// `(+ n0 n1 ...)`
pub static PRIM_ADD:Fold<Number> = Fold { fold: add };

fn mul(args: &[Number]) -> Number {
    let mut product:Number = One::one();
    for a in args.iter() {
        product = &product * a;
    }
    return product;
}

/// `(* n0 n1 ...)`
pub static PRIM_MUL:Fold<Number> = Fold { fold: mul };

fn sub(arg0: &Number, args: &[Number]) -> Number {
    if args.len() == 0 {
        return -arg0;
    }
    let mut sum:Number = arg0.clone();
    for a in args.iter() {
        sum = &sum - a;
    }
    return sum;
}

/// `(- n0 n1 ...)`
pub static PRIM_SUB:Fold1<Number> = Fold1 { fold1: sub };

fn div(arg0: &Number, args: &[Number]) -> Result<Number, RuntimeError> {
    if args.len() == 0 {
        if arg0.is_exact() && arg0.is_zero() {
            return Err(RuntimeError {
                kind: RuntimeErrorKind::DivideByZero,
                desc: "Tried to divied by 0".to_string()
            });
        }
        return Ok(-arg0);
    }

    let mut product:Number = arg0.clone();
    for a in args.iter() {
        if a.is_exact() && a.is_zero() {
            return Err(RuntimeError {
                kind: RuntimeErrorKind::DivideByZero,
                desc: "Tried to divied by 0".to_string()
            });
        }
        product = &product / a;
    }

    return Ok(product);
}

/// `(/ n0 n1 ...)`
pub static PRIM_DIV:Fold1Err<Number> = Fold1Err { fold1: div };

fn list(args: &[RDatum]) -> RDatum {
    FromIterator::from_iter(args.iter().map(Clone::clone))
}

/// `(list a0 a1 ...)`
pub static PRIM_LIST:Fold<RDatum> = Fold { fold: list };

fn car(arg: (RDatum, RDatum)) -> RDatum {
    arg.0
}

/// `(car x)`
pub static PRIM_CAR: F1<(RDatum, RDatum), RDatum> = F1 { f1: car };

fn cdr(arg: (RDatum, RDatum)) -> RDatum {
    arg.1
}

/// `(cdr x)`
pub static PRIM_CDR: F1<(RDatum, RDatum), RDatum> = F1 { f1: cdr };

fn zero(arg: Number) -> bool {
    arg.is_zero()
}

/// `(zero? x)`
pub static PRIM_ZERO: F1<Number, bool> = F1 { f1: zero };

fn is_real(arg: &RDatum) -> bool {
    match arg {
        &Datum::Num(Number::Real(_)) => true,
        _ => false
    }
}

/// `(real? x)`
pub static PRIM_REAL: R1<bool> = R1 { f1: is_real };

fn is_rational(arg: &RDatum) -> bool {
    match arg {
        &Datum::Num(Number::Real(Real::Flonum(f))) => f.is_finite(),
        &Datum::Num(Number::Real(_)) => true,
        _ => false
    }
}

/// `(rational? x)`
pub static PRIM_RATIONAL: R1<bool> = R1 { f1: is_rational };

fn is_integer(arg: &RDatum) -> bool {
    match arg {
        &Datum::Num(Number::Real(ref r)) => r.is_integer(),
        _ => false
    }
}

/// `(integer? x)`
pub static PRIM_INTEGER: R1<bool> = R1 { f1: is_integer };

macro_rules! impl_typecheck {
    ($static_name:ident, $func_name:ident, $type_name:ident) => (
        fn $func_name(arg: &RDatum) -> bool {
            DatumType::get_type(arg) == DatumType::$type_name
        }

        pub static $static_name: R1<bool> = R1 { f1: $func_name };
    )
}

impl_typecheck!(PRIM_BOOLEAN, is_boolean, Bool);
impl_typecheck!(PRIM_PAIR, is_pair, Pair);
impl_typecheck!(PRIM_SYMBOL, is_symbol, Sym);
impl_typecheck!(PRIM_NUMBER, is_number, Num);
impl_typecheck!(PRIM_CHAR, is_char, Char);
impl_typecheck!(PRIM_STRING, is_string, String);
impl_typecheck!(PRIM_VECTOR, is_vector, Vector);
impl_typecheck!(PRIM_PROCEDURE, is_procedure, Callable);
impl_typecheck!(PRIM_NULL, is_null, Null);

/// Lists all primitive functions with its name
pub fn libprimitive() -> Vec<(&'static str, &'static (PrimFunc + 'static))> {
    vec![
        ("+", &PRIM_ADD),
        ("-", &PRIM_SUB),
        ("*", &PRIM_MUL),
        ("/", &PRIM_DIV),
        ("list", &PRIM_LIST),
        ("boolean?", &PRIM_BOOLEAN),
        ("pair?", &PRIM_PAIR),
        ("symbol?", &PRIM_SYMBOL),
        ("number?", &PRIM_NUMBER),
        ("char?", &PRIM_CHAR),
        ("string?", &PRIM_STRING),
        ("vector?", &PRIM_VECTOR),
        ("procedure?", &PRIM_PROCEDURE),
        ("null?", &PRIM_NULL),
        ("car", &PRIM_CAR),
        ("cdr", &PRIM_CDR),
        ("zero?", &PRIM_ZERO),
        // complex? is synonym to number?
        ("complex?", &PRIM_NUMBER),
        ("real?", &PRIM_REAL),
        ("rational?", &PRIM_RATIONAL),
        ("integer?", &PRIM_INTEGER)
    ]
}
