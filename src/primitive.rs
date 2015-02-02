use num::{Zero, One};

use number::Number;
use error::{RuntimeError, RuntimeErrorKind};
use runtime::{DatumCast, RDatum};

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

impl<T> PrimFunc for Fold<T> where T: DatumCast {
    fn call(&self, args: &[RDatum]) -> Result<RDatum, RuntimeError> {
        let p_args:Result<Vec<T>, RuntimeError> = args.iter().map(DatumCast::unwrap).collect();
        let f = self.fold;
        p_args.map(|v| f(v.as_slice()).wrap())
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

/// Lists all primitive functions with its name
pub fn libprimitive() -> Vec<(&'static str, &'static (PrimFunc + 'static))> {
    vec![
        ("+", &PRIM_ADD),
        ("-", &PRIM_SUB),
        ("*", &PRIM_MUL),
        ("/", &PRIM_DIV)
    ]
}
