use std::rc::Rc;
use std::ops::Neg;
use num::{Zero, One};

use number::Number;
use error::{RuntimeError, RuntimeErrorKind};
use runtime::{DatumCast, RDatum};
use datum::Datum;

/// `(+ n0 n1 ...)`
pub static PRIM_ADD:fn(&[RDatum]) -> Result<RDatum, RuntimeError> = add;

fn add(args: &[RDatum]) -> Result<RDatum, RuntimeError> {
    let mut sum:Number = Zero::zero();
    for arg in args.iter() {
        let a = try!(DatumCast::unwrap(arg));
        sum = sum + a;
    }
    return Ok(sum.wrap());
}

/// `(* n0 n1 ...)`
pub static PRIM_MUL:fn(&[RDatum]) -> Result<RDatum, RuntimeError> = mul;

fn mul(args: &[RDatum]) -> Result<RDatum, RuntimeError> {
    let mut product:Number = One::one();
    for arg in args.iter() {
        let a = try!(DatumCast::unwrap(arg));
        product = product * a;
    }
    return Ok(product.wrap());
}

/// `(- n0 n1 ...)`
pub static PRIM_SUB:fn(&[RDatum]) -> Result<RDatum, RuntimeError> = sub;

fn sub(args: &[RDatum]) -> Result<RDatum, RuntimeError> {
    match args.len() {
        0 => return Err(RuntimeError { kind: RuntimeErrorKind::NumArgs, desc: "Expected at least 1 arguments, received 0".to_string() }),
        1 => {
            let n: Number = try!(DatumCast::unwrap(&args[0]));
            return Ok(n.neg().wrap());
        },
        _ => ()
    }

    let mut it = args.iter();
    let mut sum:Number = try!(DatumCast::unwrap(it.next().unwrap()));

    for arg in it {
        let a = try!(DatumCast::unwrap(arg));
        sum = sum - a;
    }
    return Ok(sum.wrap());
}

/// `(/ n0 n1 ...)`
pub static PRIM_DIV:fn(&[RDatum]) -> Result<RDatum, RuntimeError> = div;

fn div(args: &[RDatum]) -> Result<RDatum, RuntimeError> {
    match args.len() {
        0 => return Err(RuntimeError { kind: RuntimeErrorKind::NumArgs, desc: "Expected at least 1 arguments, received 0".to_string() }),
        1 => {
            let n: Number = try!(DatumCast::unwrap(&args[0]));
            if n.is_exact() && n.is_zero() {
                return Err(RuntimeError {
                    kind: RuntimeErrorKind::DivideByZero,
                    desc: "Tried to divied by 0".to_string()
                });
            } else {
                let _1: Number = One::one();
                return Ok(Datum::Num(_1 / n));
            }
        },
        _ => ()
    }

    let mut it = args.iter();
    let mut product:Number = try!(DatumCast::unwrap(it.next().unwrap()));

    for arg in it {
        let a: Number = try!(DatumCast::unwrap(arg));
        if a.is_exact() && a.is_zero() {
            return Err(RuntimeError {
                kind: RuntimeErrorKind::DivideByZero,
                desc: "Tried to divied by 0".to_string()
            });
        }
        product = product / a;
    }
    return Ok(product.wrap());
}

/// Lists all primitive functions with its name
pub fn libprimitive() -> Vec<(&'static str, Rc<fn(&[RDatum]) -> Result<RDatum, RuntimeError>>)> {
    vec![
        ("+", Rc::new(PRIM_ADD)),
        ("-", Rc::new(PRIM_SUB)),
        ("*", Rc::new(PRIM_MUL)),
        ("/", Rc::new(PRIM_DIV))
    ]
}
