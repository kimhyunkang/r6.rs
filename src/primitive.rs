use std::rc::Rc;

use error::RuntimeError;
use runtime::{DatumCast, RDatum};

pub static PRIM_ADD:fn(&[RDatum]) -> Result<RDatum, RuntimeError> = add;

fn add(args: &[RDatum]) -> Result<RDatum, RuntimeError> {
    let mut sum:isize = 0;
    for arg in args.iter() {
        let a = try!(DatumCast::unwrap(arg));
        sum += a;
    }
    return Ok(sum.wrap());
}

pub fn libprimitive() -> Vec<(&'static str, Rc<fn(&[RDatum]) -> Result<RDatum, RuntimeError>>)> {
    vec![
        ("+", Rc::new(PRIM_ADD))
    ]
}
