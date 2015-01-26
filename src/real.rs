use std::ops::Add;
use std::num::{Float, FromPrimitive, ToPrimitive, FromStrRadix};
use std::cmp::min;
use std::fmt;
use std::fmt::Writer;
use std::f64;

use num::bigint::{BigInt, ToBigInt};
use num::rational::{Ratio, BigRational};
use num::{Signed, Zero, Integer, CheckedAdd};

#[derive(PartialEq, Debug)]
pub enum Real {
    Fixnum(isize),
    Integer(BigInt),
    Rational(BigRational),
    Flonum(f64)
}

impl fmt::Display for Real {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Real::Fixnum(n) => write!(f, "{}", n),
            &Real::Integer(ref n) => write!(f, "{}", n),
            &Real::Rational(ref n) => write!(f, "{}", n),
            &Real::Flonum(n) => write!(f, "{}", n)
        }
    }
}

#[inline]
pub fn fix2int(n: isize) -> BigInt {
    n.to_bigint().unwrap()
}

#[inline]
pub fn int2rat(n: BigInt) -> BigRational {
    Ratio::from_integer(n)
}

#[inline]
pub fn fix2rat(n: isize) -> BigRational {
    int2rat(fix2int(n))
}

#[inline]
pub fn fix2flo(n: isize) -> f64 {
    FromPrimitive::from_int(n).unwrap()
}

pub fn int2flo(n: &BigInt) -> f64 {
    if n.is_zero() {
        return 0f64;
    }

    let neg = *n < Zero::zero();
    let repr = {
        let mut digits = Vec::new();
        let mut m = n.abs();
        let base: BigInt = FromPrimitive::from_u64(1 << 32).unwrap();
        while m >= base {
            let (q, r) = m.div_mod_floor(&base);
            digits.push(r.to_uint().unwrap());
            m = q;
        }
        if !m.is_zero() {
            digits.push(m.to_uint().unwrap());
        }
        let mut s = String::new();
        for &d in digits.iter().rev() {
            write!(&mut s, "{}", fmt::radix(d, 2)).unwrap();
        }
        s
    };
    let mantissa_repr = &repr[0 .. min(f64::MANTISSA_DIGITS, repr.len())];
    let u_mantissa:i64 = FromStrRadix::from_str_radix(mantissa_repr,
                                                        2).unwrap();
    let i_mantissa = if neg {
        -u_mantissa
    } else {
        u_mantissa
    };
    let exp = (repr.len() - mantissa_repr.len()) as isize;
    let m:f64 = i_mantissa.to_f64().unwrap();
    return Float::ldexp(m, exp);
}

pub fn rat2flo(r: &BigRational) -> f64 {
    int2flo(r.numer()) / int2flo(r.denom())
}

fn coerce<Fix, Big, Rat, Flo>(lhs: Real, rhs: Real,
                              fix_op: Fix, big_op: Big,
                              rat_op: Rat, flo_op: Flo)
        -> Real
    where Fix: Fn(isize, isize) -> Option<isize>,
          Big: Fn(BigInt, BigInt) -> BigInt,
          Rat: Fn(BigRational, BigRational) -> BigRational,
          Flo: Fn(f64, f64) -> f64
{
    match (lhs, rhs) {
        (Real::Fixnum(l), Real::Fixnum(r)) =>
            match fix_op(l, r) {
                Some(n) => Real::Fixnum(n),
                None => Real::Integer(big_op(fix2int(l), fix2int(r)))
            },
        (Real::Fixnum(l), Real::Integer(r)) =>
            Real::Integer(big_op(fix2int(l), r)),
        (Real::Fixnum(l), Real::Rational(r)) =>
            Real::Rational(rat_op(fix2rat(l), r)),
        (Real::Fixnum(l), Real::Flonum(r)) =>
            Real::Flonum(flo_op(fix2flo(l), r)),
        (Real::Integer(l), Real::Fixnum(r)) =>
            Real::Integer(big_op(l, fix2int(r))),
        (Real::Integer(l), Real::Integer(r)) =>
            Real::Integer(big_op(l, r)),
        (Real::Integer(l), Real::Rational(r)) =>
            Real::Rational(rat_op(int2rat(l), r)),
        (Real::Integer(ref l), Real::Flonum(r)) =>
            Real::Flonum(flo_op(int2flo(l), r)),
        (Real::Rational(l), Real::Fixnum(r)) =>
            Real::Rational(rat_op(l, fix2rat(r))),
        (Real::Rational(l), Real::Integer(r)) =>
            Real::Rational(rat_op(l, int2rat(r))),
        (Real::Rational(l), Real::Rational(r)) =>
            Real::Rational(rat_op(l, r)),
        (Real::Rational(ref l), Real::Flonum(r)) =>
            Real::Flonum(flo_op(rat2flo(l), r)),
        (Real::Flonum(l), Real::Fixnum(r)) =>
            Real::Flonum(flo_op(l, fix2flo(r))),
        (Real::Flonum(l), Real::Integer(ref r)) =>
            Real::Flonum(flo_op(l, int2flo(r))),
        (Real::Flonum(l), Real::Rational(ref r)) =>
            Real::Flonum(flo_op(l, rat2flo(r))),
        (Real::Flonum(l), Real::Flonum(r)) =>
            Real::Flonum(flo_op(l, r))
    }
}

impl Add<Real> for Real {
    type Output = Real;

    fn add(self, other: Real) -> <Real as Add<Real>>::Output {
        coerce(self, other,
               |x, y| x.checked_add(&y),
               |x, y| x + y,
               |x, y| x + y,
               |x, y| x + y)
    }
}

#[cfg(test)]
mod test {
    use super::Real;

    #[test]
    fn test_add() {
        assert_eq!(Real::Fixnum(3), Real::Fixnum(1) + Real::Fixnum(2));
    }
}
