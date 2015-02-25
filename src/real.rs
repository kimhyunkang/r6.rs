use std::ops::{Add, Sub, Mul, Div, Neg};
use std::num::{Float, FromPrimitive, ToPrimitive, FromStrRadix};
use std::cmp::min;
use std::fmt;
use std::fmt::Write;
use std::{f64, isize};
use std::cmp::Ordering;

use num::bigint::{BigInt, ToBigInt};
use num::rational::{Ratio, BigRational};
use num::{Signed, Zero, One, Integer, CheckedAdd, CheckedSub, CheckedMul};

#[derive(Debug, Clone)]
pub enum Real {
    Fixnum(isize),
    Integer(BigInt),
    Rational(BigRational),
    Flonum(f64)
}

impl Real {
    pub fn to_f64(&self) -> f64 {
        match self {
            &Real::Fixnum(n) => fix2flo(n),
            &Real::Integer(ref n) => int2flo(n),
            &Real::Rational(ref n) => rat2flo(n),
            &Real::Flonum(n) => n
        }
    }

    pub fn reduce(self) -> Real {
        match self {
            Real::Fixnum(_) =>
                self,
            Real::Integer(n) => {
                let max_fixnum: BigInt = FromPrimitive::from_int(isize::MAX).unwrap();
                let min_fixnum: BigInt = FromPrimitive::from_int(isize::MIN).unwrap();

                if min_fixnum <= n && n <= max_fixnum {
                    Real::Fixnum(n.to_int().unwrap())
                } else {
                    Real::Integer(n)
                }
            },
            Real::Rational(n) =>
                if n.is_integer() {
                    Real::Integer(n.to_integer()).reduce()
                } else {
                    Real::Rational(n)
                },
            Real::Flonum(_) =>
                self
        }
    }

    pub fn to_ratio(self) -> Option<BigRational> {
        match self {
            Real::Fixnum(n) => FromPrimitive::from_int(n).map(Ratio::from_integer),
            Real::Integer(n) => Some(Ratio::from_integer(n)),
            Real::Rational(n) => Some(n),
            Real::Flonum(n) => Ratio::from_float(n)
        }
    }

    pub fn is_exact(&self) -> bool {
        match self {
            &Real::Flonum(_) => false,
            _ => true
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            &Real::Flonum(f) => f.is_finite() && f.trunc() == f,
            _ => true
        }
    }
}

impl Zero for Real {
    fn zero() -> Real {
        Real::Fixnum(0)
    }

    fn is_zero(&self) -> bool {
        match self {
            &Real::Fixnum(n) => n.is_zero(),
            &Real::Integer(ref n) => n.is_zero(),
            &Real::Rational(ref n) => n.is_zero(),
            &Real::Flonum(n) => n.is_zero()
        }
    }
}

impl One for Real {
    fn one() -> Real {
        Real::Fixnum(1)
    }
}

impl Neg for Real {
    type Output = Real;

    fn neg(self) -> Real {
        match self {
            Real::Fixnum(n) => Real::Fixnum(n.neg()),
            Real::Integer(n) => Real::Integer(n.neg()),
            Real::Rational(n) => Real::Rational(n.neg()),
            Real::Flonum(n) => Real::Flonum(n.neg())
        }
    }
}

impl<'a> Neg for &'a Real {
    type Output = Real;

    fn neg(self) -> Real {
        match self {
            &Real::Fixnum(n) => Real::Fixnum(n.neg()),
            &Real::Integer(ref n) => Real::Integer(n.neg()),
            &Real::Rational(ref n) => Real::Rational(n.neg()),
            &Real::Flonum(n) => Real::Flonum(n.neg())
        }
    }
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
pub fn int2rat(n: &BigInt) -> BigRational {
    Ratio::from_integer(n.clone())
}

#[inline]
pub fn fix2rat(n: isize) -> BigRational {
    int2rat(&fix2int(n))
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

fn coerce_arith<Fix, Big, Rat, Flo>(lhs: &Real, rhs: &Real,
                                    fix_op: Fix, big_op: Big,
                                    rat_op: Rat, flo_op: Flo)
        -> Real
    where Fix: Fn(isize, isize) -> Option<isize>,
          Big: Fn(&BigInt, &BigInt) -> BigInt,
          Rat: Fn(&BigRational, &BigRational) -> BigRational,
          Flo: Fn(f64, f64) -> f64
{
    coerce(lhs, rhs,
           |x, y| match fix_op(x, y) {
               Some(n) => Real::Fixnum(n),
               None => Real::Integer(big_op(&fix2int(x), &fix2int(y))).reduce()
           },
           |x, y| Real::Integer(big_op(x, y)).reduce(),
           |x, y| Real::Rational(rat_op(x, y)).reduce(),
           |x, y| Real::Flonum(flo_op(x, y))
    )
}

fn coerce<Fix, Big, Rat, Flo, T>(lhs: &Real, rhs: &Real,
                                 fix_op: Fix, big_op: Big,
                                 rat_op: Rat, flo_op: Flo)
        -> T
    where Fix: Fn(isize, isize) -> T,
          Big: Fn(&BigInt, &BigInt) -> T,
          Rat: Fn(&BigRational, &BigRational) -> T,
          Flo: Fn(f64, f64) -> T
{
    match (lhs, rhs) {
        (&Real::Fixnum(l), &Real::Fixnum(r)) =>
            fix_op(l, r),
        (&Real::Fixnum(l), &Real::Integer(ref r)) =>
            big_op(&fix2int(l), r),
        (&Real::Fixnum(l), &Real::Rational(ref r)) =>
            rat_op(&fix2rat(l), r),
        (&Real::Fixnum(l), &Real::Flonum(r)) =>
            flo_op(fix2flo(l), r),
        (&Real::Integer(ref l), &Real::Fixnum(r)) =>
            big_op(l, &fix2int(r)),
        (&Real::Integer(ref l), &Real::Integer(ref r)) =>
            big_op(l, r),
        (&Real::Integer(ref l), &Real::Rational(ref r)) =>
            rat_op(&int2rat(l), r),
        (&Real::Integer(ref l), &Real::Flonum(r)) =>
            flo_op(int2flo(l), r),
        (&Real::Rational(ref l), &Real::Fixnum(r)) =>
            rat_op(l, &fix2rat(r)),
        (&Real::Rational(ref l), &Real::Integer(ref r)) =>
            rat_op(l, &int2rat(r)),
        (&Real::Rational(ref l), &Real::Rational(ref r)) =>
            rat_op(l, r),
        (&Real::Rational(ref l), &Real::Flonum(r)) =>
            flo_op(rat2flo(l), r),
        (&Real::Flonum(l), &Real::Fixnum(r)) =>
            flo_op(l, fix2flo(r)),
        (&Real::Flonum(l), &Real::Integer(ref r)) =>
            flo_op(l, int2flo(r)),
        (&Real::Flonum(l), &Real::Rational(ref r)) =>
            flo_op(l, rat2flo(r)),
        (&Real::Flonum(l), &Real::Flonum(r)) =>
            flo_op(l, r)
    }
}

macro_rules! impl_arith {
    ($tr:ident, $op:ident, $checked_op:ident) => {
        impl $tr<Real> for Real {
            type Output = Real;

            fn $op(self, other: Real) -> Real {
                coerce_arith(&self, &other,
                             |x, y| x.$checked_op(&y),
                             |x, y| x.$op(y),
                             |x, y| x.$op(y),
                             |x, y| x.$op(&y)
                )
            }
        }

        impl<'a, 'b> $tr<&'a Real> for &'b Real {
            type Output = Real;

            fn $op(self, other: &Real) -> Real {
                coerce_arith(self, other,
                             |x, y| x.$checked_op(&y),
                             |x, y| x.$op(y),
                             |x, y| x.$op(y),
                             |x, y| x.$op(&y)
                )
            }
        }
    }
}

impl_arith!(Add, add, checked_add);
impl_arith!(Sub, sub, checked_sub);
impl_arith!(Mul, mul, checked_mul);

impl Div<Real> for Real {
    type Output = Real;

    fn div(self, other: Real) -> Real {
        coerce(&self, &other,
               |x, y| Real::Rational(Ratio::new(fix2int(x), fix2int(y))),
               |x, y| Real::Rational(Ratio::new(x.clone(), y.clone())),
               |x, y| Real::Rational(x / y),
               |x, y| Real::Flonum(x / y)
        )
    }
}

impl<'a, 'b> Div<&'a Real> for &'b Real {
    type Output = Real;

    fn div(self, other: &Real) -> Real {
        coerce(self, other,
               |x, y| Real::Rational(Ratio::new(fix2int(x), fix2int(y))),
               |x, y| Real::Rational(Ratio::new(x.clone(), y.clone())),
               |x, y| Real::Rational(x / y),
               |x, y| Real::Flonum(x / y)
        )
    }
}

impl PartialEq for Real {
    fn eq(&self, other: &Real) -> bool {
        coerce(self, other,
               |x, y| x == y,
               |x, y| x == y,
               |x, y| x == y,
               |x, y| x == y
        )
    }
}

impl PartialOrd for Real {
    fn partial_cmp(&self, other: &Real) -> Option<Ordering> {
        coerce(self, other,
                    |x, y| x.partial_cmp(&y),
                    |x, y| x.partial_cmp(y),
                    |x, y| x.partial_cmp(y),
                    |x, y| x.partial_cmp(&y)
        )
    }
}

#[cfg(test)]
mod test {
    use super::{Real, int2flo, fix2int};
    use std::num::FromPrimitive;
    use num::rational::Ratio;

    #[test]
    fn test_add() {
        assert_eq!(Real::Fixnum(3), Real::Fixnum(1) + Real::Fixnum(2));
    }

    #[test]
    fn test_coerce() {
        let x = Real::Rational(Ratio::new(fix2int(3), fix2int(2)));
        let y = Real::Fixnum(1);
        let z = Real::Rational(Ratio::new(fix2int(5), fix2int(2)));
        assert_eq!(z, x + y);
    }

    #[test]
    fn test_sub() {
        assert_eq!(Real::Fixnum(1), Real::Fixnum(3) - Real::Fixnum(2));
    }

    #[test]
    fn test_mul() {
        assert_eq!(Real::Fixnum(6), Real::Fixnum(3) * Real::Fixnum(2));
    }

    #[test]
    fn test_div() {
        let x = Real::Fixnum(6);
        let y = Real::Fixnum(4);
        assert_eq!(Real::Rational(Ratio::new(fix2int(3), fix2int(2))), x/y);
    }

    #[test]
    fn test_reduce() {
        let x = Real::Integer(FromPrimitive::from_int(1).unwrap());
        let y = Real::Integer(FromPrimitive::from_int(2).unwrap());
        assert_eq!(Real::Fixnum(3), x+y);
    }

    #[test]
    fn test_reduce_rat() {
        let x = Real::Rational(Ratio::new(fix2int(3), fix2int(2)));
        let y = Real::Rational(Ratio::new(fix2int(1), fix2int(2)));
        assert_eq!(Real::Fixnum(2), x + y);
    }

    #[test]
    fn test_int2flo() {
        let n = FromPrimitive::from_int(3).unwrap();
        assert_eq!(3.0, int2flo(&n));
    }
}
