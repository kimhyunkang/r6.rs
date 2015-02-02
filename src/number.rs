use std::ops::{Add, Sub, Mul, Div, Neg};
use std::fmt;
use std::num::FromPrimitive;

use num::complex::{Complex, Complex64};
use num::bigint::BigInt;
use num::rational::{Ratio, BigRational};
use num::{Zero, One};

use real::{Real, int2rat, fix2rat, rat2flo};

#[derive(Clone)]
pub enum Number {
    Real(Real),
    ECmplx(Complex<BigRational>),
    ICmplx(Complex64)
}

impl fmt::Debug for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Number::Real(ref r) => write!(f, "{}", r),
            &Number::ECmplx(ref c) => write!(f, "#e{}", c),
            &Number::ICmplx(ref c) => write!(f, "#i{}", c),
        }
    }
}

impl Number {
    pub fn new_int(re: isize, im: isize) -> Number {
        if im == 0 {
            Number::Real(Real::Fixnum(re))
        } else {
            let re_part = Ratio::new(
                FromPrimitive::from_int(re).unwrap(),
                One::one()
            );
            let im_part = Ratio::new(
                FromPrimitive::from_int(im).unwrap(),
                One::one()
            );
            Number::ECmplx(Complex::new(re_part, im_part))
        }
    }

    pub fn new_exact(re: (isize, isize), im: (isize, isize)) -> Number {
        let re_part = Ratio::new(
                FromPrimitive::from_int(re.0).unwrap(),
                FromPrimitive::from_int(re.1).unwrap()
        );

        let im_part = Ratio::new(
                FromPrimitive::from_int(im.0).unwrap(),
                FromPrimitive::from_int(im.1).unwrap()
        );

        if im_part.is_zero() {
            Number::Real(Real::Rational(re_part))
        } else {
            Number::ECmplx(Complex::new(re_part, im_part))
        }
    }

    pub fn new_inexact(re: f64, im: f64) -> Number {
        Number::ICmplx(Complex::new(re, im))
    }

    pub fn new_flonum(re: f64) -> Number {
        Number::Real(Real::Flonum(re))
    }

    pub fn new_imag(im: Real) -> Number {
        match im {
            Real::Flonum(i) => Number::new_inexact(0.0, i),
            _ => Number::ECmplx(Complex::new(Zero::zero(), im.to_ratio().unwrap()))
        }
    }

    pub fn new(re: Real, im: Real) -> Number {
        if re.is_exact() && im.is_exact() {
            if im.is_zero() {
                Number::Real(re)
            } else {
                Number::ECmplx(Complex::new(re.to_ratio().unwrap(), im.to_ratio().unwrap()))
            }
        } else {
            Number::new_inexact(re.to_f64(), im.to_f64())
        }
    }

    pub fn new_ratio(re: isize, im: isize) -> Number {
        let re_big = FromPrimitive::from_int(re).unwrap();
        let im_big = FromPrimitive::from_int(im).unwrap();
        Number::Real(Real::Rational(Ratio::new(re_big, im_big)).reduce())
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Number::Real(ref r) => write!(f, "{}", r),
            &Number::ECmplx(ref c) => write!(f, "{}", c),
            &Number::ICmplx(ref c) => write!(f, "{}", c)
        }
    }
}

impl Zero for Number {
    fn zero() -> Number {
        Number::Real(Real::Fixnum(0))
    }

    fn is_zero(&self) -> bool {
        match self {
            &Number::Real(ref r) => r.is_zero(),
            &Number::ECmplx(ref e) => e.is_zero(),
            &Number::ICmplx(ref i) => i.is_zero()
        }
    }
}

impl One for Number {
    fn one() -> Number {
        Number::Real(Real::Fixnum(1))
    }
}

impl Neg for Number {
    type Output = Number;

    fn neg(self) -> Number {
        match self {
            Number::Real(r) => Number::Real(r.neg()),
            Number::ECmplx(r) => Number::ECmplx(r.neg()),
            Number::ICmplx(r) => Number::ICmplx(r.neg())
        }
    }
}

impl FromPrimitive for Number {
    fn from_i64(n: i64) -> Option<Number> {
        let i: Option<BigInt> = FromPrimitive::from_i64(n);
        i.map(|n| Number::Real(Real::Integer(n).reduce()))
    }

    fn from_u64(n: u64) -> Option<Number> {
        let u: Option<BigInt> = FromPrimitive::from_u64(n);
        u.map(|n| Number::Real(Real::Integer(n).reduce()))
    }
}

pub fn inexact(n: &Complex<BigRational>) -> Complex64 {
    Complex::new(rat2flo(&n.re), rat2flo(&n.im))
}

pub fn complex(n: &Real) -> Result<Complex<BigRational>, Complex64> {
    match n {
        &Real::Fixnum(n) => Ok(Complex::new(fix2rat(n), Zero::zero())),
        &Real::Integer(ref n) => Ok(Complex::new(int2rat(n), Zero::zero())),
        &Real::Rational(ref n) => Ok(Complex::new(n.clone(), Zero::zero())),
        &Real::Flonum(f) => Err(Complex::new(f, 0.0))
    }
}

fn coerce<R, E, I, T>(lhs: &Number, rhs: &Number,
                      r_op: R, e_op: E, i_op: I)
        -> T
    where R: Fn(&Real, &Real) -> T,
          E: Fn(&Complex<BigRational>, &Complex<BigRational>) -> T,
          I: Fn(&Complex64, &Complex64) -> T
{
    match (lhs, rhs) {
        (&Number::Real(ref l), &Number::Real(ref r)) =>
            r_op(l, r),
        (&Number::Real(ref l), &Number::ECmplx(ref r)) =>
            match complex(l) {
                Ok(e) => e_op(&e, r),
                Err(i) => i_op(&i, &inexact(r))
            },
        (&Number::Real(ref l), &Number::ICmplx(ref r)) =>
            i_op(&Complex::new(l.to_f64(), Zero::zero()), r),
        (&Number::ECmplx(ref l), &Number::Real(ref r)) =>
            match complex(r) {
                Ok(e) => e_op(l, &e),
                Err(i) => i_op(&inexact(l), &i)
            },
        (&Number::ECmplx(ref l), &Number::ECmplx(ref r)) =>
            e_op(l, r),
        (&Number::ECmplx(ref l), &Number::ICmplx(ref r)) =>
            i_op(&inexact(l), r),
        (&Number::ICmplx(ref l), &Number::Real(ref r)) =>
            i_op(l, &Complex::new(r.to_f64(), Zero::zero())),
        (&Number::ICmplx(ref l), &Number::ECmplx(ref r)) =>
            i_op(l, &inexact(r)),
        (&Number::ICmplx(ref l), &Number::ICmplx(ref r)) =>
            i_op(l, r)
    }
}

fn coerce_arith<R, E, I>(lhs: &Number, rhs: &Number,
                         r_op: R, e_op: E, i_op: I)
        -> Number
    where R: Fn(&Real, &Real) -> Real,
          E: Fn(&Complex<BigRational>, &Complex<BigRational>) -> Complex<BigRational>,
          I: Fn(&Complex64, &Complex64) -> Complex64
{
    coerce(lhs, rhs, 
           |x, y| Number::Real(r_op(x, y)),
           |x, y| Number::ECmplx(e_op(x, y)),
           |x, y| Number::ICmplx(i_op(x, y))
    )
}

impl PartialEq for Number {
    fn eq(&self, other: &Number) -> bool {
        coerce(self, other,
               |x, y| x == y,
               |x, y| x == y,
               |x, y| x == y
        )
    }
}

macro_rules! impl_arith {
    ($tr:ident, $op:ident) => {
        impl $tr<Number> for Number {
            type Output = Number;

            fn $op(self, other: Number) -> Number {
                coerce_arith(&self, &other,
                             |x, y| x.$op(y),
                             |x, y| x.$op(y),
                             |x, y| x.$op(y)
                )
            }
        }

        impl<'a, 'b> $tr<&'a Number> for &'b Number {
            type Output = Number;

            fn $op(self, other: &Number) -> Number {
                coerce_arith(self, other,
                             |x, y| x.$op(y),
                             |x, y| x.$op(y),
                             |x, y| x.$op(y)
                )
            }
        }
    }
}

impl_arith!(Add, add);
impl_arith!(Sub, sub);
impl_arith!(Mul, mul);
impl_arith!(Div, div);

#[cfg(test)]
mod test {
    use super::Number;
    use real::Real;

    #[test]
    fn test_eq() {
        let cmplx = Number::new_int(1, 0);
        let fixnum = Number::Real(Real::Fixnum(1));
        assert!(cmplx == fixnum);
    }

    #[test]
    fn test_add() {
        let cmplx11 = Number::new_int(1, 1);
        let fixnum = Number::Real(Real::Fixnum(1));
        let cmplx21 = Number::new_int(2, 1);
        assert_eq!(cmplx21, fixnum + cmplx11);
    }

    #[test]
    fn test_sub() {
        let cmplx31 = Number::new_inexact(3.0, 1.0);
        let fixnum = Number::Real(Real::Fixnum(2));
        let cmplx11 = Number::new_inexact(1.0, 1.0);
        assert_eq!(cmplx11, cmplx31 - fixnum);
    }

    #[test]
    fn test_mul() {
        let cmplx21 = Number::new_int(2, 1);
        let cmplx31 = Number::new_int(3, 1);
        let cmplx55 = Number::new_int(5, 5);
        assert_eq!(cmplx55, cmplx21 * cmplx31);
    }

    #[test]
    fn test_div() {
        let cmplx21 = Number::new_inexact(2.0, 1.0);
        let cmplx31 = Number::new_int(3, 1);
        let cmplx55 = Number::new_inexact(5.0, 5.0);
        assert_eq!(cmplx21, cmplx55 / cmplx31);
    }
}
