use std::fmt;

use num::complex::{Complex, Complex64};
use num::rational::BigRational;
use num::Zero;

use real::{Real, int2rat, fix2rat, rat2flo};

#[derive(Debug)]
pub enum Number {
    Real(Real),
    ECmplx(Complex<BigRational>),
    ICmplx(Complex64)
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

impl PartialEq for Number {
    fn eq(&self, other: &Number) -> bool {
        coerce(self, other,
               |x, y| x == y,
               |x, y| x == y,
               |x, y| x == y
        )
    }
}

#[cfg(test)]
mod test {
    use num::complex::Complex;
    use num::{Zero, One};

    use super::Number;
    use real::Real;

    #[test]
    fn test_eq() {
        let cmplx = Number::ECmplx(Complex::new(One::one(), Zero::zero()));
        let fixnum = Number::Real(Real::Fixnum(1));
        assert!(cmplx == fixnum);
    }
}
