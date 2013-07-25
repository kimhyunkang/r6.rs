use std::num::{One, Zero, ToStrRadix, IntConvertible};
use extra::bigint::BigInt;

use complex::Cmplx;
use real::*;
use rational::Rational;

#[deriving(Eq, Clone)]
pub enum LNumeric {
    NReal(LReal),
    NExact(Cmplx<Rational>),
    NInexact(Cmplx<f64>)
}

impl LNumeric {
    pub fn is_inexact(&self) -> bool {
        match *self {
            NReal(ref r) => r.is_inexact(),
            NExact(_) => false,
            NInexact(_) => true,
        }
    }

    pub fn is_exact(&self) -> bool {
        match *self {
            NReal(ref r) => r.is_exact(),
            NExact(_) => true,
            NInexact(_) => false,
        }
    }

    pub fn is_real(&self) -> bool {
        match *self {
            NReal(_) => true,
            NExact(ref c) => c.im.is_zero(),
            NInexact(ref c) => c.im.is_zero(),
        }
    }

    pub fn to_icmplx(&self) -> Cmplx<f64> {
        match *self {
            NReal(ref r) => Cmplx {re: r.to_f64(), im: 0f64},
            NExact(Cmplx { re: ref re, im: ref im }) =>
                Cmplx {re: re.to_float::<f64>(), im: im.to_float::<f64>()},
            NInexact(cmplx) => cmplx
        }
    }

    pub fn to_exact(&self) -> LNumeric {
        match *self {
            NReal(ref r) => NReal(r.to_exact()),
            NExact(ref ecmplx) => NExact(ecmplx.clone()),
            NInexact(ref c) => {
                let re = Rational::from_float(c.re);
                let im = Rational::from_float(c.im);
                if im.is_zero() {
                    NReal(LReal::from_rational(im))
                } else {
                    NExact( Cmplx { re: re, im: im } )
                }
            }
        }
    }

    pub fn to_inexact(&self) -> LNumeric {
        match *self {
            NReal(ref r) => NReal(Rf64(r.to_f64())),
            NExact(ref ecmplx) => NInexact( Cmplx { re: ecmplx.re.to_float(),
                                                    im: ecmplx.im.to_float() } ),
            NInexact(_) => self.clone(),
        }
    }
}

impl ApproxEq<f64> for LNumeric {
    #[inline]
    fn approx_epsilon() -> f64 {
        ApproxEq::approx_epsilon::<f64, f64>() * Real::sqrt2()
    }

    #[inline]
    fn approx_eq(&self, other: &LNumeric) -> bool
    {
        self.approx_eq_eps(other, &ApproxEq::approx_epsilon::<f64, LNumeric>())
    }

    #[inline]
    fn approx_eq_eps(&self, other: &LNumeric, approx_epsilon: &f64) -> bool
    {
        let delta = *self - *other;
        let delta2 = delta.to_icmplx().norm();
        delta2 < *approx_epsilon
    }
}

impl ToStr for LNumeric {
    fn to_str(&self) -> ~str {
        match *self {
            NReal(ref r) => r.to_str(),
            NExact(ref c) => c.to_str(),
            NInexact(ref c) => if c.im.is_negative() {
                fmt!("%s%si", f64_to_str(c.re), f64_to_str(c.im))
            } else {
                fmt!("%s+%si", f64_to_str(c.re), f64_to_str(c.im))
            },
        }
    }
}

impl ToStrRadix for LNumeric {
    fn to_str_radix(&self, radix: uint) -> ~str {
        match *self {
            NReal(ref r) => r.to_str_radix(radix),
            NExact(ref c) => c.to_str_radix(radix),
            NInexact(ref c) => if c.im.is_negative() {
                fmt!("%s%si", f64_to_str_radix(c.re, radix), f64_to_str_radix(c.im, radix))
            } else {
                fmt!("%s+%si", f64_to_str_radix(c.re, radix), f64_to_str_radix(c.im, radix))
            },
        }
    }
}

impl One for LNumeric {
    fn one() -> LNumeric {
        NReal(One::one())
    }
}

impl Zero for LNumeric {
    fn zero() -> LNumeric {
        NReal(Zero::zero())
    }

    fn is_zero(&self) -> bool {
        match *self {
            NReal(ref r) => r.is_zero(),
            NExact(ref cmplx) => cmplx.is_zero(),
            NInexact(ref cmplx) => cmplx.is_zero(),
        }
    }
}

impl Neg<LNumeric> for LNumeric {
    fn neg(&self) -> LNumeric {
        match *self {
            NReal(ref r) => NReal(r.neg()),
            NExact(ref cmplx) => NExact(cmplx.neg()),
            NInexact(ref cmplx) => NInexact(cmplx.neg()),
        }
    }
}

#[inline]
pub fn coerce_cmplx<T>(lhs: &LNumeric, rhs: &LNumeric,
                        real_op: &fn(&LReal, &LReal) -> T,
                        ecmp_op: &fn(&Cmplx<Rational>, &Cmplx<Rational>) -> T,
                        icmp_op: &fn(&Cmplx<f64>, &Cmplx<f64>) -> T) -> T
{
    match (lhs, rhs) {
        (&NReal(ref x), &NReal(ref y)) => real_op(x, y),
        (&NExact(ref x), &NExact(ref y)) => ecmp_op(x, y),
        (&NInexact(ref x), &NInexact(ref y)) => icmp_op(x, y),
        (&NReal(ref x), &NInexact(ref y)) => icmp_op(&Cmplx { re: x.to_f64(), im: 0f64 }, y),
        (&NInexact(ref x), &NReal(ref y)) => icmp_op(x, &Cmplx { re: y.to_f64(), im: 0f64 }),
        (&NExact(ref x), &NInexact(ref y)) => {
            let ix = do x.type_map |r| { r.to_float::<f64>() };
            icmp_op(&ix, y)
        },
        (&NInexact(ref x), &NExact(ref y)) => {
            let iy = do y.type_map |r| { r.to_float::<f64>() };
            icmp_op(x, &iy)
        },
        (&NReal(Rf64(ref x)), &NExact(ref y)) => {
            let ix = Cmplx { re: *x, im: 0f64 };
            let iy = do y.type_map |r| { r.to_float::<f64>() };
            icmp_op(&ix, &iy)
        },
        (&NExact(ref x), &NReal(Rf64(ref y))) => {
            let ix = do x.type_map |r| { r.to_float::<f64>() };
            let iy = Cmplx { re: *y, im: 0f64 };
            icmp_op(&ix, &iy)
        },
        (&NReal(ref x), &NExact(ref y)) => {
            let ix = Cmplx { re: x.to_rational(), im: Zero::zero() };
            ecmp_op(&ix, y)
        },
        (&NExact(ref x), &NReal(ref y)) => {
            let iy = Cmplx { re: y.to_rational(), im: Zero::zero() };
            ecmp_op(x, &iy)
        },
    }
}

#[inline]
pub fn coerce_icmplx<T>(lhs: &LNumeric, rhs: &LNumeric,
                        real_op: &fn(&LReal, &LReal) -> T,
                        icmp_op: &fn(&Cmplx<f64>, &Cmplx<f64>) -> T) -> T
{
    match (lhs, rhs) {
        (&NReal(ref x), &NReal(ref y)) => real_op(x, y),
        (&NExact(ref x), &NExact(ref y)) => {
            let ix = do x.type_map |r| { r.to_float::<f64>() };
            let iy = do y.type_map |r| { r.to_float::<f64>() };
            icmp_op(&ix, &iy)
        },
        (&NInexact(ref x), &NInexact(ref y)) => icmp_op(x, y),
        (&NReal(ref x), &NInexact(ref y)) => icmp_op(&Cmplx { re: x.to_f64(), im: 0f64 }, y),
        (&NInexact(ref x), &NReal(ref y)) => icmp_op(x, &Cmplx { re: y.to_f64(), im: 0f64 }),
        (&NExact(ref x), &NInexact(ref y)) => {
            let ix = do x.type_map |r| { r.to_float::<f64>() };
            icmp_op(&ix, y)
        },
        (&NInexact(ref x), &NExact(ref y)) => {
            let iy = do y.type_map |r| { r.to_float::<f64>() };
            icmp_op(x, &iy)
        },
        (&NReal(ref x), &NExact(ref y)) => {
            let ix = Cmplx { re: x.to_f64(), im: 0f64 };
            let iy = do y.type_map |r| { r.to_float::<f64>() };
            icmp_op(&ix, &iy)
        },
        (&NExact(ref x), &NReal(ref y)) => {
            let ix = do x.type_map |r| { r.to_float::<f64>() };
            let iy = Cmplx { re: y.to_f64(), im: 0f64 };
            icmp_op(&ix, &iy)
        },
    }
}

macro_rules! impl_op(
    ($T:ty, $op:ident) => (
        impl $T for LNumeric {
            #[inline]
            fn $op(&self, other: &LNumeric) -> LNumeric {
                coerce_cmplx(self, other,
                            |x, y| { NReal(x.$op(y)) },
                            |x, y| {
                                let exact = x.$op(y);
                                if exact.im.is_zero() {
                                    NReal(LReal::from_rational(exact.re))
                                } else {
                                    NExact(exact)
                                }
                            },
                            |x, y| { NInexact(x.$op(y)) })
            }
        }
    )
)

impl_op!(Add<LNumeric, LNumeric>, add)
impl_op!(Sub<LNumeric, LNumeric>, sub)
impl_op!(Mul<LNumeric, LNumeric>, mul)
impl_op!(Div<LNumeric, LNumeric>, div)

impl Fractional for LNumeric {
    fn recip(&self) -> LNumeric {
        match *self {
            NReal(ref r) => NReal(r.recip()),
            NExact(ref c) => NExact(c.recip()),
            NInexact(ref c) => NInexact(c.recip()),
        }
    }
}

macro_rules! trans_numeric(
    ($op:ident) => (
        match self {
            &NReal(ref x) => NReal(x.$op()),
            &NExact(ref x) => NInexact(x.type_map(|r| {r.to_float::<f64>()}).$op()),
            &NInexact(ref x) => NInexact(x.$op()),
        }
    );
    ($op:ident, $other:ident) => (
        coerce_icmplx(self, $other,
                    |x, y| { NReal(x.$op(y)) },
                    |x, y| { NInexact(x.$op(y)) })
    )
)

impl Exponential for LNumeric {
    #[inline]
    fn exp(&self) -> LNumeric { trans_numeric!(exp) }
    #[inline]
    fn exp2(&self) -> LNumeric { trans_numeric!(exp2) }
    #[inline]
    fn ln(&self) -> LNumeric { trans_numeric!(ln) }
    #[inline]
    fn log2(&self) -> LNumeric { trans_numeric!(log2) }
    #[inline]
    fn log10(&self) -> LNumeric { trans_numeric!(log10) }
    #[inline]
    fn log(&self, other: &LNumeric) -> LNumeric { trans_numeric!(log, other) }
}

impl Algebraic for LNumeric {
    #[inline]
    fn pow(&self, other: &LNumeric) -> LNumeric { trans_numeric!(pow, other) }
    #[inline]
    fn hypot(&self, other: &LNumeric) -> LNumeric { trans_numeric!(hypot, other) }
    #[inline]
    fn sqrt(&self) -> LNumeric { trans_numeric!(sqrt) }
    #[inline]
    fn rsqrt(&self) -> LNumeric { trans_numeric!(rsqrt) }
    #[inline]
    fn cbrt(&self) -> LNumeric { trans_numeric!(cbrt) }
}

impl Trigonometric for LNumeric {
    #[inline]
    fn sin(&self) -> LNumeric { trans_numeric!(sin) }
    #[inline]
    fn cos(&self) -> LNumeric { trans_numeric!(cos) }
    #[inline]
    fn tan(&self) -> LNumeric { trans_numeric!(tan) }
    #[inline]
    fn asin(&self) -> LNumeric { trans_numeric!(asin) }
    #[inline]
    fn acos(&self) -> LNumeric { trans_numeric!(acos) }
    #[inline]
    fn atan(&self) -> LNumeric { trans_numeric!(atan) }
    #[inline]
    fn atan2(&self, other: &LNumeric) -> LNumeric {
        trans_numeric!(atan2, other)
    }

    #[inline]
    fn sin_cos(&self) -> (LNumeric, LNumeric) {
        (self.sin(), self.cos())
    }
}

pub fn from_int(n: int) -> LNumeric {
    NReal(RInt(IntConvertible::from_int(n)))
}

pub fn from_uint(n: uint) -> LNumeric {
    NReal(RInt(BigInt::from_uint(n)))
}

pub fn from_bigint(n: BigInt) -> LNumeric {
    NReal(RInt(n))
}

pub fn from_rational(r: Rational) -> LNumeric {
    NReal(RRat(r))
}

pub fn from_f64(r: f64) -> LNumeric {
    NReal(Rf64(r))
}

pub fn exact(re: Rational, im: Rational) -> LNumeric {
    if im.is_zero() {
        NReal(RRat(re))
    } else {
        NExact( Cmplx { re: re, im: im } )
    }
}

pub fn inexact(re: f64, im: f64) -> LNumeric {
    NInexact( Cmplx { re: re, im: im } )
}

pub fn polar(norm: f64, arg: f64) -> LNumeric {
    inexact(norm * arg.cos(), norm * arg.sin())
}

pub fn get_int(n: &LNumeric) -> Option<BigInt>
{
    match *n {
        NReal(RInt(ref i)) => Some(i.clone()),
        _ => None,
    }
}

pub fn get_uint(n: &LNumeric) -> Option<uint>
{
    match *n {
        NReal(RInt(ref i)) if !i.is_negative() => Some(i.to_uint()),
        _ => None,
    }
}

#[test]
fn test_log_exp() {
    let x = NInexact( Cmplx { re: 1.0, im: 1.0 } );
    assert_approx_eq!(x.ln().exp(), x)
}

#[test]
fn test_pow() {
    let x = NExact( Cmplx { re: Rational::new_int(2, 1), im: Zero::zero() } );
    let y = NInexact( Cmplx { re: 4.0, im: 0.0 } );
    assert_approx_eq!(x.pow(&x), y)
}

#[test]
fn test_ln() {
    let x = NExact( Cmplx { re: Rational::new_int(2, 1), im: Zero::zero() } );
    let y = NInexact( Cmplx { re: Real::ln_2(), im: 0.0 } );
    assert_approx_eq!(x.ln(), y)
}

#[test]
fn test_mul() {
    let x = NExact( Cmplx { re: Rational::new_int(2, 1), im: Zero::zero() } );
    let y = NInexact( Cmplx { re: 3.0, im: 0.0 } );
    let z = NInexact( Cmplx { re: 6.0, im: 0.0 } );
    assert_eq!(x * y, z)
}
