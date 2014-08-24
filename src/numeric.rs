use std::num::{One, Zero, ToStrRadix, FromPrimitive};
use std::fmt;
use num::bigint::BigInt;
use num::complex::Complex;
use num::rational::BigRational;

use real::{LReal, RInt, RRat, Rf64, f64_fmt, f64_to_str_radix};
use bigint_helper::{rational_from_float, rational_to_f64};

#[deriving(PartialEq, Clone)]
pub enum LNumeric {
    NReal(LReal),
    NExact(Complex<BigRational>),
    NInexact(Complex<f64>)
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

    pub fn to_icmplx(&self) -> Complex<f64> {
        match *self {
            NReal(ref r) => Complex {re: r.to_f64(), im: 0f64},
            NExact(Complex { re: ref re, im: ref im }) =>
                Complex {re: rational_to_f64(re), im: rational_to_f64(im)},
            NInexact(cmplx) => cmplx
        }
    }

    pub fn to_exact(&self) -> LNumeric {
        match *self {
            NReal(ref r) => NReal(r.to_exact()),
            NExact(ref ecmplx) => NExact(ecmplx.clone()),
            NInexact(ref c) => {
                let re = rational_from_float(c.re);
                let im = rational_from_float(c.im);
                if im.is_zero() {
                    NReal(LReal::from_rational(im))
                } else {
                    NExact( Complex { re: re, im: im } )
                }
            }
        }
    }

    pub fn to_inexact(&self) -> LNumeric {
        match *self {
            NReal(ref r) => NReal(Rf64(r.to_f64())),
            NExact(ref ecmplx) => NInexact( Complex { re: rational_to_f64(&ecmplx.re),
                                                    im: rational_to_f64(&ecmplx.im) } ),
            NInexact(_) => self.clone(),
        }
    }
}

impl fmt::Show for LNumeric {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            NReal(ref r) => r.fmt(f),
            NExact(ref c) => c.fmt(f),
            NInexact(ref c) => if c.im.is_negative() {
                try!(f64_fmt(c.re, f))
                try!(f64_fmt(c.im, f))
                write!(f, "i")
            } else {
                try!(f64_fmt(c.re, f))
                try!(write!(f, "+"))
                try!(f64_fmt(c.im, f))
                write!(f, "i")
            },
        }
    }
}

#[allow(deprecated)]
impl ToStrRadix for LNumeric {
    fn to_str_radix(&self, radix: uint) -> String {
        match *self {
            NReal(ref r) => r.to_str_radix(radix),
            NExact(ref c) => c.to_str_radix(radix),
            NInexact(ref c) => if c.im.is_negative() {
                format!("{}{}i", f64_to_str_radix(c.re, radix), f64_to_str_radix(c.im, radix))
            } else {
                format!("{}+{}i", f64_to_str_radix(c.re, radix), f64_to_str_radix(c.im, radix))
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

pub fn cmplx_map<T, U>(c: &Complex<T>, f: |&T| -> U) -> Complex<U> {
    Complex { re: f(&c.re), im: f(&c.im) }
}

pub fn coerce_cmplx<T>(lhs: &LNumeric, rhs: &LNumeric,
                        real_op: |&LReal, &LReal| -> T,
                        ecmp_op: |&Complex<BigRational>, &Complex<BigRational>| -> T,
                        icmp_op: |&Complex<f64>, &Complex<f64>| -> T) -> T
{
    match (lhs, rhs) {
        (&NReal(ref x), &NReal(ref y)) => real_op(x, y),
        (&NExact(ref x), &NExact(ref y)) => ecmp_op(x, y),
        (&NInexact(ref x), &NInexact(ref y)) => icmp_op(x, y),
        (&NReal(ref x), &NInexact(ref y)) => icmp_op(&Complex { re: x.to_f64(), im: 0f64 }, y),
        (&NInexact(ref x), &NReal(ref y)) => icmp_op(x, &Complex { re: y.to_f64(), im: 0f64 }),
        (&NExact(ref x), &NInexact(ref y)) => {
            let ix = cmplx_map(x, |r| rational_to_f64(r));
            icmp_op(&ix, y)
        },
        (&NInexact(ref x), &NExact(ref y)) => {
            let iy = cmplx_map(y, |r| rational_to_f64(r));
            icmp_op(x, &iy)
        },
        (&NReal(Rf64(ref x)), &NExact(ref y)) => {
            let ix = Complex { re: *x, im: 0f64 };
            let iy = cmplx_map(y, |r| rational_to_f64(r));
            icmp_op(&ix, &iy)
        },
        (&NExact(ref x), &NReal(Rf64(ref y))) => {
            let ix = cmplx_map(x, |r| rational_to_f64(r));
            let iy = Complex { re: *y, im: 0f64 };
            icmp_op(&ix, &iy)
        },
        (&NReal(ref x), &NExact(ref y)) => {
            let ix = Complex { re: x.to_rational(), im: Zero::zero() };
            ecmp_op(&ix, y)
        },
        (&NExact(ref x), &NReal(ref y)) => {
            let iy = Complex { re: y.to_rational(), im: Zero::zero() };
            ecmp_op(x, &iy)
        },
    }
}

pub fn coerce_icmplx<T>(lhs: &LNumeric, rhs: &LNumeric,
                        real_op: |&LReal, &LReal| -> T,
                        icmp_op: |&Complex<f64>, &Complex<f64>| -> T) -> T
{
    match (lhs, rhs) {
        (&NReal(ref x), &NReal(ref y)) => real_op(x, y),
        (&NExact(ref x), &NExact(ref y)) => {
            let ix = cmplx_map(x, |r| rational_to_f64(r));
            let iy = cmplx_map(y, |r| rational_to_f64(r));
            icmp_op(&ix, &iy)
        },
        (&NInexact(ref x), &NInexact(ref y)) => icmp_op(x, y),
        (&NReal(ref x), &NInexact(ref y)) => icmp_op(&Complex { re: x.to_f64(), im: 0f64 }, y),
        (&NInexact(ref x), &NReal(ref y)) => icmp_op(x, &Complex { re: y.to_f64(), im: 0f64 }),
        (&NExact(ref x), &NInexact(ref y)) => {
            let ix = cmplx_map(x, |r| rational_to_f64(r));
            icmp_op(&ix, y)
        },
        (&NInexact(ref x), &NExact(ref y)) => {
            let iy = cmplx_map(y, |r| rational_to_f64(r));
            icmp_op(x, &iy)
        },
        (&NReal(ref x), &NExact(ref y)) => {
            let ix = Complex { re: x.to_f64(), im: 0f64 };
            let iy = cmplx_map(y, |r| rational_to_f64(r));
            icmp_op(&ix, &iy)
        },
        (&NExact(ref x), &NReal(ref y)) => {
            let ix = cmplx_map(x, |r| rational_to_f64(r));
            let iy = Complex { re: y.to_f64(), im: 0f64 };
            icmp_op(&ix, &iy)
        },
    }
}

macro_rules! impl_op(
    ($T:ty, $op:ident) => (
        impl $T for LNumeric {
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

pub fn numeric_abs(n: &LNumeric) -> LReal {
    match n {
        &NReal(ref x) => x.abs(),
        _ => Rf64(n.to_icmplx().norm()),
    }
}

pub fn from_int(n: int) -> LNumeric {
    NReal(RInt(FromPrimitive::from_int(n).unwrap()))
}

pub fn from_uint(n: uint) -> LNumeric {
    NReal(RInt(FromPrimitive::from_uint(n).unwrap()))
}

pub fn from_bigint(n: BigInt) -> LNumeric {
    NReal(RInt(n))
}

pub fn from_rational(r: BigRational) -> LNumeric {
    NReal(RRat(r))
}

pub fn from_f64(r: f64) -> LNumeric {
    NReal(Rf64(r))
}

pub fn exact(re: BigRational, im: BigRational) -> LNumeric {
    if im.is_zero() {
        NReal(RRat(re))
    } else {
        NExact( Complex { re: re, im: im } )
    }
}

pub fn inexact(re: f64, im: f64) -> LNumeric {
    NInexact( Complex { re: re, im: im } )
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
        NReal(RInt(ref i)) if !i.is_negative() => i.to_uint(),
        _ => None,
    }
}

/*
#[test]
fn test_log_exp() {
    let x = NInexact( Complex { re: 1.0, im: 1.0 } );
    assert!((x.ln().exp() - x).abs().to_f64() < 0.0001)
}

#[test]
fn test_pow() {
    let _2: BigInt = FromPrimitive::from_int(2).unwrap();
    let _1: BigInt = FromPrimitive::from_int(1).unwrap();
    let x = NExact( Complex { re: Ratio::new(_2, _1), im: Zero::zero() } );
    let y = NInexact( Complex { re: 4.0, im: 0.0 } );
    assert!((x.pow(&x) - y).abs().to_f64() < 0.0001)
}

#[test]
fn test_ln() {
    use std::num::Float;

    let _2: BigInt = FromPrimitive::from_int(2).unwrap();
    let _1: BigInt = FromPrimitive::from_int(1).unwrap();
    let x = NExact( Complex { re: Ratio::new(_2, _1), im: Zero::zero() } );
    let y = NInexact( Complex { re: Float::ln_2(), im: 0.0 } );
    assert!((x.ln() - y).abs().to_f64() < 0.0001)
}
*/

#[test]
fn test_mul() {
    use num::rational::Ratio;

    let _2: BigInt = FromPrimitive::from_int(2).unwrap();
    let _1: BigInt = FromPrimitive::from_int(1).unwrap();
    let x = NExact( Complex { re: Ratio::new(_2, _1), im: Zero::zero() } );
    let y = NInexact( Complex { re: 3.0, im: 0.0 } );
    let z = NInexact( Complex { re: 6.0, im: 0.0 } );
    assert_eq!(x * y, z)
}
