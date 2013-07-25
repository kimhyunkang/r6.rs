use std::num::{Zero, One, ToStrRadix};
use extra::bigint::BigInt;
use rational::Rational;
use bigint_helper::*;

#[deriving(Clone)]
pub enum LReal {
    RInt(BigInt),
    RRat(Rational),
    Rf64(f64),
}

#[inline]
pub fn coerce_real<T>(lhs: &LReal, rhs: &LReal,
                    int_op: &fn(&BigInt, &BigInt) -> T,
                    rat_op: &fn(&Rational, &Rational) -> T,
                    f64_op: &fn(&f64, &f64) -> T) -> T
{
    match (lhs, rhs) {
        (&RInt(ref x), &RInt(ref y)) => int_op(x, y),
        (&RInt(ref x), &RRat(ref y)) => rat_op(&Rational::new(x.clone(), One::one()), y),
        (&RInt(ref x), &Rf64(ref y)) => f64_op(&bigint_to_float(x), y),
        (&RRat(ref x), &RInt(ref y)) => rat_op(x, &Rational::new(y.clone(), One::one())),
        (&RRat(ref x), &RRat(ref y)) => rat_op(x, y),
        (&RRat(ref x), &Rf64(ref y)) => f64_op(&x.to_float(), y),
        (&Rf64(ref x), &RInt(ref y)) => f64_op(x, &bigint_to_float(y)),
        (&Rf64(ref x), &RRat(ref y)) => f64_op(x, &y.to_float()),
        (&Rf64(ref x), &Rf64(ref y)) => f64_op(x, y),
    }
}

#[inline]
pub fn coerce_barrier<T>(lhs: &LReal, rhs: &LReal,
                    rat_op: &fn(&Rational, &Rational) -> T,
                    f64_op: &fn(&f64, &f64) -> T) -> Option<T>
{
    match (lhs, rhs) {
        (&RInt(ref x), &RInt(ref y)) => {
            let rx = Rational::new(x.clone(), One::one());
            let ry = Rational::new(y.clone(), One::one());
            Some(rat_op(&rx, &ry))
        },
        (&RInt(ref x), &RRat(ref y)) => Some(rat_op(&Rational::new(x.clone(), One::one()), y)),
        (&RRat(ref x), &RInt(ref y)) => Some(rat_op(x, &Rational::new(y.clone(), One::one()))),
        (&RRat(ref x), &RRat(ref y)) => Some(rat_op(x, y)),
        (&Rf64(ref x), &Rf64(ref y)) => Some(f64_op(x, y)),
        _ => None,
    }
}

impl LReal {
    pub fn to_f64(&self) -> f64 {
        match self {
            &RInt(ref x) => bigint_to_float(x),
            &RRat(ref x) => x.to_float(),
            &Rf64(x) => x,
        }
    }

    pub fn to_rational(&self) -> Rational {
        match self {
            &RInt(ref x) => Rational::from_bigint(x.clone()),
            &RRat(ref x) => x.clone(),
            &Rf64(ref x) => Rational::from_float(*x),
        }
    }

    pub fn to_exact(&self) -> LReal {
        match self {
            &RInt(_) => self.clone(),
            &RRat(_) => self.clone(),
            &Rf64(x) => {
                let rat = Rational::from_float(x);
                if *rat.numerator() == One::one() {
                    RInt(rat.denominator().clone())
                } else {
                    RRat(rat)
                }
            }
        }
    }

    pub fn from_rational(rat: Rational) -> LReal {
        if *rat.numerator() == One::one() {
            RInt(rat.denominator().clone())
        } else {
            RRat(rat)
        }
    }

    pub fn is_exact(&self) -> bool {
        match self {
            &Rf64(_) => false,
            _ => true,
        }
    }

    pub fn is_inexact(&self) -> bool {
        match self {
            &Rf64(_) => true,
            _ => false,
        }
    }
}

impl Eq for LReal {
    #[inline]
    fn eq(&self, other: &LReal) -> bool {
        match coerce_barrier(self, other, |x, y| { x == y }, |x, y| { x == y }) {
            Some(true) => true,
            _ => false,
        }
    }

    #[inline]
    fn ne(&self, other: &LReal) -> bool {
        match coerce_barrier(self, other, |x, y| { x != y }, |x, y| { x != y }) {
            Some(false) => false,
            _ => true,
        }
    }
}

impl ApproxEq<f64> for LReal {
    #[inline]
    fn approx_epsilon() -> f64 {
        ApproxEq::approx_epsilon::<f64, f64>()
    }

    #[inline]
    fn approx_eq(&self, other: &LReal) -> bool {
        self.approx_eq_eps(other, &ApproxEq::approx_epsilon::<f64, f64>())
    }

    #[inline]
    fn approx_eq_eps(&self, other: &LReal, approx_epsilon: &f64) -> bool {
        self.to_f64().approx_eq_eps(&other.to_f64(), approx_epsilon)
    }
}


macro_rules! impl_coerce (
    ($op:ident) => (
        coerce_real(self, other,
                    |x, y| { x.$op(y) },
                    |x, y| { x.$op(y) },
                    |x, y| { x.$op(y) })
    )
)

macro_rules! impl_coerce_trans (
    ($op:ident) => (
        coerce_real(self, other,
                    |x, y| { RInt(x.$op(y)) },
                    |x, y| {
                        let rat = x.$op(y);
                        if *rat.numerator() == One::one() {
                            RInt(rat.denominator().clone())
                        } else {
                            RRat(rat)
                        }
                    },
                    |x, y| { Rf64(x.$op(y)) })
    )
)

impl Ord for LReal {
    fn gt(&self, other: &LReal) -> bool { impl_coerce!(gt) }
    fn ge(&self, other: &LReal) -> bool { impl_coerce!(ge) }
    fn lt(&self, other: &LReal) -> bool { impl_coerce!(lt) }
    fn le(&self, other: &LReal) -> bool { impl_coerce!(le) }
}

impl Orderable for LReal {
    /// Returns `NaN` if either of the numbers are `NaN`.
    #[inline]
    fn min(&self, other: &LReal) -> LReal {
        if (*self < *other) {
            self.clone()
        } else {
            other.clone()
        }
    }

    /// Returns `NaN` if either of the numbers are `NaN`.
    #[inline]
    fn max(&self, other: &LReal) -> LReal {
        if (*self > *other) {
            self.clone()
        } else {
            other.clone()
        }
    }

    /// Returns the number constrained within the range `mn <= self <= mx`.
    /// If any of the numbers are `NaN` then `NaN` is returned.
    #[inline]
    fn clamp(&self, mn: &LReal, mx: &LReal) -> LReal {
        cond!(
            (!(*self <= *mx)) { mx.clone() }
            (!(*self >= *mn)) { mn.clone() }
            _                 { self.clone() }
        )
    }
}

macro_rules! impl_op(
    ($T:ty, $op:ident) => (
        impl $T for LReal {
            #[inline]
            fn $op(&self, other: &LReal) -> LReal {
                impl_coerce_trans!($op)
            }
        }
    )
)

impl_op!(Add<LReal, LReal>, add)
impl_op!(Sub<LReal, LReal>, sub)
impl_op!(Mul<LReal, LReal>, mul)

impl Div<LReal, LReal> for LReal {
    #[inline]
    fn div(&self, other: &LReal) -> LReal {
        coerce_real(self, other,
                    |x, y| {
                        let rat = Rational::new(x.clone(), y.clone());
                        if *rat.numerator() == One::one() {
                            RInt(rat.denominator().clone())
                        } else {
                            RRat(rat)
                        }
                    },
                    |x, y| {
                        let rat = x.div(y);
                        if *rat.numerator() == One::one() {
                            RInt(rat.denominator().clone())
                        } else {
                            RRat(rat)
                        }
                    },
                    |x, y| { Rf64(x.div(y)) })
    }
}

impl Num for LReal {}

macro_rules! coerce(
    ($x:ident, $e:expr) => (
    )
)

macro_rules! meth(
    ($op:ident) => (
        match self {
            &RInt(ref x) => x.$op(),
            &RRat(ref x) => x.$op(),
            &Rf64(ref x) => x.$op(),
        }
    )
)

macro_rules! trans(
    ($op:ident) => (
        match self {
            &RInt(ref x) => RInt(x.$op()),
            &RRat(ref x) => RRat(x.$op()),
            &Rf64(ref x) => Rf64(x.$op()),
        }
    )
)

impl ToStr for LReal {
    fn to_str(&self) -> ~str {
        meth!(to_str)
    }
}

impl ToStrRadix for LReal {
    fn to_str_radix(&self, radix: uint) -> ~str {
        match self {
            &RInt(ref x) => x.to_str_radix(radix),
            &RRat(ref x) => x.to_str_radix(radix),
            &Rf64(ref x) => x.to_str_radix(radix),
        }
    }
}

impl Neg<LReal> for LReal {
    #[inline]
    fn neg(&self) -> LReal { 
        trans!(neg)
    }
}

impl Zero for LReal {
    #[inline]
    fn zero() -> LReal { RInt(Zero::zero()) }

    /// Returns true if the number is equal to either `0.0` or `-0.0`
    #[inline]
    fn is_zero(&self) -> bool {
        meth!(is_zero)
    }
}

impl One for LReal {
    #[inline]
    fn one() -> LReal { RInt(One::one()) }
}

impl Signed for LReal {
    #[inline]
    fn abs(&self) -> LReal {
        if self.is_negative() {
            self.neg()
        } else {
            self.clone()
        }
    }

    #[inline]
    fn abs_sub(&self, other: &LReal) -> LReal {
        impl_coerce_trans!(abs_sub)
    }

    #[inline]
    fn signum(&self) -> LReal {
        if self.is_zero() {
            RInt(Zero::zero())
        } else if self.is_positive() {
            RInt(One::one())
        } else {
            RInt(-One::one::<BigInt>())
        }
    }

    #[inline]
    fn is_positive(&self) -> bool {
        meth!(is_positive)
    }

    #[inline]
    fn is_negative(&self) -> bool {
        meth!(is_negative)
    }
}

macro_rules! impl_round (
    ($op:ident) => (
        {
            match self {
                &RInt(ref x) => RInt(x.clone()),
                &RRat(ref x) => {
                    let rat = x.$op();
                    RInt(rat.denominator().clone())
                },
                &Rf64(ref x) => Rf64(x.$op()),
            }
        }
    )
)

impl Fractional for LReal {
    #[inline]
    fn recip(&self) -> LReal {
        match self {
            &RInt(ref x) => if *x == One::one::<BigInt>() || *x == -One::one::<BigInt>() {
                    self.clone()
                } else {
                    RRat(Rational::new(One::one(), x.clone()))
                },
            &RRat(ref x) => {
                let r = x.recip();
                if *r.numerator() == One::one() {
                    RInt(r.denominator().clone())
                } else {
                    RRat(r)
                }
            },
            &Rf64(x) => Rf64(x.recip()),
        }
    }
}

impl Round for LReal {
    #[inline]
    fn floor(&self) -> LReal { impl_round!(floor) }

    #[inline]
    fn ceil(&self) -> LReal { impl_round!(ceil) }

    #[inline]
    fn round(&self) -> LReal { impl_round!(round) }

    #[inline]
    fn trunc(&self) -> LReal { impl_round!(trunc) }

    #[inline]
    fn fract(&self) -> LReal {
        match self {
            &RInt(_) => RInt(Zero::zero()),
            &RRat(ref x) => RRat(x.fract()),
            &Rf64(ref x) => Rf64(x.fract()),
        }
    }
}

macro_rules! impl_f64 (
    ($op:ident) => (
        Rf64(self.to_f64().$op())
    )
)

macro_rules! impl_f64_binop (
    ($op:ident) => (
        Rf64(self.to_f64().$op(&other.to_f64()))
    )
)

impl Exponential for LReal {
    #[inline]
    fn exp(&self) -> LReal { impl_f64!(exp) }
    #[inline]
    fn exp2(&self) -> LReal { impl_f64!(exp2) }
    #[inline]
    fn ln(&self) -> LReal { impl_f64!(ln) }
    #[inline]
    fn log(&self, other: &LReal) -> LReal { impl_f64_binop!(log) }
    #[inline]
    fn log2(&self) -> LReal { impl_f64!(log2) }
    #[inline]
    fn log10(&self) -> LReal { impl_f64!(log10) }
}

impl Algebraic for LReal {
    #[inline]
    fn pow(&self, other: &LReal) -> LReal { impl_f64_binop!(pow) }
    #[inline]
    fn sqrt(&self) -> LReal { impl_f64!(sqrt) }
    #[inline]
    fn rsqrt(&self) -> LReal { impl_f64!(rsqrt) }
    #[inline]
    fn cbrt(&self) -> LReal { impl_f64!(cbrt) }
    #[inline]
    fn hypot(&self, other: &LReal) -> LReal { impl_f64_binop!(hypot) }
}

impl Trigonometric for LReal {
    #[inline]
    fn sin(&self) -> LReal { impl_f64!(sin) }
    #[inline]
    fn cos(&self) -> LReal { impl_f64!(cos) }
    #[inline]
    fn tan(&self) -> LReal { impl_f64!(tan) }
    #[inline]
    fn asin(&self) -> LReal { impl_f64!(asin) }
    #[inline]
    fn acos(&self) -> LReal { impl_f64!(acos) }
    #[inline]
    fn atan(&self) -> LReal { impl_f64!(atan) }
    #[inline]
    fn atan2(&self, other: &LReal) -> LReal { impl_f64_binop!(atan2) }
    #[inline]
    fn sin_cos(&self) -> (LReal, LReal) {
        (self.sin(), self.cos())
    }
}

impl Hyperbolic for LReal {
    #[inline]
    fn sinh(&self) -> LReal { impl_f64!(sinh) }
    #[inline]
    fn cosh(&self) -> LReal { impl_f64!(cosh) }
    #[inline]
    fn tanh(&self) -> LReal { impl_f64!(tanh) }
    #[inline]
    fn asinh(&self) -> LReal { impl_f64!(asinh) }
    #[inline]
    fn acosh(&self) -> LReal { impl_f64!(acosh) }
    #[inline]
    fn atanh(&self) -> LReal { impl_f64!(atanh) }
}

#[test]
fn from_rational_test() {
    let x = LReal::from_rational( Rational::new_int(3, 1) );
    let y = RInt(BigInt::from_uint(3));
    assert_eq!(x, y);
}
