use std::num::{Zero, One, ToStrRadix};
use std::fmt;
use num::bigint::BigInt;
use num::rational::{Ratio, BigRational};

use bigint_helper::{rational_from_float, bigint_to_float, rational_to_f64};

#[deriving(Clone)]
pub enum LReal {
    RInt(BigInt),
    RRat(BigRational),
    Rf64(f64),
}

pub fn coerce_real<T>(lhs: &LReal, rhs: &LReal,
                    int_op: |&BigInt, &BigInt| -> T,
                    rat_op: |&BigRational, &BigRational| -> T,
                    f64_op: |&f64, &f64| -> T) -> T
{
    match (lhs, rhs) {
        (&RInt(ref x), &RInt(ref y)) => int_op(x, y),
        (&RInt(ref x), &RRat(ref y)) => rat_op(&Ratio::new(x.clone(), One::one()), y),
        (&RInt(ref x), &Rf64(ref y)) => f64_op(&bigint_to_float(x), y),
        (&RRat(ref x), &RInt(ref y)) => rat_op(x, &Ratio::new(y.clone(), One::one())),
        (&RRat(ref x), &RRat(ref y)) => rat_op(x, y),
        (&RRat(ref x), &Rf64(ref y)) => f64_op(&rational_to_f64(x), y),
        (&Rf64(ref x), &RInt(ref y)) => f64_op(x, &bigint_to_float(y)),
        (&Rf64(ref x), &RRat(ref y)) => f64_op(x, &rational_to_f64(y)),
        (&Rf64(ref x), &Rf64(ref y)) => f64_op(x, y),
    }
}

pub fn coerce_barrier<T>(lhs: &LReal, rhs: &LReal,
                    rat_op: |&BigRational, &BigRational| -> T,
                    f64_op: |&f64, &f64| -> T) -> Option<T>
{
    match (lhs, rhs) {
        (&RInt(ref x), &RInt(ref y)) => {
            let rx = Ratio::new(x.clone(), One::one());
            let ry = Ratio::new(y.clone(), One::one());
            Some(rat_op(&rx, &ry))
        },
        (&RInt(ref x), &RRat(ref y)) => Some(rat_op(&Ratio::new(x.clone(), One::one()), y)),
        (&RRat(ref x), &RInt(ref y)) => Some(rat_op(x, &Ratio::new(y.clone(), One::one()))),
        (&RRat(ref x), &RRat(ref y)) => Some(rat_op(x, y)),
        (&Rf64(ref x), &Rf64(ref y)) => Some(f64_op(x, y)),
        _ => None,
    }
}

impl LReal {
    pub fn to_f64(&self) -> f64 {
        match self {
            &RInt(ref x) => bigint_to_float(x),
            &RRat(ref x) => rational_to_f64(x),
            &Rf64(x) => x,
        }
    }

    pub fn to_rational(&self) -> BigRational {
        match self {
            &RInt(ref x) => Ratio::new(x.clone(), One::one()),
            &RRat(ref x) => x.clone(),
            &Rf64(ref x) => rational_from_float(*x),
        }
    }

    pub fn to_exact(&self) -> LReal {
        match self {
            &RInt(_) => self.clone(),
            &RRat(_) => self.clone(),
            &Rf64(x) => {
                let rat = rational_from_float(x);
                if *rat.denom() == One::one() {
                    RInt(rat.numer().clone())
                } else {
                    RRat(rat)
                }
            }
        }
    }

    pub fn from_rational(rat: BigRational) -> LReal {
        if *rat.denom() == One::one() {
            RInt(rat.numer().clone())
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

impl PartialEq for LReal {
    fn eq(&self, other: &LReal) -> bool {
        match coerce_barrier(self, other, |x, y| { x == y }, |x, y| { x == y }) {
            Some(true) => true,
            _ => false,
        }
    }

    fn ne(&self, other: &LReal) -> bool {
        match coerce_barrier(self, other, |x, y| { x != y }, |x, y| { x != y }) {
            Some(false) => false,
            _ => true,
        }
    }
}

impl PartialOrd for LReal {
    fn partial_cmp(&self, other: &LReal) -> Option<Ordering> {
        coerce_real(self, other,
                    |x, y| { x.partial_cmp(y) },
                    |x, y| { x.partial_cmp(y) },
                    |x, y| { x.partial_cmp(y) })
    }
}

macro_rules! impl_op(
    ($T:ty, $op:ident) => (
        impl $T for LReal {
            fn $op(&self, other: &LReal) -> LReal {
                coerce_real(self, other,
                            |x, y| { RInt(x.$op(y)) },
                            |x, y| {
                                let rat = x.$op(y);
                                if *rat.denom() == One::one() {
                                    RInt(rat.numer().clone())
                                } else {
                                    RRat(rat)
                                }
                            },
                            |x, y| { Rf64(x.$op(y)) })
            }
        }
    )
)

impl_op!(Add<LReal, LReal>, add)
impl_op!(Sub<LReal, LReal>, sub)
impl_op!(Mul<LReal, LReal>, mul)
impl_op!(Div<LReal, LReal>, div)
impl_op!(Rem<LReal, LReal>, rem)

impl Num for LReal {}

pub fn f64_fmt(x: f64, f: &mut fmt::Formatter) -> fmt::Result {
    if x.fract().is_zero() {
        write!(f, "{}.0", x)
    } else {
        write!(f, "{}", x)
    }
}

#[allow(deprecated)]
pub fn f64_to_str_radix(f: f64, radix: uint) -> String {
    if f.fract().is_zero() {
        f.to_str_radix(radix) + ".0"
    } else {
        f.to_str_radix(radix)
    }
}

impl fmt::Show for LReal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &RInt(ref x) => write!(f, "{}", x),
            &RRat(ref x) => write!(f, "{}", x),
            &Rf64(ref x) => f64_fmt(*x, f)
        }
    }
}

#[allow(deprecated)]
impl ToStrRadix for LReal {
    fn to_str_radix(&self, radix: uint) -> String {
        match self {
            &RInt(ref x) => x.to_str_radix(radix),
            &RRat(ref x) => x.to_str_radix(radix),
            &Rf64(ref x) => f64_to_str_radix(*x, radix),
        }
    }
}

impl Neg<LReal> for LReal {
    fn neg(&self) -> LReal { 
        match self {
            &RInt(ref x) => RInt(x.neg()),
            &RRat(ref x) => RRat(x.neg()),
            &Rf64(ref x) => Rf64(x.neg()),
        }
    }
}

impl Zero for LReal {
    fn zero() -> LReal { RInt(Zero::zero()) }

    /// Returns true if the number is equal to either `0.0` or `-0.0`
    fn is_zero(&self) -> bool {
        match self {
            &RInt(ref x) => x.is_zero(),
            &RRat(ref x) => x.is_zero(),
            &Rf64(ref x) => x.is_zero(),
        }
    }
}

impl One for LReal {
    fn one() -> LReal { RInt(One::one()) }
}

impl Signed for LReal {
    fn abs(&self) -> LReal {
        if self.is_negative() {
            self.neg()
        } else {
            self.clone()
        }
    }

    fn abs_sub(&self, other: &LReal) -> LReal {
        if self.gt(other) {
            self.sub(other)
        } else {
            other.sub(self)
        }
    }

    fn signum(&self) -> LReal {
        if self.is_zero() {
            RInt(Zero::zero())
        } else if self.is_positive() {
            RInt(One::one())
        } else {
            RInt(FromPrimitive::from_int(1).unwrap())
        }
    }

    fn is_positive(&self) -> bool {
        match self {
            &RInt(ref x) => x.is_positive(),
            &RRat(ref x) => x.is_positive(),
            &Rf64(ref x) => x.is_positive(),
        }
    }

    fn is_negative(&self) -> bool {
        match self {
            &RInt(ref x) => x.is_negative(),
            &RRat(ref x) => x.is_negative(),
            &Rf64(ref x) => x.is_negative(),
        }
    }
}

#[test]
fn from_rational_test() {
    let _3: BigInt = FromPrimitive::from_int(3).unwrap();
    let _1: BigInt = FromPrimitive::from_int(1).unwrap();
    let x = LReal::from_rational( Ratio::new(_3, _1) );
    let y = RInt(FromPrimitive::from_uint(3).unwrap());
    assert_eq!(x, y);
}
