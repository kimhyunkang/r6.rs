use core::num::{One, Zero};
use core::num::One::one;
use core::num::Zero::zero;
use core::cmp::{Eq};
use core::ops::{Neg, Add, Sub, Mul, Div};
use core::to_str::ToStr;

use rational::Rational;

#[deriving(Eq)]
pub enum LNumeric {
    NExact(Rational, Rational),
    NInexact(f64, f64),
}

pub impl LNumeric {
    fn is_inexact(&self) -> bool {
        match *self {
            NExact(_,_) => false,
            NInexact(_,_) => true,
        }
    }

    fn is_exact(&self) -> bool {
        match *self {
            NExact(_,_) => true,
            NInexact(_,_) => false,
        }
    }
}

pub fn to_str(&n: &LNumeric) -> ~str {
    match n {
        NExact(re, im) => {
            if im.is_zero() {
                re.to_str()
            } else if re.is_zero() {
                im.to_str() + "i"
            } else if im.is_negative() {
                re.to_str() + im.to_str() + "i"
            } else {
                re.to_str() + "+" + im.to_str() + "i"
            }
        },
        NInexact(re, im) => {
            if im == 0f64 {
                re.to_str()
            } else if re == 0f64 {
                im.to_str() + "i"
            } else if im < 0f64 {
                re.to_str() + im.to_str() + "i"
            } else {
                re.to_str() + "+" + im.to_str() + "i"
            }
        },
    }
}

impl ToStr for LNumeric {
    fn to_str(&self) -> ~str {
        to_str(self)
    }
}

impl One for LNumeric {
    fn one() -> LNumeric {
        NExact(one(), zero())
    }
}

impl Zero for LNumeric {
    fn zero() -> LNumeric {
        NExact(zero(), zero())
    }
}

fn to_inexact(&n: &LNumeric) -> (f64, f64) {
    match n {
        NExact(re, im) => (re.to_f64(), im.to_f64()),
        NInexact(re, im) => (re, im),
    }
}

pub fn neg(&n: &LNumeric) -> LNumeric {
    match n {
        NExact(re, im) => NExact(-re, -im),
        NInexact(re, im) => NInexact(-re, -im),
    }
}

impl Neg<LNumeric> for LNumeric {
    fn neg(&self) -> LNumeric {
        neg(self)
    }
}

pub fn add(&lhs: &LNumeric, &rhs: &LNumeric) -> LNumeric {
    match (lhs, rhs) {
        (NExact(re0, im0), NExact(re1, im1)) => NExact(re0 + re1, im0 + im1),
        _ => {
            let (re0, im0) = to_inexact(&lhs);
            let (re1, im1) = to_inexact(&rhs);
            NInexact(re0 + re1, im0 + im1)
        },
    }
}

impl Add<LNumeric, LNumeric> for LNumeric {
    fn add(&self, rhs: &LNumeric) -> LNumeric {
        add(self, rhs)
    }
}

pub fn sub(&lhs: &LNumeric, &rhs: &LNumeric) -> LNumeric {
    match (lhs, rhs) {
        (NExact(re0, im0), NExact(re1, im1)) => NExact(re0 - re1, im0 - im1),
        _ => {
            let (re0, im0) = to_inexact(&lhs);
            let (re1, im1) = to_inexact(&rhs);
            NInexact(re0 - re1, im0 - im1)
        },
    }
}

impl Sub<LNumeric, LNumeric> for LNumeric {
    fn sub(&self, rhs: &LNumeric) -> LNumeric {
        sub(self, rhs)
    }
}

pub fn mul(&lhs: &LNumeric, &rhs: &LNumeric) -> LNumeric {
    match (lhs, rhs) {
        (NExact(re0, im0), NExact(re1, im1)) => {
            let re = re0 * re1 - im0 * im1;
            let im = im0 * re1 + re0 * im1;
            NExact(re, im)
        }
        _ => {
            let (re0, im0) = to_inexact(&lhs);
            let (re1, im1) = to_inexact(&rhs);
            let re = re0 * re1 - im0 * im1;
            let im = im0 * re1 + re0 * im1;
            NInexact(re, im)
        },
    }
}

impl Mul<LNumeric, LNumeric> for LNumeric {
    fn mul(&self, rhs: &LNumeric) -> LNumeric {
        mul(self, rhs)
    }
}

pub fn div(&lhs: &LNumeric, &rhs: &LNumeric) -> LNumeric {
    match (lhs, rhs) {
        (NExact(re0, im0), NExact(re1, im1)) => {
            let n = re1 * re1 + im1 * im1;
            let re = (re0 * re1 + im0 * im1) / n;
            let im = (im0 * re1 - re0 * im1) / n;
            NExact(re, im)
        }
        _ => {
            let (re0, im0) = to_inexact(&lhs);
            let (re1, im1) = to_inexact(&rhs);
            let n = re1 * re1 + im1 * im1;
            let re = (re0 * re1 + im0 * im1) / n;
            let im = (im0 * re1 - re0 * im1) / n;
            NInexact(re, im)
        },
    }
}

impl Div<LNumeric, LNumeric> for LNumeric {
    fn div(&self, rhs: &LNumeric) -> LNumeric {
        div(self, rhs)
    }
}
