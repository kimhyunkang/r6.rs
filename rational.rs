use std::num::{One, Zero, IntConvertible, ToStrRadix};
use bigint_helper::*;
use extra::bigint::BigInt;

pub struct Rational {
    priv d: BigInt,
    priv n: BigInt,
}

impl Rational {
    pub fn new(d: BigInt, n: BigInt) -> Rational {
        if n.is_zero() {
            fail!(~"divide by zero");
        }

        let g = d.gcd(&n);

        if n.is_negative() {
            Rational { d: -d/g, n: -n/g }
        } else {
            Rational { d: d/g, n: n/g }
        }
    }

    pub fn from_bigint(d: BigInt) -> Rational {
        Rational { d: d, n: One::one() }
    }

    pub fn new_int(new_d: int, new_n: int) -> Rational {
        let d = IntConvertible::from_int::<BigInt>(new_d);
        let n = IntConvertible::from_int::<BigInt>(new_n);

        Rational::new(d, n)
    }

    pub fn denominator<'r>(&'r self) -> &'r BigInt {
        &self.d
    }

    pub fn numerator<'r>(&'r self) -> &'r BigInt {
        &self.n
    }

    pub fn to_float<T:Float + NumCast + Zero>(&self) -> T {
        let d:T = bigint_to_float(&self.d);
        let n:T = bigint_to_float(&self.n);
        d / n
    }

    pub fn from_float<T:Float>(f: T) -> Rational {
        match float_disassemble(f) {
            None => fail!(~"f is not normal float"),
            Some((mantissa, exp)) => if exp < 0 {
                    let n = One::one::<BigInt>() << (-exp as uint);
                    Rational::new(mantissa, n)
                } else {
                    let d = mantissa << (exp as uint);
                    Rational::new(d, One::one())
                }
        }
    }
}

impl Round for Rational {
    fn floor(&self) -> Rational {
        Rational {
            d: self.d.div_floor(&self.n),
            n: One::one(),
        }
    }

    fn ceil(&self) -> Rational {
        Rational {
            d: -((-self.d).div_floor(&self.n)),
            n: One::one(),
        }
    }

    fn round(&self) -> Rational {
        Rational {
            d: IntConvertible::from_int::<BigInt>(self.to_float::<f64>().round() as int),
            n: One::one(),
        }
    }

    fn trunc(&self) -> Rational {
        Rational {
            d: self.d / self.n,
            n: One::one(),
        }
    }

    fn fract(&self) -> Rational {
        Rational {
            d: self.d % self.n,
            n: self.n.clone(),
        }
    }
}

impl One for Rational {
    fn one() -> Rational {
        Rational {
            d: One::one(),
            n: One::one(),
        }
    }
}

impl Zero for Rational {
    fn zero() -> Rational {
        Rational {
            d: Zero::zero(),
            n: One::one(),
        }
    }

    fn is_zero(&self) -> bool {
        self.d.is_zero()
    }
}

impl Signed for Rational {
    fn abs(&self) -> Rational {
        Rational { d: self.d.abs(), n: self.n.clone() }
    }

    fn abs_sub(&self, other: &Rational) -> Rational {
        if *self <= *other { 
            Zero::zero()
        } else {
            *self - *other
        }
    }

    fn signum(&self) -> Rational {
        Rational { d: self.d.signum(), n: One::one() }
    }

    fn is_positive(&self) -> bool {
        self.d.is_positive()
    }

    fn is_negative(&self) -> bool {
        self.d.is_negative()
    }
}

impl Clone for Rational {
    fn clone(&self) -> Rational {
        Rational {
            d: self.d.clone(),
            n: self.n.clone(),
        }
    }
}

impl Neg<Rational> for Rational {
    fn neg(&self) -> Rational {
        Rational {
            d: -self.d,
            n: self.n.clone(),
        }
    }
}

impl Eq for Rational {
    fn eq(&self, other: &Rational) -> bool {
        self.d == other.d && self.n == other.n
    }
    fn ne(&self, other: &Rational) -> bool {
        self.d != other.d || self.n != other.n
    }
}

impl Ord for Rational {
    fn lt(&self, other: &Rational) -> bool {
        self.d * other.n < other.d * self.n
    }
    fn le(&self, other: &Rational) -> bool {
        self.d * other.n <= other.d * self.n
    }
    fn gt(&self, other: &Rational) -> bool {
        self.d * other.n > other.d * self.n
    }
    fn ge(&self, other: &Rational) -> bool {
        self.d * other.n >= other.d * self.n
    }
}

impl Add<Rational, Rational> for Rational {
    fn add(&self, rhs: &Rational) -> Rational {
        let g = self.n.gcd(&rhs.n);
        let ln = self.n / g;
        let rn = rhs.n / g;
        Rational::new(self.d * rn + rhs.d * ln, self.n * rn)
    }
}

impl Sub<Rational, Rational> for Rational {
    fn sub(&self, rhs: &Rational) -> Rational {
        let g = self.n.gcd(&rhs.n);
        let ln = self.n / g;
        let rn = rhs.n / g;
        Rational::new(self.d * rn - rhs.d * ln, self.n * rn)
    }
}

impl Mul<Rational, Rational> for Rational {
    fn mul(&self, rhs: &Rational) -> Rational {
        let d = self.d * rhs.d;
        let n = self.n * rhs.n;
        let g = d.gcd(&n);
        Rational {
            d: d / g,
            n: n / g,
        }
    }
}

impl Div<Rational, Rational> for Rational {
    fn div(&self, rhs: &Rational) -> Rational {
        let d = self.d * rhs.n;
        let n = self.n * rhs.d;
        let g = d.gcd(&n);
        Rational {
            d: d / g,
            n: n / g,
        }
    }
}

impl Rem<Rational, Rational> for Rational {
    fn rem(&self, rhs: &Rational) -> Rational {
        let q = self / *rhs;
        let int_q = q.d / q.n;
        self - rhs * Rational { d: int_q, n: One::one() }
    }
}

impl Fractional for Rational {
    fn recip(&self) -> Rational {
        Rational::new(self.n.clone(), self.d.clone())
    }
}

impl Num for Rational;

impl ToStr for Rational {
    fn to_str(&self) -> ~str {
        self.to_str_radix(10)
    }
}

impl ToStrRadix for Rational {
    fn to_str_radix(&self, radix: uint) -> ~str {
        if self.n == One::one() {
            self.d.to_str_radix(radix)
        } else {
            self.d.to_str_radix(radix) + "/" + self.n.to_str_radix(radix)
        }
    }
}
