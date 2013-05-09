use core::num::{One, Zero};
use core::cmp::{Eq, Ord};
use core::ops::{Neg, Add, Sub, Mul, Div};

pub struct Rational {
    priv d: int,
    priv n: int,
}

priv fn gcd(mut u: int, mut v: int) -> int {
    let mut g = 1;
    if(u == 0) {
        v
    } else if(v == 0) {
        u
    } else {
        while(u%2 == 0 && v%2 == 0) {
            u /= 2;
            v /= 2;
            g *= 2;
        }

        while(u%2 == 0) { u /= 2; }
        while(v%2 == 0) { v /= 2; }

        if(u > v) {
            g * gcd((u - v)/2, v)
        } else {
            g * gcd((v - u)/2, u)
        }
    }
}

pub impl Rational {
    fn new(d: int, n: int) -> Rational {
        if(n == 0) {
            fail!(~"divide by zero");
        }

        let g = gcd(d, n);

        if(n < 0) {
            Rational { d: -d/g, n: -n/g }
        } else {
            Rational { d: d/g, n: n/g }
        }
    }

    fn denominator(&self) -> int {
        self.d
    }

    fn numerator(&self) -> int {
        self.n
    }

    fn to_f64(&self) -> f64 {
        (self.d as f64) / (self.n as f64)
    }

    fn is_zero(&self) -> bool {
        self.d == 0
    }

    fn is_nonnegative(&self) -> bool {
        int::is_nonnegative(self.d)
    }

    fn is_negative(&self) -> bool {
        int::is_negative(self.d)
    }

    fn is_nonpositive(&self) -> bool {
        int::is_nonpositive(self.d)
    }

    fn is_positive(&self) -> bool {
        int::is_positive(self.d)
    }
}

impl One for Rational {
    fn one() -> Rational {
        Rational {
            d: 1,
            n: 1,
        }
    }
}

impl Zero for Rational {
    fn zero() -> Rational {
        Rational {
            d: 0,
            n: 1,
        }
    }
}

impl Neg<Rational> for Rational {
    fn neg(&self) -> Rational {
        Rational {
            d: -self.d,
            n: self.n,
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
        let g = gcd(self.n, rhs.n);
        let ln = self.n / g;
        let rn = rhs.n / g;
        Rational {
            d: self.d * rn + rhs.d * ln,
            n: self.n * rn,
        }
    }
}

impl Sub<Rational, Rational> for Rational {
    fn sub(&self, rhs: &Rational) -> Rational {
        let g = gcd(self.n, rhs.n);
        let ln = self.n / g;
        let rn = rhs.n / g;
        Rational {
            d: self.d * rn - rhs.d * ln,
            n: self.n * rn,
        }
    }
}

impl Mul<Rational, Rational> for Rational {
    fn mul(&self, rhs: &Rational) -> Rational {
        let d = self.d * rhs.d;
        let n = self.n * rhs.n;
        let g = gcd(d, n);
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
        let g = gcd(d, n);
        Rational {
            d: d / g,
            n: n / g,
        }
    }
}

impl to_str::ToStr for Rational {
    fn to_str(&self) -> ~str {
        if(self.n == 1) {
            self.d.to_str()
        } else {
            self.d.to_str() + "/" + self.n.to_str()
        }
    }
}
