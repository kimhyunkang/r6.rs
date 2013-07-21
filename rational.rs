use std::num::{One, Zero};

pub struct Rational {
    priv d: int,
    priv n: int,
}

priv fn gcd(mut u: uint, mut v: uint) -> uint {
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

priv fn abs(x: int) -> uint {
    if x < 0 {
        -x as uint
    } else {
        x as uint
    }
}

impl Rational {
    pub fn new(d: int, n: int) -> Rational {
        if(n == 0) {
            fail!(~"divide by zero");
        }

        let g = gcd(abs(d), abs(n)) as int;

        if(n < 0) {
            Rational { d: -d/g, n: -n/g }
        } else {
            Rational { d: d/g, n: n/g }
        }
    }

    pub fn denominator(&self) -> int {
        self.d
    }

    pub fn numerator(&self) -> int {
        self.n
    }

    pub fn to_f64(&self) -> f64 {
        (self.d as f64) / (self.n as f64)
    }

    pub fn is_zero(&self) -> bool {
        self.d == 0
    }

    pub fn is_nonnegative(&self) -> bool {
        self.d >= 0
    }

    pub fn is_negative(&self) -> bool {
        self.d < 0
    }

    pub fn is_nonpositive(&self) -> bool {
        self.d <= 0
    }

    pub fn is_positive(&self) -> bool {
        self.d > 0
    }
}

impl Round for Rational {
    fn floor(&self) -> Rational {
        Rational {
            d: self.d.div_floor(&self.n),
            n: 1,
        }
    }

    fn ceil(&self) -> Rational {
        Rational {
            d: -((-self.d).div_floor(&self.n)),
            n: 1,
        }
    }

    fn round(&self) -> Rational {
        Rational {
            d: ((self.d as f64) / (self.n as f64)).round() as int,
            n: 1,
        }
    }

    fn trunc(&self) -> Rational {
        Rational {
            d: ((self.d as f64) / (self.n as f64)).trunc() as int,
            n: 1,
        }
    }

    fn fract(&self) -> Rational {
        Rational {
            d: ((self.d as f64) / (self.n as f64)).fract() as int,
            n: 1,
        }
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

    fn is_zero(&self) -> bool {
        self.d == 0
    }
}

impl Clone for Rational {
    fn clone(&self) -> Rational {
        Rational {
            d: self.d,
            n: self.n,
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
        let g = gcd(self.n as uint, rhs.n as uint) as int;
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
        let g = gcd(self.n as uint, rhs.n as uint) as int;
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
        let g = gcd(abs(d), abs(n)) as int;
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
        let g = gcd(abs(d), abs(n)) as int;
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
        self - rhs * Rational { d: int_q, n: 1 }
    }
}

impl Num for Rational;

impl ToStr for Rational {
    fn to_str(&self) -> ~str {
        if(self.n == 1) {
            self.d.to_str()
        } else {
            self.d.to_str() + "/" + self.n.to_str()
        }
    }
}

#[test]
fn gcd_test() {
    assert_eq!(gcd(-2, 1), 1);
}
