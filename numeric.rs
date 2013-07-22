use std::num::{One, Zero};
use extra::complex::Cmplx;

use rational::Rational;

#[deriving(Eq)]
pub enum LNumeric {
    NExact(Cmplx<Rational>),
    NInexact(Cmplx<f64>)
}

impl LNumeric {
    pub fn is_inexact(&self) -> bool {
        match *self {
            NExact(_) => false,
            NInexact(_) => true,
        }
    }

    pub fn is_exact(&self) -> bool {
        match *self {
            NExact(_) => true,
            NInexact(_) => false,
        }
    }

    pub fn is_real(&self) -> bool {
        match *self {
            NExact(c) => c.im.is_zero(),
            NInexact(c) => c.im.is_zero(),
        }
    }
}

pub fn to_str(&n: &LNumeric) -> ~str {
    match n {
        NExact(Cmplx{ re: re, im: im }) => {
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
        NInexact(Cmplx{ re: re, im: im }) => {
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
        let delta2 = to_inexact(&delta).norm();
        delta2 < *approx_epsilon
    }
}

impl ToStr for LNumeric {
    fn to_str(&self) -> ~str {
        to_str(self)
    }
}

impl One for LNumeric {
    fn one() -> LNumeric {
        NExact(Cmplx { re: One::one(), im: Zero::zero() })
    }
}

impl Zero for LNumeric {
    fn zero() -> LNumeric {
        NExact(Zero::zero())
    }

    fn is_zero(&self) -> bool {
        match *self {
            NExact(cmplx) => cmplx.is_zero(),
            NInexact(cmplx) => cmplx.is_zero(),
        }
    }
}

pub fn to_inexact(&n: &LNumeric) -> Cmplx<f64> {
    match n {
        NExact(Cmplx { re: re, im: im }) => Cmplx {re: re.to_f64(), im: im.to_f64()},
        NInexact(cmplx) => cmplx
    }
}

pub fn neg(&n: &LNumeric) -> LNumeric {
    match n {
        NExact(cmplx) => NExact(-cmplx),
        NInexact(cmplx) => NInexact(-cmplx),
    }
}

impl Neg<LNumeric> for LNumeric {
    fn neg(&self) -> LNumeric {
        neg(self)
    }
}

pub fn add(&lhs: &LNumeric, &rhs: &LNumeric) -> LNumeric {
    match (lhs, rhs) {
        (NExact(cmplx0), NExact(cmplx1)) => NExact(cmplx0 + cmplx1),
        _ => {
            let cmplx0 = to_inexact(&lhs);
            let cmplx1 = to_inexact(&rhs);
            NInexact(cmplx0 + cmplx1)
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
        (NExact(cmplx0), NExact(cmplx1)) => NExact(cmplx0 - cmplx1),
        _ => {
            let cmplx0 = to_inexact(&lhs);
            let cmplx1 = to_inexact(&rhs);
            NInexact(cmplx0 - cmplx1)
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
        (NExact(cmplx0), NExact(cmplx1)) => NExact(cmplx0 * cmplx1),
        _ => {
            let cmplx0 = to_inexact(&lhs);
            let cmplx1 = to_inexact(&rhs);
            NInexact(cmplx0 * cmplx1)
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
        (NExact(cmplx0), NExact(cmplx1)) => NExact(cmplx0 / cmplx1),
        _ => {
            let cmplx0 = to_inexact(&lhs);
            let cmplx1 = to_inexact(&rhs);
            NInexact(cmplx0 / cmplx1)
        },
    }
}

impl Div<LNumeric, LNumeric> for LNumeric {
    fn div(&self, rhs: &LNumeric) -> LNumeric {
        div(self, rhs)
    }
}

impl Fractional for LNumeric {
    fn recip(&self) -> LNumeric {
        match *self {
            NExact( c ) => NExact(One::one::<Cmplx<Rational>>() / c),
            NInexact( c ) => NInexact(One::one::<Cmplx<f64>>() / c),
        }
    }
}

pub fn cmplx_exp<T: Clone + Exponential + Trigonometric + Num>(c: &Cmplx<T>) -> Cmplx<T> {
    // e^(a+bi) = e^a * (cos b + i sin b)
    let pow = c.re.exp();
    Cmplx { re: c.im.cos() * pow, im: c.im.sin() * pow }
}

impl Exponential for LNumeric {
    fn exp(&self) -> LNumeric {
        NInexact( cmplx_exp( &to_inexact(self)) )
    }

    fn exp2(&self) -> LNumeric {
        let x = to_inexact(self);
        let l2 = Real::ln_2();
        NInexact( cmplx_exp( &Cmplx{ re: x.re * l2, im: x.im * l2 }) )
    }

    fn ln(&self) -> LNumeric {
        let (norm, arg) = to_inexact(self).to_polar();
        NInexact( Cmplx{ re: norm.ln(), im: arg } )
    }

    fn log(&self, base: &LNumeric) -> LNumeric {
        self.ln() / base.ln()
    }

    fn log2(&self) -> LNumeric {
        let (norm, arg) = to_inexact(self).to_polar();
        let l2 = Real::ln_2();
        NInexact( Cmplx{ re: norm.ln() / l2, im: arg / l2 } )
    }

    fn log10(&self) -> LNumeric {
        let (norm, arg) = to_inexact(self).to_polar();
        let l10 = Real::ln_10();
        NInexact( Cmplx{ re: norm.ln() / l10, im: arg / l10 } )
    }
}

impl Algebraic for LNumeric {
    fn pow(&self, n: &LNumeric) -> LNumeric {
        // x^n = e^(n * ln x)
        (self.ln() * (*n)).exp()
    }

    fn sqrt(&self) -> LNumeric {
        // sqrt(z) = sqrt(|z|) * { cos(arg z / 2) + i sin(arg z / 2) }
        let (norm, arg) = to_inexact(self).to_polar();
        let n = norm.sqrt();
        NInexact( Cmplx { re: (arg * 0.5).cos() * n, im: (arg * 0.5).sin() * n } )
    }

    fn rsqrt(&self) -> LNumeric {
        // 1/sqrt(z) = 1/sqrt(|z|) * 1/{ cos(arg z / 2) + i sin(arg z / 2) }
        //           = 1/sqrt(|z|) * { cos(arg z / 2) - i sin(arg z / 2)
        let (norm, arg) = to_inexact(self).to_polar();
        let n = norm.rsqrt();
        NInexact( Cmplx { re: (arg * 0.5).cos() * n, im: (arg * -0.5).sin() * n } )
    }

    fn cbrt(&self) -> LNumeric {
        // cbrt(z) = cbrt(|z|) * { cos(arg z / 3) + i sin(arg z / 3) }
        let (norm, arg) = to_inexact(self).to_polar();
        let n = norm.cbrt();
        NInexact( Cmplx { re: (arg / 3.0).cos() * n, im: (arg / 3.0).sin() * n } )
    }

    fn hypot(&self, rhs: &LNumeric) -> LNumeric {
        let x = *self;
        let y = *rhs;
        ((x * x) + (y * y)).sqrt()
    }
}

pub fn from_int(n: int) -> LNumeric {
    NExact( Cmplx{ re: Rational::new(n, 1), im: Zero::zero() } )
}

pub fn from_rational(re: Rational) -> LNumeric {
    NExact( Cmplx{ re: re, im: Zero::zero() } )
}

pub fn from_f64(re: f64) -> LNumeric {
    NInexact( Cmplx{ re: re, im: 0f64 } )
}

pub fn from_real(re: &LReal) -> LNumeric {
    match *re {
        NRational(x) => from_rational(x),
        NFloat(x) => from_f64(x),
    }
}

pub fn exact(re: Rational, im: Rational) -> LNumeric {
    NExact( Cmplx { re: re, im: im } )
}

pub fn inexact(re: f64, im: f64) -> LNumeric {
    NInexact( Cmplx { re: re, im: im } )
}

pub enum LReal {
    NRational(Rational),
    NFloat(f64)
}

priv fn coerce<T>(a: &LReal, b: &LReal,
                    op_r: &fn(&Rational, &Rational) -> T,
                    op_f: &fn(f64, f64) -> T) -> T
{
    match (a, b) {
        (&NRational(ref x), &NRational(ref y)) => op_r(x, y),
        (&NRational(ref x), &NFloat(y)) => op_f(x.to_f64(), y),
        (&NFloat(x), &NRational(ref y)) => op_f(x, y.to_f64()),
        (&NFloat(x), &NFloat(y)) => op_f(x, y),
    }
}

impl Eq for LReal {
    fn eq(&self, other: &LReal) -> bool {
        coerce(self, other, |x,y| {x == y}, |x,y| {x == y})
    }

    fn ne(&self, other: &LReal) -> bool {
        coerce(self, other, |x,y| {x != y}, |x,y| {x != y})
    }
}

impl Ord for LReal {
    fn lt(&self, other: &LReal) -> bool {
        coerce(self, other, |x,y| {x < y}, |x,y| {x < y})
    }

    fn le(&self, other: &LReal) -> bool {
        coerce(self, other, |x,y| {x <= y}, |x,y| {x <= y})
    }

    fn gt(&self, other: &LReal) -> bool {
        coerce(self, other, |x,y| {x > y}, |x,y| {x > y})
    }

    fn ge(&self, other: &LReal) -> bool {
        coerce(self, other, |x,y| {x >= y}, |x,y| {x >= y})
    }
}

impl Round for LReal {
    fn floor(&self) -> LReal {
        match *self {
            NRational( f ) => NRational( f.floor() ),
            NFloat( f ) => NFloat( f.floor() ),
        }
    }

    fn ceil(&self) -> LReal {
        match *self {
            NRational( f ) => NRational( f.ceil() ),
            NFloat( f ) => NFloat( f.ceil() ),
        }
    }

    fn round(&self) -> LReal {
        match *self {
            NRational( f ) => NRational( f.round() ),
            NFloat( f ) => NFloat( f.round() ),
        }
    }

    fn trunc(&self) -> LReal {
        match *self {
            NRational( f ) => NRational( f.trunc() ),
            NFloat( f ) => NFloat( f.trunc() ),
        }
    }

    fn fract(&self) -> LReal {
        match *self {
            NRational( f ) => NRational( f.fract() ),
            NFloat( f ) => NFloat( f.fract() ),
        }
    }
}

pub fn get_real(n: &LNumeric) -> Option<LReal>
{
    match *n {
        NExact( Cmplx{ re: re, im: im } ) => if im.is_zero() {
                Some( NRational ( re ) )
            } else {
                None
            },
        NInexact( Cmplx{ re: re, im: im } ) => if im.is_zero() {
                Some( NFloat ( re ) )
            } else {
                None
            },
    }
}

pub fn get_int(n: &LNumeric) -> Option<int>
{
    match *n {
        NExact( Cmplx{ re: re, im: im } ) => if im.is_zero() && re.numerator() == 1 {
                Some(re.denominator())
            } else {
                None
            },
        NInexact(_) => None,
    }
}

pub fn get_uint(n: &LNumeric) -> Option<uint>
{
    match *n {
        NExact( Cmplx{ re: re, im: im } ) =>
            if im.is_zero() && re.numerator() == 1 && re.denominator() >= 0 {
                Some(re.denominator() as uint)
            } else {
                None
            },
        NInexact(_) => None,
    }
}

pub fn modulo(l: int, r: int) -> int
{
    let q = l % r;
    if q < 0 {
        if r < q {
            q
        } else {
            q + r
        }
    } else if q > 0 {
        if q < r {
            q
        } else {
            q + r
        }
    } else {
        q
    }
}

#[test]
fn test_eq() {
    assert_eq!(NRational(Rational::new(2,1)) == NRational(Rational::new(2,1)), true);
    assert_eq!(NRational(Rational::new(2,1)) < NFloat(3.0), true);
    assert_eq!(NFloat(3.0) > NRational(Rational::new(2,1)), true);
}

#[test]
fn test_log_exp() {
    let x = NInexact( Cmplx { re: 1.0, im: 1.0 } );
    assert_approx_eq!(x.ln().exp(), x)
}
