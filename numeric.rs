use std::num::{One, Zero, ToStrRadix};
use extra::complex::Cmplx;
use extra::bigint::BigInt;

use rational::Rational;

#[deriving(Eq, Clone)]
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
            NExact(ref c) => c.im.is_zero(),
            NInexact(ref c) => c.im.is_zero(),
        }
    }

    pub fn to_inexact(&self) -> Cmplx<f64> {
        match *self {
            NExact(Cmplx { re: ref re, im: ref im }) => Cmplx {re: re.to_f64(), im: im.to_f64()},
            NInexact(cmplx) => cmplx
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
            if im.is_zero() {
                re.to_str()
            } else if re.is_zero() {
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
        let delta2 = delta.to_inexact().norm();
        delta2 < *approx_epsilon
    }
}

impl ToStr for LNumeric {
    fn to_str(&self) -> ~str {
        to_str(self)
    }
}

pub fn to_str_radix(n: &Cmplx<Rational>, radix: uint) -> ~str
{
    if n.im.is_zero() {
        n.re.to_str_radix(radix)
    } else if n.re.is_zero() {
        n.im.to_str_radix(radix) + "i"
    } else if n.im.is_negative() {
        n.re.to_str_radix(radix) + n.im.to_str_radix(radix) + "i"
    } else {
        n.re.to_str_radix(radix) + "+" + n.im.to_str_radix(radix) + "i"
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
            NExact(ref cmplx) => cmplx.is_zero(),
            NInexact(ref cmplx) => cmplx.is_zero(),
        }
    }
}

impl Neg<LNumeric> for LNumeric {
    fn neg(&self) -> LNumeric {
        match self {
            &NExact(ref cmplx) => NExact(-*cmplx),
            &NInexact(ref cmplx) => NInexact(-*cmplx),
        }
    }
}

impl Add<LNumeric, LNumeric> for LNumeric {
    fn add(&self, rhs: &LNumeric) -> LNumeric {
        match (self, rhs) {
            (&NExact(ref cmplx0), &NExact(ref cmplx1)) => NExact(*cmplx0 + *cmplx1),
            _ => {
                let cmplx0 = self.to_inexact();
                let cmplx1 = rhs.to_inexact();
                NInexact(cmplx0 + cmplx1)
            },
        }
    }
}

impl Sub<LNumeric, LNumeric> for LNumeric {
    fn sub(&self, rhs: &LNumeric) -> LNumeric {
        match (self, rhs) {
            (&NExact(ref cmplx0), &NExact(ref cmplx1)) => NExact(*cmplx0 - *cmplx1),
            _ => {
                let cmplx0 = self.to_inexact();
                let cmplx1 = rhs.to_inexact();
                NInexact(cmplx0 - cmplx1)
            },
        }
    }
}

impl Mul<LNumeric, LNumeric> for LNumeric {
    fn mul(&self, rhs: &LNumeric) -> LNumeric {
        match (self, rhs) {
            (&NExact(ref cmplx0), &NExact(ref cmplx1)) => NExact((*cmplx0) * (*cmplx1)),
            _ => {
                let cmplx0 = self.to_inexact();
                let cmplx1 = rhs.to_inexact();
                NInexact(cmplx0 * cmplx1)
            },
        }
    }
}

impl Div<LNumeric, LNumeric> for LNumeric {
    fn div(&self, rhs: &LNumeric) -> LNumeric {
        match (self, rhs) {
            (&NExact(ref cmplx0), &NExact(ref cmplx1)) => NExact(*cmplx0 / *cmplx1),
            _ => {
                let cmplx0 = self.to_inexact();
                let cmplx1 = rhs.to_inexact();
                NInexact(cmplx0 / cmplx1)
            },
        }
    }
}

impl Fractional for LNumeric {
    fn recip(&self) -> LNumeric {
        match self {
            &NExact( ref c ) => NExact(One::one::<Cmplx<Rational>>() / *c),
            &NInexact( ref c ) => NInexact(One::one::<Cmplx<f64>>() / *c),
        }
    }
}

pub fn cmplx_exp<T: Clone + Exponential + Trigonometric + Num>(c: &Cmplx<T>) -> Cmplx<T> {
    // e^(a+bi) = e^a * (cos b + i sin b)
    let pow = c.re.exp();
    Cmplx { re: c.im.cos() * pow, im: c.im.sin() * pow }
}

pub fn cmplx_ln<T: Clone + Exponential + Trigonometric + Algebraic + Num>(c: &Cmplx<T>)
    -> Cmplx<T>
{
    // e^(a+bi) = e^a * (cos b + i sin b)
    let (norm, arg) = c.to_polar();
    Cmplx{ re: norm.ln(), im: arg }
}

impl Exponential for LNumeric {
    fn exp(&self) -> LNumeric {
        NInexact( cmplx_exp( &self.to_inexact()) )
    }

    fn exp2(&self) -> LNumeric {
        let x = self.to_inexact();
        let l2 = Real::ln_2();
        NInexact( cmplx_exp( &Cmplx{ re: x.re * l2, im: x.im * l2 }) )
    }

    fn ln(&self) -> LNumeric {
        NInexact( cmplx_ln( &self.to_inexact() ) )
    }

    fn log(&self, base: &LNumeric) -> LNumeric {
        self.ln() / base.ln()
    }

    fn log2(&self) -> LNumeric {
        let (norm, arg) = self.to_inexact().to_polar();
        let l2 = Real::ln_2();
        NInexact( Cmplx{ re: norm.ln() / l2, im: arg / l2 } )
    }

    fn log10(&self) -> LNumeric {
        let (norm, arg) = self.to_inexact().to_polar();
        let l10 = Real::ln_10();
        NInexact( Cmplx{ re: norm.ln() / l10, im: arg / l10 } )
    }
}

pub fn cmplx_sqrt(c: &Cmplx<f64>) -> Cmplx<f64> {
    // sqrt(z) = sqrt(|z|) * { cos(arg z / 2) + i sin(arg z / 2) }
    let (norm, arg) = c.to_polar();
    let n = norm.sqrt();
    Cmplx { re: (arg * 0.5).cos() * n, im: (arg * 0.5).sin() * n }
}

impl Algebraic for LNumeric {
    fn pow(&self, n: &LNumeric) -> LNumeric {
        // x^n = e^(n * ln x)
        (self.ln() * (*n)).exp()
    }

    fn sqrt(&self) -> LNumeric {
        NInexact( cmplx_sqrt(&self.to_inexact()) )
    }

    fn rsqrt(&self) -> LNumeric {
        // 1/sqrt(z) = 1/sqrt(|z|) * 1/{ cos(arg z / 2) + i sin(arg z / 2) }
        //           = 1/sqrt(|z|) * { cos(arg z / 2) - i sin(arg z / 2)
        let (norm, arg) = self.to_inexact().to_polar();
        let n = norm.rsqrt();
        NInexact( Cmplx { re: (arg * 0.5).cos() * n, im: (arg * -0.5).sin() * n } )
    }

    fn cbrt(&self) -> LNumeric {
        // cbrt(z) = cbrt(|z|) * { cos(arg z / 3) + i sin(arg z / 3) }
        let (norm, arg) = self.to_inexact().to_polar();
        let n = norm.cbrt();
        NInexact( Cmplx { re: (arg / 3.0).cos() * n, im: (arg / 3.0).sin() * n } )
    }

    fn hypot(&self, rhs: &LNumeric) -> LNumeric {
        ((*self * *self) + (*rhs * *rhs)).sqrt()
    }
}

pub fn cmplx_sin<T: Trigonometric + Hyperbolic + Num>(c: &Cmplx<T>) -> Cmplx<T>
{
    // sin(x + iy) = sin x cosh y + i cos x sinh y
    Cmplx { re: c.re.sin() * c.im.cosh(), im: c.re.cos() * c.im.sinh() }
}

pub fn cmplx_cos<T: Trigonometric + Hyperbolic + Num>(c: &Cmplx<T>) -> Cmplx<T>
{
    // cos(x + iy) = cos x cosh y - i sin x sinh y
    Cmplx { re: c.re.cos() * c.im.cosh(), im: -(c.re.sin() * c.im.sinh()) }
}

impl Trigonometric for LNumeric {
    #[inline]
    fn sin(&self) -> LNumeric {
        NInexact( cmplx_sin(&self.to_inexact()) )
    }

    #[inline]
    fn cos(&self) -> LNumeric {
        NInexact( cmplx_cos(&self.to_inexact()) )
    }

    #[inline]
    fn tan(&self) -> LNumeric {
        let x = self.to_inexact();
        NInexact( cmplx_sin(&x) / cmplx_cos(&x) )
    }

    #[inline]
    fn asin(&self) -> LNumeric {
        // asin x = -i * ln( ix + sqrt(1 - x^2) )
        let x = self.to_inexact();
        let i = Cmplx { re: 0f64, im: 1f64 };
        let y = One::one::<Cmplx<f64>>() - x*x;
        let z = x * i + cmplx_sqrt(&y);
        NInexact(-i * cmplx_ln(&z))
    }

    #[inline]
    fn acos(&self) -> LNumeric {
        // acos x = -i * ln( x + i sqrt(1 - x^2) )
        let x = self.to_inexact();
        let i = Cmplx { re: 0f64, im: 1f64 };
        let y = One::one::<Cmplx<f64>>() - x*x;
        let z = x + cmplx_sqrt(&y) * i;
        NInexact(i * cmplx_ln(&z))
    }

    #[inline]
    fn atan(&self) -> LNumeric {
        // atan x = i/2 * ( ln(1 - ix) - ln(1 + ix) )
        let x = self.to_inexact();
        let i = Cmplx { re: 0f64, im: 1f64 };
        let ix = i * x;
        let ihalf = Cmplx { re: 0.5f64, im: 0.5f64 };
        let one:Cmplx<f64> = One::one();
        let y:Cmplx<f64> = cmplx_ln(&(one - ix)) - cmplx_ln(&(one + ix));
        NInexact(ihalf * y)
    }

    #[inline]
    fn atan2(&self, _: &LNumeric) -> LNumeric {
        fail!(~"atan2 not defined for complex numbers")
    }

    #[inline]
    fn sin_cos(&self) -> (LNumeric, LNumeric) {
        let x = self.to_inexact();
        (NInexact(cmplx_sin(&x)), NInexact(cmplx_cos(&x)))
    }
}

pub fn from_int(n: int) -> LNumeric {
    NExact( Cmplx{ re: Rational::new_int(n, 1), im: Zero::zero() } )
}

pub fn from_bigint(n: BigInt) -> LNumeric {
    NExact( Cmplx{ re: Rational::new(n, One::one()), im: Zero::zero() } )
}

pub fn from_rational(re: &Rational) -> LNumeric {
    NExact( Cmplx{ re: re.clone(), im: Zero::zero() } )
}

pub fn from_f64(re: f64) -> LNumeric {
    NInexact( Cmplx{ re: re, im: 0f64 } )
}

pub fn from_real(re: &LReal) -> LNumeric {
    match re {
        &NRational(ref x) => from_rational(x),
        &NFloat(x) => from_f64(x),
    }
}

pub fn exact(re: Rational, im: Rational) -> LNumeric {
    NExact( Cmplx { re: re, im: im } )
}

pub fn inexact(re: f64, im: f64) -> LNumeric {
    NInexact( Cmplx { re: re, im: im } )
}

pub fn polar(norm: f64, arg: f64) -> LNumeric {
    inexact(norm * arg.cos(), norm * arg.sin())
}

pub enum LReal {
    NRational(Rational),
    NFloat(f64)
}

impl LReal {
    pub fn to_inexact(&self) -> f64 {
        match self {
            &NRational(ref x) => x.to_f64(),
            &NFloat(x) => x,
        }
    }
}

pub fn coerce<T>(a: &LReal, b: &LReal,
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
        match self {
            &NRational( ref f ) => NRational( f.floor() ),
            &NFloat( ref f ) => NFloat( f.floor() ),
        }
    }

    fn ceil(&self) -> LReal {
        match self {
            &NRational( ref f ) => NRational( f.ceil() ),
            &NFloat( ref f ) => NFloat( f.ceil() ),
        }
    }

    fn round(&self) -> LReal {
        match self {
            &NRational( ref f ) => NRational( f.round() ),
            &NFloat( ref f ) => NFloat( f.round() ),
        }
    }

    fn trunc(&self) -> LReal {
        match self {
            &NRational( ref f ) => NRational( f.trunc() ),
            &NFloat( ref f ) => NFloat( f.trunc() ),
        }
    }

    fn fract(&self) -> LReal {
        match self {
            &NRational( ref f ) => NRational( f.fract() ),
            &NFloat( ref f ) => NFloat( f.fract() ),
        }
    }
}

pub fn get_real(n: &LNumeric) -> Option<LReal>
{
    match n {
        &NExact( Cmplx{ re: ref re, im: ref im } ) => if im.is_zero() {
                Some( NRational ( re.clone() ) )
            } else {
                None
            },
        &NInexact( Cmplx{ re: ref re, im: ref im } ) => if im.is_zero() {
                Some( NFloat ( re.clone() ) )
            } else {
                None
            },
    }
}

pub fn get_int(n: &LNumeric) -> Option<BigInt>
{
    match *n {
        NExact( Cmplx{ re: ref re, im: ref im } ) =>
            if im.is_zero() && *re.numerator() == One::one() {
                Some(re.denominator().clone())
            } else {
                None
            },
        NInexact(_) => None,
    }
}

pub fn get_uint(n: &LNumeric) -> Option<uint>
{
    match *n {
        NExact( Cmplx{ re: ref re, im: ref im } ) =>
            if im.is_zero() && *re.numerator() == One::one() && !re.denominator().is_negative() {
                Some(re.denominator().to_uint())
            } else {
                None
            },
        NInexact(_) => None,
    }
}

pub fn modulo(l: BigInt, r: BigInt) -> BigInt
{
    let q = l % r;
    if q.is_negative() {
        if r < q {
            q
        } else {
            q + r
        }
    } else if q.is_positive() {
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
    assert_eq!(NRational(Rational::new_int(2,1)) == NRational(Rational::new_int(2,1)), true);
    assert_eq!(NRational(Rational::new_int(2,1)) < NFloat(3.0), true);
    assert_eq!(NFloat(3.0) > NRational(Rational::new_int(2,1)), true);
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
