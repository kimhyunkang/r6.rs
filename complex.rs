use std::num::{Zero, One};

#[deriving(Eq, Clone)]
pub struct Cmplx<T> {
    pub re: T,
    pub im: T,
}

impl<T: Num> Cmplx<T> {
    #[inline]
    pub fn new(re: T, im: T) -> Cmplx<T> {
        Cmplx { re: re, im: im }
    }

    #[inline]
    pub fn norm_sq(&self) -> T {
        self.re * self.re + self.im * self.im
    }

    #[inline]
    pub fn sq(&self) -> Cmplx<T> {
        let im_frac2 = self.re * self.im;
        Cmplx { re: self.re * self.re - self.im * self.im, im: im_frac2 + im_frac2 }
    }

    #[inline]
    pub fn scale(&self, t: T) -> Cmplx<T> {
        Cmplx { re: self.re * t, im: self.im * t }
    }

    #[inline]
    pub fn unscale(&self, t: T) -> Cmplx<T> {
        Cmplx { re: self.re / t, im: self.im / t }
    }
}

impl<T: Zero + One> Cmplx<T> {
    #[inline]
    fn i() -> Cmplx<T> {
        Cmplx { re: Zero::zero(), im: One::one() }
    }
}

impl<T: Clone + Neg<T>> Cmplx<T> {
    #[inline]
    fn conjugate(&self) -> Cmplx<T> {
        Cmplx { re: self.re.clone(), im: -(self.im) }
    }
}

impl<T: Algebraic + Trigonometric + Num> Cmplx<T> {
    #[inline]
    pub fn norm(&self) -> T {
        self.re.hypot(&self.im)
    }

    #[inline]
    pub fn arg(&self) -> T {
        self.im.atan2(&self.re)
    }

    #[inline]
    pub fn to_polar(&self) -> (T, T) {
        (self.norm(), self.arg())
    }

    #[inline]
    pub fn from_polar(norm: T, arg: T) -> Cmplx<T> {
        Cmplx { re: norm * arg.cos(), im: norm * arg.sin() }
    }
}

impl<T: Zero> Zero for Cmplx<T> {
    #[inline]
    pub fn zero() -> Cmplx<T> {
        Cmplx { re: Zero::zero(), im: Zero::zero() }
    }

    #[inline]
    pub fn is_zero(&self) -> bool {
        self.re.is_zero() && self.im.is_zero()
    }
}

impl<T: Zero + One> One for Cmplx<T> {
    #[inline]
    pub fn one() -> Cmplx<T> {
        Cmplx { re: One::one(), im: Zero::zero() }
    }
}

impl<T: Num + Ord + ApproxEq<T>> ApproxEq<T> for Cmplx<T> {
    #[inline]
    fn approx_epsilon() -> T {
        let eps = ApproxEq::approx_epsilon::<T, T>();
        eps * eps
    }

    #[inline]
    fn approx_eq_eps(&self, other: &Cmplx<T>, approx_epsilon: &T) -> bool {
        ((*self) - (*other)).norm_sq() <= *approx_epsilon
    }

    #[inline]
    fn approx_eq(&self, other: &Cmplx<T>) -> bool {
        self.approx_eq_eps(other, &ApproxEq::approx_epsilon::<T, Cmplx<T>>())
    }
}

impl<T: Num> Add<Cmplx<T>, Cmplx<T>> for Cmplx<T> {
    #[inline]
    fn add(&self, other: &Cmplx<T>) -> Cmplx<T> {
        Cmplx { re: self.re + other.re, im: self.im + other.im }
    }
}

impl<T: Num> Sub<Cmplx<T>, Cmplx<T>> for Cmplx<T> {
    #[inline]
    fn sub(&self, other: &Cmplx<T>) -> Cmplx<T> {
        Cmplx { re: self.re - other.re, im: self.im - other.im }
    }
}

impl<T: Num> Mul<Cmplx<T>, Cmplx<T>> for Cmplx<T> {
    #[inline]
    fn mul(&self, other: &Cmplx<T>) -> Cmplx<T> {
        let re = self.re * other.re - self.im * other.im;
        let im = self.re * other.im + self.im * other.re;
        Cmplx { re: re, im: im }
    }
}

impl<T: Num> Div<Cmplx<T>, Cmplx<T>> for Cmplx<T> {
    #[inline]
    fn div(&self, other: &Cmplx<T>) -> Cmplx<T> {
        let norm_sq = other.norm_sq();
        Cmplx { re: (self.re * other.re + self.im * other.im) / norm_sq,
                im: (self.im * other.re - self.re * other.im) / norm_sq }
    }
}

impl<T: Neg<T>> Neg<Cmplx<T>> for Cmplx<T> {
    #[inline]
    fn neg(&self) -> Cmplx<T> {
        Cmplx { re: -self.re, im: -self.im }
    }
}

impl<T: Num> Fractional for Cmplx<T> {
    #[inline]
    fn recip(&self) -> Cmplx<T> {
        // 1 / (x+iy) = (x-iy) / (x^2 + y^2)
        let norm_sq = self.norm_sq();
        Cmplx { re: self.re / norm_sq, im: -(self.im) / norm_sq }
    }
}

impl<T: Exponential + Trigonometric + Real + Num> Exponential for Cmplx<T> {
    #[inline]
    fn exp(&self) -> Cmplx<T> {
        // e^(a+bi) = e^a * (cos b + i sin b)
        let pow = self.re.exp();
        Cmplx { re: self.im.cos() * pow, im: self.im.sin() * pow }
    }

    #[inline]
    fn exp2(&self) -> Cmplx<T> {
        // 2^z = e^(z ln 2)
        self.scale(Real::ln_2()).exp()
    }

    #[inline]
    fn ln(&self) -> Cmplx<T> {
        // e^(a+bi) = e^a * (cos b + i sin b)
        Cmplx{ re: self.norm().ln(), im: self.arg() }
    }

    #[inline]
    fn log(&self, base: &Cmplx<T>) -> Cmplx<T> {
        self.ln() / base.ln()
    }

    #[inline]
    fn log2(&self) -> Cmplx<T> {
        // log_2 z = ln z / ln 2
        self.ln().unscale(Real::ln_2())
    }

    #[inline]
    fn log10(&self) -> Cmplx<T> {
        // log_2 10 = ln z / ln 10
        self.ln().unscale(Real::ln_10())
    }
}

impl<T: Exponential + Trigonometric + Real + Num + NumCast> Algebraic for Cmplx<T> {
    #[inline]
    fn pow(&self, n: &Cmplx<T>) -> Cmplx<T> {
        (self.ln() * (*n)).exp()
    }

    #[inline]
    fn sqrt(&self) -> Cmplx<T> {
        // sqrt(z) = sqrt(|z|) * { cos(arg z / 2) + i sin(arg z / 2) }
        let norm_sqrt = self.norm().sqrt();
        let arg_frac2 = self.arg() / NumCast::from(2);
        Cmplx { re: arg_frac2.cos() * norm_sqrt, im: arg_frac2.sin() * norm_sqrt }
    }

    #[inline]
    fn rsqrt(&self) -> Cmplx<T> {
        // 1/sqrt(z) = 1/sqrt(|z|) * 1/{ cos(arg z / 2) + i sin(arg z / 2) }
        //           = 1/sqrt(|z|) * { cos(arg z / 2) - i sin(arg z / 2)
        let norm_rsqrt = self.norm().rsqrt();
        let arg_frac2 = self.arg() / NumCast::from(2);
        Cmplx { re: arg_frac2.cos() * norm_rsqrt, im: (-arg_frac2).sin() * norm_rsqrt }
    }

    #[inline]
    fn cbrt(&self) -> Cmplx<T> {
        // cbrt(z) = cbrt(|z|) * { cos(arg z / 3) + i sin(arg z / 3) }
        let norm_cbrt = self.norm().cbrt();
        let arg_frac3 = self.arg() / NumCast::from(3);
        Cmplx { re: arg_frac3.cos() * norm_cbrt, im: arg_frac3.sin() * norm_cbrt }
    }

    #[inline]
    fn hypot(&self, rhs: &Cmplx<T>) -> Cmplx<T> {
        (self.sq() + rhs.sq()).sqrt()
    }
}

impl<T: Exponential + Trigonometric + Real + Num + NumCast + Fractional> Trigonometric
    for Cmplx<T>
{
    #[inline]
    fn sin(&self) -> Cmplx<T> {
        // sin(x + iy) = sin x cosh y + i cos x sinh y
        Cmplx { re: self.re.sin() * self.im.cosh(),
                im: self.re.cos() * self.im.sinh() }
    }

    #[inline]
    fn cos(&self) -> Cmplx<T> {
        // cos(x + iy) = cos x cosh y - i sin x sinh y
        Cmplx { re: self.re.cos() * self.im.cosh(),
                im: -(self.re.sin() * self.im.sinh()) }
    }

    #[inline]
    fn tan(&self) -> Cmplx<T> {
        // tan(x + iy) = (sin 2x + i sinh 2y) / (cos 2x + cosh 2y)
        let twox = self.re + self.re;
        let twoy = self.im + self.im;
        let n = twox.cos() + twoy.cosh();
        Cmplx { re: twox.sin() / n, im: twoy.sinh() / n }
    }

    #[inline]
    fn asin(&self) -> Cmplx<T> {
        // asin x = -i * ln( ix + sqrt(1 - x^2) )
        let i = Cmplx::i();
        let y = One::one::<Cmplx<T>>() - self.sq();
        let z = (*self) * i + y.sqrt();
        z.ln() * -i
    }

    #[inline]
    fn acos(&self) -> Cmplx<T> {
        // acos x = -i * ln( x + sqrt(x^2 - 1) )
        let minus_i = -Cmplx::i();
        let y = self.sq() - One::one::<Cmplx<T>>();
        let z = (*self) + y.sqrt();
        minus_i * z.ln()
    }

    #[inline]
    fn atan(&self) -> Cmplx<T> {
        // atan x = i/2 * ( ln(1 - ix) - ln(1 + ix) )
        let one = One::one::<Cmplx<T>>();
        let i = Cmplx::i();
        let half = NumCast::from::<T, int>(2).recip();
        let ihalf = Cmplx { re: Zero::zero(), im: half };
        let ix = i * (*self);
        let y = (one - ix).ln() - (one + ix).ln();
        ihalf * y
    }

    #[inline]
    fn atan2(&self, other: &Cmplx<T>) -> Cmplx<T> {
        ((*other) / (*self)).atan()
    }

    #[inline]
    fn sin_cos(&self) -> (Cmplx<T>, Cmplx<T>) {
        (self.sin(), self.cos())
    }
}

#[test]
fn test_add() {
    let x = Cmplx { re: 1.0, im: 2.0 };
    let y = Cmplx { re: 3.0, im: -4.0 };
    let z = Cmplx { re: 4.0, im: -2.0 };
    assert_eq!(x + y, z)
}

#[test]
fn test_sub() {
    let x = Cmplx { re: 1.0, im: 2.0 };
    let y = Cmplx { re: 3.0, im: -4.0 };
    let z = Cmplx { re: -2.0, im: 6.0 };
    assert_eq!(x - y, z)
}

#[test]
fn test_mul() {
    let x = Cmplx { re: 1.0, im: 2.0 };
    let y = Cmplx { re: 3.0, im: -4.0 };
    let z = Cmplx { re: 11.0, im: 2.0 };
    assert_eq!(x * y, z)
}

#[test]
fn test_div() {
    let x = Cmplx { re: 11.0, im: 2.0 };
    let y = Cmplx { re: 3.0, im: -4.0 };
    let z = Cmplx { re: 1.0, im: 2.0 };
    assert_eq!(x / y, z)
}

#[test]
fn test_neg() {
    let x = Cmplx { re: 1.0, im: 2.0 };
    let y = Cmplx { re: -1.0, im: -2.0 };
    assert_eq!(-x, y)
}

#[test]
fn test_approx_eq() {
    let x = Cmplx { re: 1.0000000, im: 2.0 };
    let y = Cmplx { re: 1.0000001, im: 2.0 };
    assert!(x != y)
    assert_approx_eq!(x, y)
}

#[test]
fn test_polar() {
    let x = Cmplx { re: 1.0f64, im: 3.0f64.sqrt() };
    let norm = 2.0f64;
    let arg = Real::frac_pi_3::<f64>();
    assert_approx_eq!(x.norm(), norm);
    assert_approx_eq!(x.arg(), arg);
    assert_approx_eq!(Cmplx::from_polar(norm, arg), x);
}

#[test]
fn test_euler() {
    let pi_i = Cmplx { re: 0.0, im: Real::pi::<f64>() };
    let minus_1 = Cmplx { re: -1.0, im: 0.0 };
    assert_approx_eq!(pi_i.exp(), minus_1)
}

#[test]
fn test_exp() {
    let x = Cmplx { re: 1.0, im: 2.0 };
    assert_approx_eq!(x.exp().ln(), x)
    assert_approx_eq!(x.ln().exp(), x)
}

#[test]
fn test_ln2() {
    let x = Cmplx { re: 1.0, im: 2.0 };
    let two = Cmplx { re: 2.0, im: 0.0 };
    assert_approx_eq!(x.log2(), x.log(&two))
    assert_approx_eq!(x.log2().exp2(), x)
    assert_approx_eq!(x.exp2().log2(), x)
}

#[test]
fn test_ln10() {
    let x = Cmplx { re: 1.0, im: 2.0 };
    let ten = Cmplx { re: 10.0, im: 0.0 };
    assert_approx_eq!(x.log10(), x.log(&ten))
}

#[test]
fn test_pow() {
    let x = Cmplx { re: 1.0f64, im: 2.0f64 };
    let e = Cmplx { re: Real::e::<f64>(), im: 0.0 };
    assert_approx_eq!(x.exp(), e.pow(&x))
}

#[test]
fn test_exp2() {
    let x = Cmplx { re: 1.0, im: 2.0};
    let two = Cmplx { re: 2.0, im: 0.0 };
    assert_approx_eq!(x.exp2(), two.pow(&x))
}

#[test]
fn test_sqrt() {
    let x = Cmplx { re: 1.0, im: -1.0 };
    let xsq = Cmplx { re: 0.0, im: -2.0 };
    assert_approx_eq!(x.sq(), xsq);
    assert_approx_eq!(xsq.sqrt(), x);
}

#[test]
fn test_recip() {
    let x = Cmplx { re: 1.0, im: -1.0 };
    assert_approx_eq!(x.recip() * x, One::one());
}

#[test]
fn test_rsqrt() {
    let x = Cmplx { re: 1.0, im: -1.0 };
    let y = x.rsqrt();
    assert_approx_eq!(y, x.recip().sqrt());
    assert_approx_eq!(y, x.sqrt().recip());
}

#[test]
fn test_asin() {
    let x = Cmplx { re: 1.0, im: -2.0 };
    assert_approx_eq!(x.sin().asin(), x);
}

#[test]
fn test_acos() {
    let x = Cmplx { re: 1.0, im: -2.0 };
    assert_approx_eq!(x.cos().acos(), x);
}

#[test]
fn test_tan() {
    let x = Cmplx { re: 1.0, im: -2.0 };
    assert_approx_eq!(x.sin() / x.cos(), x.tan());
}

#[test]
fn test_atan() {
    let x = Cmplx { re: 1.0, im: -2.0 };
    assert_approx_eq!(x.tan().atan(), x);
}
