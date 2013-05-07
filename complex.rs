use core::num::{One, Zero};
use core::num::One::one;
use core::num::Zero::zero;
use core::cmp::{Eq};
use core::ops::{Neg, Add, Sub, Mul, Div};

pub struct Complex<T> {
    priv real: T,
    priv imag: T,
}

pub impl<T:Copy> Complex<T> {
    fn new(real: T, imag: T) -> Complex<T> {
        Complex {
            real: real,
            imag: imag,
        }
    }

    fn get_real(&self) -> T {
        self.real
    }

    fn get_imag(&self) -> T {
        self.imag
    }
}

pub impl<T:Add<T,T> + Mul<T,T>> Complex<T> {
    fn abs(&self) -> T {
        self.real * self.real + self.imag * self.imag
    }
}

impl<T:One + Zero> One for Complex<T> {
    fn one() -> Complex<T> {
        Complex {
            real: one(),
            imag: zero(),
        }
    }
}

impl<T:Zero> Zero for Complex<T> {
    fn zero() -> Complex<T> {
        Complex {
            real: zero(),
            imag: zero(),
        }
    }
}

impl<T:Eq> Eq for Complex<T> {
    fn eq(&self, other: &Complex<T>) -> bool {
        self.real == other.real && self.imag == other.imag
    }
    fn ne(&self, other: &Complex<T>) -> bool {
        self.real != other.real || self.imag != other.imag
    }
}

impl<T: Neg<T>> Neg<Complex<T>> for Complex<T> {
    fn neg(&self) -> Complex<T> {
        Complex {
            real: -self.real,
            imag: -self.imag,
        }
    }
}

impl<T: Add<T,T>> Add<Complex<T>, Complex<T>> for Complex<T> {
    fn add(&self, rhs: &Complex<T>) -> Complex<T> {
        Complex {
            real: self.real + rhs.real,
            imag: self.imag + rhs.imag,
        }
    }
}

impl<T: Sub<T,T>> Sub<Complex<T>, Complex<T>> for Complex<T> {
    fn sub(&self, rhs: &Complex<T>) -> Complex<T> {
        Complex {
            real: self.real - rhs.real,
            imag: self.imag - rhs.imag,
        }
    }
}

impl<T: Mul<T,T> + Add<T,T> + Sub<T,T>> Mul<Complex<T>, Complex<T>> for Complex<T> {
    fn mul(&self, rhs: &Complex<T>) -> Complex<T> {
        Complex {
            real: self.real * rhs.real - self.imag * rhs.imag,
            imag: self.imag * rhs.real + self.real * rhs.imag,
        }
    }
}

impl<T: Mul<T,T> + Add<T,T> + Sub<T,T> + Div<T,T>> Div<Complex<T>, Complex<T>> for Complex<T> {
    fn div(&self, rhs: &Complex<T>) -> Complex<T> {
        let n = rhs.abs();
        Complex {
            real: (self.real * rhs.real + self.imag * rhs.imag) / n,
            imag: (self.imag * rhs.real - self.real * rhs.imag) / n,
        }
    }
}
