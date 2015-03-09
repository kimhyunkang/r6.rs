use std::fmt;
use std::ops::Deref;
use std::num::FromPrimitive;

use num::{Zero, One};
use num::complex::{Complex, Complex64};
use num::rational::{Ratio, BigRational};

use real::{Real, rat2flo};
use datum::{DatumType, Object};

pub fn int_cmplx(re: isize, im: isize) -> Box<Number> {
    if im == 0 {
        box Real::Fixnum(re)
    } else {
        let re_part = Ratio::new(
            FromPrimitive::from_int(re).unwrap(),
            One::one()
        );
        let im_part = Ratio::new(
            FromPrimitive::from_int(im).unwrap(),
            One::one()
        );
        box Complex::new(re_part, im_part)
    }
}

pub fn to_imaginary(im: Real) -> Box<Number> {
    match im {
        Real::Flonum(i) => box Complex::new(0.0, i),
        _ => box Complex::new(Zero::zero(), im.to_ratio().unwrap())
    }
}

pub fn complex(re: Real, im: Real) -> Box<Number> {
    if re.is_exact() && im.is_exact() {
        if im.is_zero() {
            box re
        } else {
            box Complex::new(re.to_ratio().unwrap(), im.to_ratio().unwrap())
        }
    } else {
        box Complex::new(re.to_f64(), im.to_f64())
    }
}

pub trait Number: Object + fmt::Debug {
    fn num_eq(&self, rhs: &Number) -> bool;

    fn accept(&self, v: &mut NumberVisitor);

    fn get_real(&self) -> Option<&Real> { None }
    fn get_exact(&self) -> Option<&Complex<BigRational>> { None }
    fn get_inexact(&self) -> Option<&Complex64> { None }
}

impl<T: Number + ?Sized> Object for T {
    fn obj_eq(&self, rhs: &Object) -> bool {
        if let Some(n) = rhs.get_number() {
            self.num_eq(n)
        } else {
            false
        }
    }

    fn get_type(&self) -> DatumType {
        DatumType::Number
    }
}

pub trait NumberVisitor {
    fn visit_real(&mut self, r: &Real);
    fn visit_exact(&mut self, r: &Complex<BigRational>);
    fn visit_inexact(&mut self, r: &Complex64);
}

trait Coercion<T> {
    fn on_real(&self, lhs: &Real, rhs: &Real) -> T;
    fn on_exact(&self, lhs: &Complex<BigRational>, rhs: &Complex<BigRational>) -> T;
    fn on_inexact(&self, lhs: &Complex64, rhs: &Complex64) -> T;
}

struct RealDispatcher<'a, T> {
    val: &'a Real,
    coercion: &'a Coercion<T>,
    res: Option<T>
}

impl<'a, T> RealDispatcher<'a, T> {
    fn new(val: &'a Real, coercion: &'a Coercion<T>) -> RealDispatcher<'a, T> {
        RealDispatcher {
            val: val,
            coercion: coercion,
            res: None
        }
    }
}

impl<'a, T> NumberVisitor for RealDispatcher<'a, T> {
    fn visit_real(&mut self, rhs: &Real) {
        self.res = Some(self.coercion.on_real(self.val, rhs));
    }

    fn visit_exact(&mut self, rhs: &Complex<BigRational>) {
        self.res = Some(match self.val.lift_complex() {
            Ok(re) => {
                let lhs = Complex::new(re, Zero::zero());
                self.coercion.on_exact(&lhs, rhs)
            },
            Err(re) => {
                let lhs = Complex::new(re, 0.0);
                let i_rhs = Complex::new(rat2flo(&rhs.re), rat2flo(&rhs.im));
                self.coercion.on_inexact(&lhs, &i_rhs)
            }
        });
    }

    fn visit_inexact(&mut self, rhs: &Complex64) {
        let lhs = Complex::new(self.val.to_f64(), 0.0);
        self.res = Some(self.coercion.on_inexact(&lhs, rhs));
    }
}

struct ExactDispatcher<'a, T> {
    val: &'a Complex<BigRational>,
    coercion: &'a Coercion<T>,
    res: Option<T>
}

impl<'a, T> ExactDispatcher<'a, T> {
    fn new(val: &'a Complex<BigRational>, coercion: &'a Coercion<T>) -> ExactDispatcher<'a, T> {
        ExactDispatcher {
            val: val,
            coercion: coercion,
            res: None
        }
    }
}

impl<'a, T> NumberVisitor for ExactDispatcher<'a, T> {
    fn visit_real(&mut self, rhs: &Real) {
        self.res = Some(match rhs.lift_complex() {
            Ok(re) => {
                let rhs = Complex::new(re, Zero::zero());
                self.coercion.on_exact(self.val, &rhs)
            },
            Err(re) => {
                let i_lhs = Complex::new(rat2flo(&self.val.re), rat2flo(&self.val.im));
                let rhs = Complex::new(re, 0.0);
                self.coercion.on_inexact(&i_lhs, &rhs)
            }
        });
    }

    fn visit_exact(&mut self, rhs: &Complex<BigRational>) {
        self.res = Some(self.coercion.on_exact(self.val, rhs));
    }

    fn visit_inexact(&mut self, rhs: &Complex64) {
        let lhs = Complex::new(rat2flo(&self.val.re), rat2flo(&self.val.im));
        self.res = Some(self.coercion.on_inexact(&lhs, rhs));
    }
}

struct InexactDispatcher<'a, T> {
    val: &'a Complex64,
    coercion: &'a Coercion<T>,
    res: Option<T>
}

impl<'a, T> InexactDispatcher<'a, T> {
    fn new(val: &'a Complex64, coercion: &'a Coercion<T>) -> InexactDispatcher<'a, T> {
        InexactDispatcher {
            val: val,
            coercion: coercion,
            res: None
        }
    }
}

impl<'a, T> NumberVisitor for InexactDispatcher<'a, T> {
    fn visit_real(&mut self, rhs: &Real) {
        let cmplx_rhs = Complex::new(rhs.to_f64(), 0.0);
        self.res = Some(self.coercion.on_inexact(self.val, &cmplx_rhs));
    }

    fn visit_exact(&mut self, rhs: &Complex<BigRational>) {
        let i_rhs = Complex::new(rat2flo(&rhs.re), rat2flo(&rhs.im));
        self.res = Some(self.coercion.on_inexact(self.val, &i_rhs));
    }

    fn visit_inexact(&mut self, rhs: &Complex64) {
        self.res = Some(self.coercion.on_inexact(self.val, rhs));
    }
}

pub struct NumberEq;
pub static NUMBER_EQ: NumberEq = NumberEq;
impl Coercion<bool> for NumberEq {
    fn on_real(&self, lhs: &Real, rhs: &Real) -> bool { lhs == rhs }
    fn on_exact(&self, lhs: &Complex<BigRational>, rhs: &Complex<BigRational>) -> bool { lhs == rhs }
    fn on_inexact(&self, lhs: &Complex64, rhs: &Complex64) -> bool { lhs == rhs }
}

impl Number for Real {
    fn get_real(&self) -> Option<&Real> {
        Some(self)
    }

    fn accept(&self, v: &mut NumberVisitor) {
        v.visit_real(self)
    }

    fn num_eq(&self, rhs: &Number) -> bool {
        let mut dispatcher = RealDispatcher::new(self, &NUMBER_EQ);
        rhs.accept(&mut dispatcher);
        dispatcher.res.unwrap()
    }
}

impl Number for Complex<BigRational> {
    fn get_exact(&self) -> Option<&BigRational> {
        Some(self)
    }

    fn accept(&self, v: &mut NumberVisitor) {
        v.visit_exact(self)
    }

    fn num_eq(&self, rhs: &Number) -> bool {
        let mut dispatcher = ExactDispatcher::new(self, &NUMBER_EQ);
        rhs.accept(&mut dispatcher);
        dispatcher.res.unwrap()
    }
}

impl Number for Complex64 {
    fn get_inexact(&self) -> Option<&Complex64> {
        Some(self)
    }

    fn accept(&self, v: &mut NumberVisitor) {
        v.visit_inexact(self)
    }

    fn num_eq(&self, rhs: &Number) -> bool {
        let mut dispatcher = InexactDispatcher::new(self, &NUMBER_EQ);
        rhs.accept(&mut dispatcher);
        dispatcher.res.unwrap()
    }
}

impl PartialEq for Box<Number> {
    fn eq(&self, other: &Box<Number>) -> bool {
        self.num_eq(other.deref())
    }
}

#[cfg(test)]
mod test {
    use real::Real;
    use num_trait::Number;
    use num::complex::{Complex, Complex64};
    use num::rational::BigRational;
    use num::{Zero, One};

    #[test]
    fn test_real_exact_eq() {
        let lhs = Real::Fixnum(1);
        let rhs: Complex<BigRational> = Complex::new(One::one(), Zero::zero());

        assert_eq!(box lhs as Box<Number>, box rhs as Box<Number>);
    }

    #[test]
    fn test_real_inexact_eq() {
        let lhs = Real::Fixnum(1);
        let rhs: Complex64 = Complex::new(1.0, 0.0);

        assert_eq!(box lhs as Box<Number>, box rhs as Box<Number>);
    }
}
