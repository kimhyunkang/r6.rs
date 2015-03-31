use std::fmt;
use std::ops::{Deref, Add, Sub, Mul, Div, Neg};
use std::num::FromPrimitive;

use num::{Zero, One};
use num::complex::{Complex, Complex64};
use num::rational::{Ratio, BigRational};

use real::{Real, rat2flo};
use datum::{DatumType, Object, UpcastObject};

pub fn int_cmplx(re: isize, im: isize) -> Box<Number> {
    if im == 0 {
        box Real::Fixnum(re)
    } else {
        let re_part = Ratio::new(
            FromPrimitive::from_isize(re).unwrap(),
            One::one()
        );
        let im_part = Ratio::new(
            FromPrimitive::from_isize(im).unwrap(),
            One::one()
        );
        box Complex::new(re_part, im_part)
    }
}

pub fn exact_cmplx(re: (isize, isize), im: (isize, isize)) -> Box<Number> {
    let re_rat = Ratio::new(
        FromPrimitive::from_isize(re.0).unwrap(),
        FromPrimitive::from_isize(re.1).unwrap()
    );

    let im_rat = Ratio::new(
        FromPrimitive::from_isize(im.0).unwrap(),
        FromPrimitive::from_isize(im.1).unwrap()
    );

    box Complex::new(re_rat, im_rat)
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

pub fn int_rat(numer: isize, denom: isize) -> Box<Number> {
    box Real::Rational(Ratio::new(
        FromPrimitive::from_isize(numer).unwrap(),
        FromPrimitive::from_isize(denom).unwrap()
    ))
}

pub trait Number: Object + fmt::Debug {
    fn num_eq(&self, rhs: &Number) -> bool;
    fn num_add(&self, rhs: &Number) -> Box<Number>;
    fn num_sub(&self, rhs: &Number) -> Box<Number>;
    fn num_mul(&self, rhs: &Number) -> Box<Number>;
    fn num_div(&self, rhs: &Number) -> Box<Number>;

    fn num_neg(&self) -> Box<Number>;

    fn num_is_zero(&self) -> bool;

    fn is_exact(&self) -> bool;

    fn accept(&self, v: &mut NumberVisitor);

    fn get_real(&self) -> Option<&Real> { None }
    fn get_exact(&self) -> Option<&Complex<BigRational>> { None }
    fn get_inexact(&self) -> Option<&Complex64> { None }

    fn clone_num(&self) -> Box<Number>;

    fn num_upcast(self: Box<Self>) -> Box<Object>;
}

impl<T: Number> Object for T {
    fn obj_eq(&self, rhs: &Object) -> bool {
        if let Some(n) = rhs.get_number() {
            self.num_eq(n)
        } else {
            false
        }
    }

    fn get_type(&self) -> DatumType {
        DatumType::Num
    }

    fn get_number(&self) -> Option<&Number> {
        Some(self)
    }
}

impl UpcastObject for Number {
    fn upcast(self: Box<Number>) -> Box<Object> {
        self.num_upcast()
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

macro_rules! impl_coercion {
    ($type_name:ident, $static_name:ident, $op:ident) => (
        pub struct $type_name;
        pub static $static_name: $type_name = $type_name;
        impl Coercion<Box<Number>> for $type_name {
            fn on_real(&self, lhs: &Real, rhs: &Real) -> Box<Number> {
                Box::new(lhs.$op(rhs))
            }

            fn on_exact(&self, lhs: &Complex<BigRational>, rhs: &Complex<BigRational>) -> Box<Number> {
                Box::new(lhs.$op(rhs))
            }

            fn on_inexact(&self, lhs: &Complex64, rhs: &Complex64) -> Box<Number> {
                Box::new(lhs.$op(rhs))
            }
        }
    )
}

impl_coercion!(NumberAdd, NUMBER_ADD, add);
impl_coercion!(NumberSub, NUMBER_SUB, sub);
impl_coercion!(NumberMul, NUMBER_MUL, mul);
impl_coercion!(NumberDiv, NUMBER_DIV, div);

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

    fn num_add(&self, rhs: &Number) -> Box<Number> {
        let mut dispatcher = RealDispatcher::new(self, &NUMBER_ADD);
        rhs.accept(&mut dispatcher);
        dispatcher.res.unwrap()
    }

    fn num_sub(&self, rhs: &Number) -> Box<Number> {
        let mut dispatcher = RealDispatcher::new(self, &NUMBER_SUB);
        rhs.accept(&mut dispatcher);
        dispatcher.res.unwrap()
    }

    fn num_mul(&self, rhs: &Number) -> Box<Number> {
        let mut dispatcher = RealDispatcher::new(self, &NUMBER_MUL);
        rhs.accept(&mut dispatcher);
        dispatcher.res.unwrap()
    }

    fn num_div(&self, rhs: &Number) -> Box<Number> {
        let mut dispatcher = RealDispatcher::new(self, &NUMBER_DIV);
        rhs.accept(&mut dispatcher);
        dispatcher.res.unwrap()
    }

    fn num_neg(&self) -> Box<Number> {
        Box::new(self.neg())
    }

    fn num_is_zero(&self) -> bool {
        self.is_zero()
    }

    fn is_exact(&self) -> bool {
        Real::is_exact(self)
    }

    fn clone_num(&self) -> Box<Number> {
        box self.clone()
    }

    fn num_upcast(self: Box<Real>) -> Box<Object> {
        self
    }
}

impl Number for Complex<BigRational> {
    fn get_exact(&self) -> Option<&Complex<BigRational>> {
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

    fn num_add(&self, rhs: &Number) -> Box<Number> {
        let mut dispatcher = ExactDispatcher::new(self, &NUMBER_ADD);
        rhs.accept(&mut dispatcher);
        dispatcher.res.unwrap()
    }

    fn num_sub(&self, rhs: &Number) -> Box<Number> {
        let mut dispatcher = ExactDispatcher::new(self, &NUMBER_SUB);
        rhs.accept(&mut dispatcher);
        dispatcher.res.unwrap()
    }

    fn num_mul(&self, rhs: &Number) -> Box<Number> {
        let mut dispatcher = ExactDispatcher::new(self, &NUMBER_MUL);
        rhs.accept(&mut dispatcher);
        dispatcher.res.unwrap()
    }

    fn num_div(&self, rhs: &Number) -> Box<Number> {
        let mut dispatcher = ExactDispatcher::new(self, &NUMBER_DIV);
        rhs.accept(&mut dispatcher);
        dispatcher.res.unwrap()
    }

    fn num_neg(&self) -> Box<Number> {
        Box::new(self.neg())
    }

    fn num_is_zero(&self) -> bool {
        self.is_zero()
    }

    fn is_exact(&self) -> bool {
        true
    }

    fn clone_num(&self) -> Box<Number> {
        box self.clone()
    }

    fn num_upcast(self: Box<Complex<BigRational>>) -> Box<Object> {
        self
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

    fn num_add(&self, rhs: &Number) -> Box<Number> {
        let mut dispatcher = InexactDispatcher::new(self, &NUMBER_ADD);
        rhs.accept(&mut dispatcher);
        dispatcher.res.unwrap()
    }

    fn num_sub(&self, rhs: &Number) -> Box<Number> {
        let mut dispatcher = InexactDispatcher::new(self, &NUMBER_SUB);
        rhs.accept(&mut dispatcher);
        dispatcher.res.unwrap()
    }

    fn num_mul(&self, rhs: &Number) -> Box<Number> {
        let mut dispatcher = InexactDispatcher::new(self, &NUMBER_MUL);
        rhs.accept(&mut dispatcher);
        dispatcher.res.unwrap()
    }

    fn num_div(&self, rhs: &Number) -> Box<Number> {
        let mut dispatcher = InexactDispatcher::new(self, &NUMBER_DIV);
        rhs.accept(&mut dispatcher);
        dispatcher.res.unwrap()
    }

    fn num_neg(&self) -> Box<Number> {
        Box::new(self.neg())
    }

    fn num_is_zero(&self) -> bool {
        self.is_zero()
    }

    fn is_exact(&self) -> bool {
        false
    }

    fn clone_num(&self) -> Box<Number> {
        box self.clone()
    }

    fn num_upcast(self: Box<Complex64>) -> Box<Object> {
        self
    }
}

impl PartialEq for Box<Number> {
    fn eq(&self, other: &Box<Number>) -> bool {
        self.num_eq(other.deref())
    }
}

macro_rules! impl_num_ops {
    ($trait_name:ident, $trait_func:ident, $impl_func:ident) => (
        impl<'a, 'b> $trait_name<&'a Number> for &'b Number {
            type Output = Box<Number>;

            fn $trait_func(self, other: &Number) -> Box<Number> {
                self.$impl_func(other)
            }
        }
    )
}

impl_num_ops!(Add, add, num_add);
impl_num_ops!(Sub, sub, num_sub);
impl_num_ops!(Mul, mul, num_mul);
impl_num_ops!(Div, div, num_div);

#[cfg(test)]
mod test {
    use real::Real;
    use num_trait::Number;
    use num::complex::{Complex, Complex64};
    use num::rational::BigRational;
    use num::{Zero, One};

    #[test]
    fn test_real_exact_eq() {
        let lhs: Box<Number> = box Real::Fixnum(1);
        let rhs_plain: Complex<BigRational> = Complex::new(One::one(), Zero::zero());
        let rhs: Box<Number> = box rhs_plain;

        assert_eq!(&lhs, &rhs);
    }

    #[test]
    fn test_real_inexact_eq() {
        let lhs: Box<Number> = box Real::Fixnum(1);
        let rhs_plain: Complex64 = Complex::new(1.0, 0.0);
        let rhs: Box<Number> = box rhs_plain;

        assert_eq!(&lhs, &rhs);
    }
}
