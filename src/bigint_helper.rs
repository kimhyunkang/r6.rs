use std::num::{Zero, One, FromStrRadix, ToStrRadix, FromPrimitive, NumCast};
use std::cmp::min;
use std::fmt;
use num::bigint::BigInt;
use num::rational::{Ratio, BigRational};

pub fn rational_from_float<T:FloatMath + fmt::Show>(f: T) -> BigRational {
    match float_disassemble(f) {
        None => fail!("f is not normal float"),
        Some((mantissa, exp)) => if exp < 0 {
                let _1: BigInt = FromPrimitive::from_int(1).unwrap();
                let n = _1 << (-exp as uint);
                Ratio::new(n, mantissa)
            } else {
                let d = mantissa << (exp as uint);
                Ratio::new(One::one(), d)
            }
    }
}

pub fn pow_uint<T:Clone + Zero + One + Div<T,T> + Mul<T, T>>(radix: &T, pow: uint) -> T {
    let _0 = Zero::zero();
    let _1 = One::one();

    if pow   == 0u { return _1; }
    if radix.is_zero() { return _0; }
    let mut my_pow     = pow;
    let mut total      = _1;
    let mut multiplier = radix.clone();
    while my_pow > 0u {
        if my_pow % 2u == 1u {
            total = total * multiplier;
        }
        my_pow = my_pow / 2u;
        multiplier = multiplier * multiplier;
    }
    total
}

#[allow(deprecated)]
pub fn bigint_to_float<T:FloatMath + Zero>(n: &BigInt) -> T {
    if n.is_zero() {
        return Zero::zero()
    }

    let mdigits = Float::mantissa_digits(None::<T>);
    let neg = n.is_negative();
    let repr = n.abs().to_str_radix(2);
    let mantissa_repr = repr.as_slice().slice(0, min(mdigits, repr.len()));
    let u_mantissa:i64 = FromStrRadix::from_str_radix(mantissa_repr, 2).unwrap();
    let i_mantissa = if neg {
        -u_mantissa
    } else {
        u_mantissa
    };
    let exp = (repr.len() - mantissa_repr.len()) as int;
    let m:T = NumCast::from(i_mantissa).unwrap();
    FloatMath::ldexp(m, exp)
}

pub fn rational_to_f64(r: &BigRational) -> f64 {
    let d:f64 = bigint_to_float(r.denom());
    let n:f64 = bigint_to_float(r.numer());

    n / d
}

pub fn float_disassemble<T:FloatMath + fmt::Show>(f: T) -> Option<(BigInt, i16)> {
    if !f.is_finite() {
        return None
    }
    let (u_mantissa, exp, sign) = f.integer_decode();
    let i_mantissa = if sign < 0 {
        -(u_mantissa as i64)
    } else {
        u_mantissa as i64
    };
    let mantissa = FromPrimitive::from_i64(i_mantissa).unwrap();
    Some((mantissa, exp))
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
fn float_disassemble_test() {
    let _1: BigInt = One::one();

    let ix = 2.0f64;
    let mantissa = _1 << 52;
    let exp = -51;
    assert_eq!(float_disassemble(ix), Some((mantissa, exp)))

    let ix = 0.5f64;
    let mantissa = _1 << 52;
    let exp = -53;
    assert_eq!(float_disassemble(ix), Some((mantissa, exp)))

    let ix = 0.25f64;
    let mantissa = _1 << 52;
    let exp = -54;
    assert_eq!(float_disassemble(ix), Some((mantissa, exp)))

    let ix = 0.75f64;
    let _3: BigInt = FromPrimitive::from_uint(3).unwrap();
    let mantissa = _3 << 51;
    let exp = -53;
    assert_eq!(float_disassemble(ix), Some((mantissa, exp)))
}
