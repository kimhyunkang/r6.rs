use std::num::{Zero, One, FromStrRadix, ToStrRadix};
use std::cmp::min;
use std::cast;
use extra::bigint::BigInt;

pub fn pow_uint<T:Clone + Zero + One + Div<T,T> + Mul<T, T>>(radix: &T, pow: uint) -> T {
    let _0 = Zero::zero();
    let _1 = One::one();

    if pow   == 0u { return _1; }
    if radix.is_zero() { return _0; }
    let mut my_pow     = pow;
    let mut total      = _1;
    let mut multiplier = radix.clone();
    while (my_pow > 0u) {
        if my_pow % 2u == 1u {
            total = total * multiplier;
        }
        my_pow = my_pow / 2u;
        multiplier = multiplier * multiplier;
    }
    total
}

pub fn bigint_to_float<T:Float + NumCast + Zero>(n: &BigInt) -> T {
    if n.is_zero() {
        return Zero::zero()
    }

    let mdigits = Float::mantissa_digits::<T>();
    let neg = n.is_negative();
    let repr = n.abs().to_str_radix(2);
    let mantissa_repr = repr.slice(0, min(mdigits, repr.len()));
    let u_mantissa:i64 = FromStrRadix::from_str_radix(mantissa_repr, 2).unwrap();
    let i_mantissa = if neg {
        -u_mantissa
    } else {
        u_mantissa
    };
    let exp = (repr.len() - mantissa_repr.len()) as int;
    Float::ldexp(NumCast::from(i_mantissa), exp)
}

pub fn float_disassemble<T:Float>(f: T) -> Option<(BigInt, int)> {
    if !f.is_finite() {
        return None
    }
    let base = 1u64 << (Float::mantissa_digits::<T>() - 1);
    let mask = base-1;
    let (fr, fexp) = f.frexp();
    let u_mantissa = unsafe { cast::transmute::<T, u64>(fr) } & mask;
    let i_mantissa = if f.is_negative() {
        -((u_mantissa + base) as i64)
    } else {
        (u_mantissa + base) as i64
    };
    let mantissa = FromStrRadix::from_str_radix(i_mantissa.to_str_radix(16), 16).unwrap();
    let exp = fexp - (Float::mantissa_digits::<T>() as int);
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
    let ix = 2.0f64;
    let mantissa = One::one::<BigInt>() << 52;
    let exp = -51;
    assert_eq!(float_disassemble(ix), Some((mantissa, exp)))

    let ix = 0.5f64;
    let mantissa = One::one::<BigInt>() << 52;
    let exp = -53;
    assert_eq!(float_disassemble(ix), Some((mantissa, exp)))

    let ix = 0.25f64;
    let mantissa = One::one::<BigInt>() << 52;
    let exp = -54;
    assert_eq!(float_disassemble(ix), Some((mantissa, exp)))

    let ix = 0.75f64;
    let mantissa = BigInt::from_uint(3) << 51;
    let exp = -53;
    assert_eq!(float_disassemble(ix), Some((mantissa, exp)))
}
