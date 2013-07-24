use std::num::{Zero, One, FromStrRadix, ToStrRadix};
use std::cmp::min;
use extra::bigint::BigInt;

pub fn big_pow(radix: &BigInt, pow: uint) -> BigInt {
    let _0: BigInt = Zero::zero();
    let _1: BigInt = One::one();

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
