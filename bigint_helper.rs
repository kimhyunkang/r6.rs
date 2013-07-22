use std::num::{Zero, One};
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

#[inline]
pub fn bigint_to_f64(n: &BigInt) -> f64 {
    FromStr::from_str::<f64>(n.to_str()).unwrap()
}
