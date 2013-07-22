use std::num::{Bounded, IntConvertible};

pub struct BoundedIterator {
    priv current: int,
    priv max: int,
}

impl BoundedIterator {
    pub fn new<T: Bounded + IntConvertible>() -> BoundedIterator {
        let min:T = Bounded::min_value();
        let max:T = Bounded::max_value();
        BoundedIterator {
            current: min.to_int(),
            max: max.to_int(),
        }
    }
}

impl<T: IntConvertible> Iterator<T> for BoundedIterator {
    fn next(&mut self) -> Option<T> {
        if self.current <= self.max {
            let x = IntConvertible::from_int(self.current);
            self.current += 1;
            Some(x)
        } else {
            None
        }
    }
}
