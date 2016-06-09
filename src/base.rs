use std::collections::HashMap;
use std::borrow::Cow;

use compiler::{EnvVar, Syntax};
use primitive::libprimitive;
use runtime::PrimFuncPtr;

/// Compiles the global env from `base`
pub fn libbase() -> HashMap<Cow<'static, str>, EnvVar> {
    let mut lib = HashMap::new();
    for &(name, func) in libprimitive().iter() {
        lib.insert(Cow::Borrowed(name), EnvVar::PrimFunc(PrimFuncPtr::new(name, func)));
    }

    for syn in Syntax::iter() {
        lib.insert(Cow::Borrowed(syn.name()), EnvVar::Syntax(syn));
    }

    return lib;
}
