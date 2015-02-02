use std::collections::HashMap;
use std::string::CowString;
use std::borrow::Cow;

use compiler::{EnvVar, Syntax};
use primitive::libprimitive;

/// Compiles the global env from `base`
pub fn libbase() -> HashMap<CowString<'static>, EnvVar> {
    let mut lib = HashMap::new();
    for &(name, func) in libprimitive().iter() {
        lib.insert(Cow::Borrowed(name), EnvVar::PrimFunc(name, func));
    }
    lib.insert(Cow::Borrowed("lambda"), EnvVar::Syntax(Syntax::Lambda));
    lib.insert(Cow::Borrowed("if"), EnvVar::Syntax(Syntax::If));
    lib.insert(Cow::Borrowed("let"), EnvVar::Syntax(Syntax::Let));
    lib.insert(Cow::Borrowed("set!"), EnvVar::Syntax(Syntax::Set));
    return lib;
}
