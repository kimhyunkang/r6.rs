use std::collections::HashMap;
use std::string::CowString;
use std::borrow::Cow;

use compiler::{EnvVar, Syntax};
use primitive::libprimitive;

pub fn libbase() -> HashMap<CowString<'static>, EnvVar> {
    let mut lib = HashMap::new();
    for &(name, ref func) in libprimitive().iter() {
        lib.insert(Cow::Borrowed(name), EnvVar::PrimFunc(name, func.clone()));
    }
    lib.insert(Cow::Borrowed("lambda"), EnvVar::Syntax(Syntax::Lambda));
    return lib;
}
