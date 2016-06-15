use std::collections::HashMap;
use std::borrow::Cow;
use std::rc::Rc;

use compiler::{EnvVar, Syntax};
use primitive::libprimitive;
use runtime::{DatumType, Inst, PrimFuncPtr};

/// Compiles the global env from `base`
pub fn libbase() -> HashMap<Cow<'static, str>, EnvVar> {
    let mut lib = HashMap::new();
    for &(name, func) in libprimitive().iter() {
        lib.insert(Cow::Borrowed(name), EnvVar::PrimFunc(PrimFuncPtr::new(name, func)));
    }

    let apply: Vec<Inst> = vec![
        // 0
        Inst::Type(DatumType::Pair),
        Inst::JumpIfFalse(5),
        Inst::DropArg,
        Inst::Uncons,
        Inst::Jump(0),
        // 5
        Inst::DropArg,
        Inst::Type(DatumType::Null),
        Inst::ThrowIfFalse("apply: non-list argument"),
        Inst::DropArg,
        Inst::DropArg,
        Inst::CallSplicing,
        Inst::SetArgSize(0),
        Inst::Return
    ];

    let eqv: Vec<Inst> = vec![
        Inst::Eqv,
        Inst::Return
    ];

    let equal: Vec<Inst> = vec![
        Inst::Equal,
        Inst::Return
    ];

    lib.insert(Cow::Borrowed("apply"), EnvVar::Procedure(Rc::new(apply)));
    lib.insert(Cow::Borrowed("eqv?"), EnvVar::Procedure(Rc::new(eqv)));
    lib.insert(Cow::Borrowed("equal?"), EnvVar::Procedure(Rc::new(equal)));

    for syn in Syntax::iter() {
        lib.insert(Cow::Borrowed(syn.name()), EnvVar::Syntax(syn));
    }

    return lib;
}
