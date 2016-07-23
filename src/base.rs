use std::cell::RefCell;
use std::collections::HashMap;
use std::borrow::Cow;
use std::rc::Rc;

use compiler::Syntax;
use datum::Datum;
use primitive::libprimitive;
use runtime::{DatumType, Inst, PrimFuncPtr, RuntimeData, Closure, RDatum};

/// Compiles the global env from `base`
pub fn base_syntax() -> HashMap<Cow<'static, str>, Syntax> {
    let mut lib = HashMap::new();

    for syn in Syntax::iter() {
        lib.insert(Cow::Borrowed(syn.name()), syn);
    }

    return lib;
}

pub fn static_closure(bytecode: Vec<Inst>) -> Rc<RefCell<RDatum>> {
    Rc::new(RefCell::new(Datum::Ext(RuntimeData::Closure(Closure::new(Rc::new(bytecode), None, None)))))
}

pub fn libbase() -> HashMap<Cow<'static, str>, Rc<RefCell<RDatum>>> {
    let mut lib = HashMap::new();
    for &(name, func) in libprimitive().iter() {
        lib.insert(Cow::Borrowed(name), Rc::new(RefCell::new(Datum::Ext(RuntimeData::PrimFunc(PrimFuncPtr::new(name, func))))));
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
        Inst::Return
    ];

    let eqv: Vec<Inst> = vec![
        Inst::Eqv,
        Inst::Return
    ];

    let eq: Vec<Inst> = vec![
        Inst::Eqv,
        Inst::Return
    ];

    let equal: Vec<Inst> = vec![
        Inst::Equal,
        Inst::Return
    ];

    lib.insert(Cow::Borrowed("apply"), static_closure(apply));
    lib.insert(Cow::Borrowed("eqv?"), static_closure(eqv));
    lib.insert(Cow::Borrowed("eq?"), static_closure(eq));
    lib.insert(Cow::Borrowed("equal?"), static_closure(equal));

    return lib;
}
