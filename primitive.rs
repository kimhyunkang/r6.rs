#[deriving(Eq)]
pub enum PFunc {
    PEval,
    PBegin,
    PAdd,
    PSub,
    PMul,
    PDiv,
    PQuotient,
    PRemainder,
    PModulo,
    PCar,
    PCdr,
    PCons,
    PEqv,
    PEqual,
    PNumber,
    PReal,
    PInteger,
    PExact,
    PInexact,
    PEQ,
    PGT,
    PLT,
    PGE,
    PLE,
    PNot,
    PNull,
    PPair,
    PIsString,
    PString,
}

pub fn prelude() -> ~[(@str, PFunc)] {
    do [PEval, PBegin, PAdd, PSub, PMul, PDiv,
        PQuotient, PRemainder, PModulo,
        PCar, PCdr, PCons,
        PEqv, PEqual,
        PNumber, PReal, PInteger,
        PExact, PInexact,
        PEQ, PGT, PLT, PGE, PLE, PNot,
        PNull, PPair,
        PIsString, PString].map |prim| {
        (proc_to_str(prim).to_managed(), *prim)
    }
}

pub fn proc_to_str(&prim: &PFunc) -> ~str {
    match prim {
        PEval => ~"eval",
        PBegin => ~"begin",
        PAdd => ~"+",
        PSub => ~"-",
        PMul => ~"*",
        PDiv => ~"/",
        PQuotient => ~"quotient",
        PRemainder => ~"remainder",
        PModulo => ~"modulo",
        PCar => ~"car",
        PCdr => ~"cdr",
        PCons => ~"cons",
        PEqv => ~"eqv?",
        PEqual => ~"equal?",
        PNumber => ~"number?",
        PReal => ~"real?",
        PInteger => ~"integer?",
        PExact => ~"exact?",
        PInexact => ~"inexact?",
        PEQ => ~"=",
        PGT => ~">",
        PLT => ~"<",
        PGE => ~">=",
        PLE => ~"<=",
        PNot => ~"not",
        PNull => ~"null?",
        PPair => ~"pair?",
        PIsString => ~"string?",
        PString => ~"string",
    }
}

impl ToStr for PFunc {
    fn to_str(&self) -> ~str {
        proc_to_str(self)
    }
}

#[deriving(Eq)]
pub enum PrimSyntax {
    SynIf,
    SynLambda,
    SynLet,
    SynDefine,
    SynSet,
    SynQuote,
    SynQQuote,
    SynUnquote,
    SynAnd,
    SynOr,
}

pub fn syntax_prelude() -> ~[(@str, PrimSyntax)] {
    do [SynIf, SynLambda, SynLet, SynDefine, SynSet,
        SynQuote, SynQQuote, SynUnquote,
        SynAnd, SynOr].map |syn| {
        (syntax_to_str(syn).to_managed(), *syn)
    }
}

pub fn syntax_to_str(&prim: &PrimSyntax) -> ~str {
    match prim {
        SynIf => ~"if",
        SynLambda => ~"lambda",
        SynLet => ~"let",
        SynDefine => ~"define",
        SynSet => ~"set!",
        SynQuote => ~"quote",
        SynQQuote => ~"quasiquote",
        SynUnquote => ~"unquote",
        SynAnd => ~"and",
        SynOr => ~"or",
    }
}

impl ToStr for PrimSyntax {
    fn to_str(&self) -> ~str {
        syntax_to_str(self)
    }
}
