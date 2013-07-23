use std::num::IntConvertible;
use std::cast;

#[deriving(Eq)]
pub enum PFunc {
    PEval,
    PApply,
    PBegin,
    PAdd,
    PSub,
    PMul,
    PDiv,
    PQuotient,
    PRemainder,
    PModulo,
    PNumerator,
    PDenominator,
    PFloor,
    PCeiling,
    PRound,
    PTruncate,
    PExp,
    PLog,
    PSin,
    PCos,
    PTan,
    PAsin,
    PAcos,
    PAtan,
    PSqrt,
    PExpt,
    PMakeRectangular,
    PMakePolar,
    PRealPart,
    PImagPart,
    PMagnitude,
    PAngle,
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
    PExactInexact,
    PNumberString,
    PEQ,
    PGT,
    PLT,
    PGE,
    PLE,
    PNot,
    PBoolean,
    PChar,
    PCharAlphabetical,
    PCharNumeric,
    PCharWhitespace,
    PCharUpperCase,
    PCharLowerCase,
    PCharEQ,
    PCharGT,
    PCharLT,
    PCharGE,
    PCharLE,
    PCharCIEQ,
    PCharCIGT,
    PCharCILT,
    PCharCIGE,
    PCharCILE,
    PProcedure,
    PIsVector,
    PMakeVector,
    PVector,
    PVectorLength,
    PVectorRef,
    PVectorList,
    PListVector,
    PNull,
    PPair,
    PIsString,
    PString,
    PStringLength,
    PStringRef,
    PSubstring,
    PSymbol,
    PStringSymbol,
    PSymbolString,
}

impl Bounded for PFunc {
    #[inline]
    fn min_value() -> PFunc { PEval }

    #[inline]
    fn max_value() -> PFunc { PSymbolString }
}

impl IntConvertible for PFunc {
    #[inline]
    fn to_int(&self) -> int {
        *self as int
    }

    #[inline]
    fn from_int(n: int) -> PFunc {
        unsafe { cast::transmute(n) }
    }
}

pub fn proc_to_str(&prim: &PFunc) -> ~str {
    match prim {
        PEval => ~"eval",
        PApply => ~"apply",
        PBegin => ~"begin",
        PAdd => ~"+",
        PSub => ~"-",
        PMul => ~"*",
        PDiv => ~"/",
        PQuotient => ~"quotient",
        PRemainder => ~"remainder",
        PModulo => ~"modulo",
        PNumerator => ~"numerator",
        PDenominator => ~"denominator",
        PFloor => ~"floor",
        PCeiling => ~"ceiling",
        PRound => ~"round",
        PTruncate => ~"truncate",
        PExp => ~"exp",
        PLog => ~"log",
        PSin => ~"sin",
        PCos => ~"cos",
        PTan => ~"tan",
        PAsin => ~"asin",
        PAcos => ~"acos",
        PAtan => ~"atan",
        PSqrt => ~"sqrt",
        PExpt => ~"expt",
        PMakeRectangular => ~"make-rectangular",
        PMakePolar => ~"make-polar",
        PRealPart => ~"real-part",
        PImagPart => ~"imag-part",
        PMagnitude => ~"magnitude",
        PAngle => ~"angle",
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
        PExactInexact => ~"exact->inexact",
        PNumberString => ~"number->string",
        PEQ => ~"=",
        PGT => ~">",
        PLT => ~"<",
        PGE => ~">=",
        PLE => ~"<=",
        PNot => ~"not",
        PBoolean => ~"boolean?",
        PChar => ~"char?",
        PCharAlphabetical => ~"char-alphabetical?",
        PCharNumeric => ~"char-numeric?",
        PCharWhitespace => ~"char-whitespace?",
        PCharUpperCase => ~"char-upper-case?",
        PCharLowerCase => ~"char-lower-case?",
        PCharEQ => ~"char=?",
        PCharGT => ~"char>?",
        PCharLT => ~"char<?",
        PCharGE => ~"char>=?",
        PCharLE => ~"char<=?",
        PCharCIEQ => ~"char-ci=?",
        PCharCIGT => ~"char-ci>?",
        PCharCILT => ~"char-ci<?",
        PCharCIGE => ~"char-ci>=?",
        PCharCILE => ~"char-ci<=?",
        PProcedure => ~"procedure?",
        PIsVector => ~"vector?",
        PMakeVector => ~"make-vector",
        PVector => ~"vector",
        PVectorLength => ~"vector-length",
        PVectorRef => ~"vector-ref",
        PVectorList => ~"vector->list",
        PListVector => ~"list->vector",
        PNull => ~"null?",
        PPair => ~"pair?",
        PIsString => ~"string?",
        PString => ~"string",
        PStringLength => ~"string-length",
        PStringRef => ~"string-ref",
        PSubstring => ~"substring",
        PSymbol => ~"symbol?",
        PStringSymbol => ~"string->symbol",
        PSymbolString => ~"symbol->string",
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
    SynCond,
    SynLambda,
    SynLet,
    SynLetRec,
    SynLetStar,
    SynDefine,
    SynSet,
    SynQuote,
    SynQQuote,
    SynUnquote,
    SynAnd,
    SynOr,
}

impl Bounded for PrimSyntax {
    #[inline]
    fn min_value() -> PrimSyntax { SynIf }

    #[inline]
    fn max_value() -> PrimSyntax { SynOr }
}

impl IntConvertible for PrimSyntax {
    #[inline]
    fn to_int(&self) -> int {
        *self as int
    }

    #[inline]
    fn from_int(n: int) -> PrimSyntax {
        unsafe { cast::transmute(n) }
    }
}

pub fn syntax_to_str(&prim: &PrimSyntax) -> ~str {
    match prim {
        SynIf => ~"if",
        SynCond => ~"cond",
        SynLambda => ~"lambda",
        SynLet => ~"let",
        SynLetRec => ~"letrec",
        SynLetStar => ~"let*",
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
