use std::num::IntConvertible;
use std::cast;

#[deriving(Eq)]
pub enum PFunc {
    PEval,
    PApply,
    PMap,
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
    PInexactExact,
    PNumberString,
    PEQ,
    PGT,
    PLT,
    PGE,
    PLE,
    PZero,
    PPositive,
    PNegative,
    PNot,
    PBoolean,
    PChar,
    PCharAlphabetic,
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
    PCharInteger,
    PIntegerChar,
    PCharUpcase,
    PCharDowncase,
    PProcedure,
    PIsVector,
    PMakeVector,
    PVector,
    PVectorLength,
    PVectorRef,
    PVectorSet,
    PVectorFill,
    PVectorList,
    PListVector,
    PNull,
    PPair,
    PIsString,
    PStringEQ,
    PStringGT,
    PStringLT,
    PStringGE,
    PStringLE,
    PStringCIEQ,
    PStringCIGT,
    PStringCILT,
    PStringCIGE,
    PStringCILE,
    PString,
    PStringLength,
    PStringRef,
    PSubstring,
    PStringAppend,
    PStringList,
    PListString,
    PSymbol,
    PStringSymbol,
    PSymbolString,
    PInputPort,
    POutputPort,
    PCurrentInputPort,
    PCurrentOutputPort,
    POpenInputFile,
    POpenOutputFile,
    PCloseInputPort,
    PCloseOutputPort,
    PRead,
    PReadChar,
    PWrite,
    PDisplay,
    PNewline,
    PWriteChar,
    PLoad,
}

impl Bounded for PFunc {
    #[inline]
    fn min_value() -> PFunc { PEval }

    #[inline]
    fn max_value() -> PFunc { PLoad }
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

pub fn proc_to_str(&prim: &PFunc) -> @str {
    match prim {
        PEval => @"eval",
        PApply => @"apply",
        PMap => @"map",
        PBegin => @"begin",
        PAdd => @"+",
        PSub => @"-",
        PMul => @"*",
        PDiv => @"/",
        PQuotient => @"quotient",
        PRemainder => @"remainder",
        PModulo => @"modulo",
        PNumerator => @"numerator",
        PDenominator => @"denominator",
        PFloor => @"floor",
        PCeiling => @"ceiling",
        PRound => @"round",
        PTruncate => @"truncate",
        PExp => @"exp",
        PLog => @"log",
        PSin => @"sin",
        PCos => @"cos",
        PTan => @"tan",
        PAsin => @"asin",
        PAcos => @"acos",
        PAtan => @"atan",
        PSqrt => @"sqrt",
        PExpt => @"expt",
        PMakeRectangular => @"make-rectangular",
        PMakePolar => @"make-polar",
        PRealPart => @"real-part",
        PImagPart => @"imag-part",
        PMagnitude => @"magnitude",
        PAngle => @"angle",
        PCar => @"car",
        PCdr => @"cdr",
        PCons => @"cons",
        PEqv => @"eqv?",
        PEqual => @"equal?",
        PNumber => @"number?",
        PReal => @"real?",
        PInteger => @"integer?",
        PExact => @"exact?",
        PInexact => @"inexact?",
        PExactInexact => @"exact->inexact",
        PInexactExact => @"inexact->exact",
        PNumberString => @"number->string",
        PEQ => @"=",
        PGT => @">",
        PLT => @"<",
        PGE => @">=",
        PLE => @"<=",
        PZero => @"zero?",
        PPositive => @"positive?",
        PNegative => @"negative?",
        PNot => @"not",
        PBoolean => @"boolean?",
        PChar => @"char?",
        PCharAlphabetic => @"char-alphabetic?",
        PCharNumeric => @"char-numeric?",
        PCharWhitespace => @"char-whitespace?",
        PCharUpperCase => @"char-upper-case?",
        PCharLowerCase => @"char-lower-case?",
        PCharEQ => @"char=?",
        PCharGT => @"char>?",
        PCharLT => @"char<?",
        PCharGE => @"char>=?",
        PCharLE => @"char<=?",
        PCharCIEQ => @"char-ci=?",
        PCharCIGT => @"char-ci>?",
        PCharCILT => @"char-ci<?",
        PCharCIGE => @"char-ci>=?",
        PCharCILE => @"char-ci<=?",
        PCharInteger => @"char->integer",
        PIntegerChar => @"integer->char",
        PCharUpcase => @"char-upcase",
        PCharDowncase => @"char-downcase",
        PProcedure => @"procedure?",
        PIsVector => @"vector?",
        PMakeVector => @"make-vector",
        PVector => @"vector",
        PVectorLength => @"vector-length",
        PVectorRef => @"vector-ref",
        PVectorSet => @"vector-set!",
        PVectorFill => @"vector-fill!",
        PVectorList => @"vector->list",
        PListVector => @"list->vector",
        PNull => @"null?",
        PPair => @"pair?",
        PIsString => @"string?",
        PString => @"string",
        PStringEQ => @"string=?",
        PStringGT => @"string>?",
        PStringLT => @"string<?",
        PStringGE => @"string>=?",
        PStringLE => @"string<=?",
        PStringCIEQ => @"string-ci=?",
        PStringCIGT => @"string-ci>?",
        PStringCILT => @"string-ci<?",
        PStringCIGE => @"string-ci>=?",
        PStringCILE => @"string-ci<=?",
        PStringLength => @"string-length",
        PStringRef => @"string-ref",
        PSubstring => @"substring",
        PStringAppend => @"string-append",
        PStringList => @"string->list",
        PListString => @"list->string",
        PSymbol => @"symbol?",
        PStringSymbol => @"string->symbol",
        PSymbolString => @"symbol->string",
        PInputPort => @"input-port?",
        POutputPort => @"output-port?",
        PCurrentInputPort => @"current-input-port",
        PCurrentOutputPort => @"current-output-port",
        POpenInputFile => @"open-input-file",
        POpenOutputFile => @"open-output-file",
        PCloseInputPort => @"close-input-port",
        PCloseOutputPort => @"close-output-port",
        PRead => @"read",
        PReadChar => @"read-char",
        PWrite => @"write",
        PDisplay => @"display",
        PNewline => @"newline",
        PWriteChar => @"write-char",
        PLoad => @"load",
    }
}

impl ToStr for PFunc {
    fn to_str(&self) -> ~str {
        proc_to_str(self).to_owned()
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

pub fn syntax_to_str(&prim: &PrimSyntax) -> @str {
    match prim {
        SynIf => @"if",
        SynCond => @"cond",
        SynLambda => @"lambda",
        SynLet => @"let",
        SynLetRec => @"letrec",
        SynLetStar => @"let*",
        SynDefine => @"define",
        SynSet => @"set!",
        SynQuote => @"quote",
        SynQQuote => @"quasiquote",
        SynUnquote => @"unquote",
        SynAnd => @"and",
        SynOr => @"or",
    }
}

impl ToStr for PrimSyntax {
    fn to_str(&self) -> ~str {
        syntax_to_str(self).to_owned()
    }
}
