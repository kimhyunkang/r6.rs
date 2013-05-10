#[deriving(Eq)]
pub enum PFunc {
    PEval,
    PAdd,
    PSub,
    PMul,
    PDiv,
    PCar,
    PCdr,
}

pub fn prelude() -> ~[(@str, PFunc)] {
    do vec::map([PEval, PAdd, PSub, PMul, PDiv, PCar, PCdr]) |prim| {
        (proc_to_str(prim).to_managed(), *prim)
    }
}

pub fn proc_to_str(&prim: &PFunc) -> ~str {
    match prim {
        PEval => ~"eval",
        PAdd => ~"+",
        PSub => ~"-",
        PMul => ~"*",
        PDiv => ~"/",
        PCar => ~"car",
        PCdr => ~"cdr",
    }
}

impl to_str::ToStr for PFunc {
    fn to_str(&self) -> ~str {
        proc_to_str(self)
    }
}

#[deriving(Eq)]
pub enum PrimSyntax {
    SynIf,
    SynLambda,
    SynQuote,
}

pub fn syntax_prelude() -> ~[(@str, PrimSyntax)] {
    do vec::map([SynIf, SynLambda, SynQuote]) |syn| {
        (syntax_to_str(syn).to_managed(), *syn)
    }
}

pub fn syntax_to_str(&prim: &PrimSyntax) -> ~str {
    match prim {
        SynIf => ~"if",
        SynLambda => ~"lambda",
        SynQuote => ~"quote",
    }
}

impl to_str::ToStr for PrimSyntax {
    fn to_str(&self) -> ~str {
        syntax_to_str(self)
    }
}
