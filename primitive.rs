#[deriving(Eq)]
pub enum PFunc {
    PEval,
    PAdd,
    PSub,
    PMul,
    PDiv,
}

pub fn prelude() -> ~[(~str, PFunc)] {
    do vec::map([PEval, PAdd, PSub, PMul, PDiv]) |prim| {
        (to_str(prim), *prim)
    }
}

pub fn to_str(&prim: &PFunc) -> ~str {
    match prim {
        PEval => ~"eval",
        PAdd => ~"+",
        PSub => ~"-",
        PMul => ~"*",
        PDiv => ~"/",
    }
}

impl to_str::ToStr for PFunc {
    fn to_str(&self) -> ~str {
        to_str(self)
    }
}
