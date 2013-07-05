#[cfg(test)]
use std::io;

use std::vec;
use datum::*;

#[cfg(test)]
use parser::*;

pub struct PatternCompiler {
    keywords: ~[@str],
    vars: ~[@str],
}

#[deriving(Eq)]
pub enum Pattern<T> {
    PAny,
    PVar(@str),
    PList(~[~Pattern<T>], Option<(~Pattern<T>, ~[~Pattern<T>])>, Option<~Pattern<T>>),
    PDatum(@LDatum<T>),
}

impl PatternCompiler {
    fn compile_before<T>(&mut self,
                        last_pat: ~Pattern<T>,
                        sum: ~[~Pattern<T>],
                        code: &@LDatum<T>) -> Result<~Pattern<T>, ~str>
    {
        match *code {
            @LCons(h, t) =>
                match h {
                    @LIdent(name) if name == @"..." => match self.compile_after(&t) {
                        Ok((after, end)) => Ok(~PList(sum, Some((last_pat, after)), end)),
                        Err(e) => Err(e),
                    },
                    _ => match self.compile_val(&h) {
                        Ok(pat) => 
                            self.compile_before(pat, vec::append_one(sum, last_pat), &t),
                        Err(e) => Err(e),
                    },
                },
            @LNil => {
                Ok(~PList(vec::append_one(sum, last_pat), None, None))
            },
            _ => match self.compile_val(code) {
                Ok(pat) =>
                    Ok(~PList(vec::append_one(sum, last_pat), None, Some(pat))),
                Err(e) => Err(e),
            },
        }
    }

    fn compile_after<T>(&mut self,
                    code: &@LDatum<T>) -> Result<(~[~Pattern<T>], Option<~Pattern<T>>), ~str>
    {
        let mut sum: ~[~Pattern<T>] = ~[];
        let mut iter = *code;

        loop {
            match iter {
                @LCons(h, t) => {
                    match self.compile_val(&h) {
                        Err(e) => return Err(e),
                        Ok(pat) => sum.push(pat),
                    }
                    iter = t;
                },
                @LNil => 
                    return Ok((sum, None)),
                _ => match self.compile_val(&iter) {
                    Ok(pat) => return Ok((sum, Some(pat))),
                    Err(e) => return Err(e),
                },
            }
        }
    }

    fn compile_val<T>(&mut self, code: &@LDatum<T>) -> Result<~Pattern<T>, ~str> {
        match **code {
            LCons(h, t) => {
                do self.compile_val(&h).chain |last_pat| {
                    self.compile_before(last_pat, ~[], &t)
                }
            },
            LNil => {
                Ok(~PList(~[], None, None))
            },
            LIdent(name) => if name == @"..." {
                    Err(~"identifier ... is only valid in list context")
                } else if name == @"_" {
                    Ok(~PAny)
                } else if vec::contains(self.keywords, &name) {
                    Ok(~PDatum(*code))
                } else if vec::contains(self.vars, &name) {
                    Err(~"duplicate pattern variable")
                } else {
                    self.vars.push(name);
                    Ok(~PVar(name))
                },
            _ => Ok(~PDatum(*code)),
        }
    }

    pub fn new(keywords: ~[@str]) -> PatternCompiler {
        PatternCompiler {
            keywords: keywords,
            vars: ~[],
        }
    }

    pub fn compile<T>(&mut self, code: &@LDatum<T>) -> Result<~Pattern<T>, ~str> {
        self.vars = ~[];
        self.compile_val(code)
    }
}

#[test]
fn pattern_compiler_test() {
    let keywords = ~[@"else", @"=>"];
    let mut compiler = PatternCompiler::new(keywords);
    let src = ~"(_ (else r1 r2 ...))";
    let code: @LDatum<()> = do io::with_str_reader(src) |rdr| {
        let mut parser = Parser(rdr);
        match parser.parse() {
            Ok(v) => @v,
            Err(e) => fail!(e),
        }
    };
    let pattern: Result<~Pattern<()>, ~str> = compiler.compile(&code);
    let pat_before: ~[~Pattern<()>] = ~[~PDatum(@LIdent(@"else")), ~PVar(@"r1")];
    let pat_inner = ~PList(pat_before, Some((~PVar(@"r2"), ~[])), None);
    let expected = ~PList(~[~PAny, pat_inner], None, None);
    assert_eq!(pattern, Ok(expected));
}
