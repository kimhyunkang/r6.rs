use std::num;
use std::num::{Zero, One, FromStrRadix, FromPrimitive};
use std::rc::Rc;
use std::from_str::FromStr;
use std::io::{BufferedReader, IoError, EndOfFile};
use num::bigint::BigInt;
use num::rational::{Ratio, BigRational};

use bigint_helper::{pow_uint, bigint_to_float};
use numeric::{LNumeric, NReal, inexact, exact};
use real::{LReal, Rf64};
use datum::*;

#[cfg(test)]
use numeric::{from_int, from_rational};
#[cfg(test)]
use std::io;

enum PResult {
    PFloat(String, String, String),
    PRational(String, String),
    PInt(String),
}

enum NilRes<T, E> {
    NNone,
    NErr(E),
    NSome(T),
}

enum DResult<T> {
    DSingle,
    DDatum(LDatum<T>),
    DErr(String),
}

#[cfg(test)]
fn with_str_reader<T>(s: &str, f: |io::MemReader| -> T) -> T {
    let m = io::MemReader::new(Vec::from_slice(s.as_bytes()));
    f(m)
}

fn build_exact(sign: bool, radix: uint, part: &PResult) -> Result<BigRational, String> {
    match part {
        &PInt(ref zs) =>
            Ok(Ratio::new(s_to_int(zs.as_slice(), sign, radix), One::one())),
        &PRational(ref ns, ref ds) => {
            let n:BigInt = s_to_int(ns.as_slice(), true, radix);
            let d:BigInt = s_to_int(ds.as_slice(), sign, radix);
            if n.is_zero() {
                Err("divide by zero".to_string())
            } else {
                Ok(Ratio::new(n, d))
            }
        },
        &PFloat(ref i, ref f, ref e) => {
            let exp:int =
            if e.is_empty() {
                0
            } else {
                s_to_int(e.as_slice(), true, 10)
            };

            let _10:BigInt = FromPrimitive::from_uint(10).unwrap();
            let d = pow_uint(&_10, f.len());
            let i_f = format!("{}{}", i, f);
            let n = s_to_int(i_f.as_slice(), sign, 10);
            let p = pow_uint(&_10, num::abs(exp) as uint);

            if d.is_zero() {
                Err("divide by zero".to_string())
            } else if exp < 0 {
                Ok(Ratio::new(n, d*p))
            } else {
                Ok(Ratio::new(n*p, d))
            }
        },
    }
}

fn build_inexact(sign: bool, radix: uint, part: &PResult) -> Result<f64, String> {
    match part {
        &PInt(ref zs) => 
            Ok(bigint_to_float(&s_to_int(zs.as_slice(), sign, radix))),
        &PRational(ref ds, ref ns) => {
            let d:BigInt = s_to_int(ds.as_slice(), sign, radix);
            let n:BigInt = s_to_int(ns.as_slice(), sign, radix);
            if n.is_zero() {
                Err("divide by zero".to_string())
            } else {
                Ok(bigint_to_float::<f64>(&d) / bigint_to_float::<f64>(&n))
            }
        },
        &PFloat(ref i, ref f, ref e) => {
            let s = if e.is_empty() {
                format!("{}.{}", i, f)
            } else {
                format!("{}.{}e{}", i, f, e)
            };
            Ok(s_to_f64(s.as_slice(), sign))
        }
    }
}

fn is_pfloat(pres: &PResult) -> bool {
    match pres {
        &PFloat(_, _, _) => true,
        _ => false,
    }
}

fn build_real(exactness: Option<bool>, radix: uint, rsign: bool, rpart: &PResult)
    -> Result<LReal, String>
{
    match (exactness, rpart) {
        (Some(false), _) | (None, &PFloat(_, _, _)) =>
            build_inexact(rsign, radix, rpart).map(|f| { Rf64(f) }),
        _ =>
            build_exact(rsign, radix, rpart).map(LReal::from_rational),
    }
}

fn build_complex(exactness: Option<bool>,
                    radix: uint,
                    rsign: bool,
                    rpart: &PResult,
                    isign: bool,
                    ipart: &PResult) -> Result<LNumeric, String> {
    if exactness == Some(true) || 
            (exactness == None && !is_pfloat(rpart) && !is_pfloat(ipart)) {
        build_exact(rsign, radix, rpart).and_then(|re| {
            match *ipart {
                PInt(ref s) if s.as_slice() == "0" =>
                    Ok(NReal(LReal::from_rational(re))),
                PRational(ref s, _) if s.as_slice() == "0" =>
                    Ok(NReal(LReal::from_rational(re))),
                PFloat(ref s0, ref s1, _) if s0.as_slice() == "0" && s1.as_slice() == "0" =>
                    Ok(NReal(LReal::from_rational(re))),
                _ => build_exact(isign, radix, ipart).and_then(|im| {
                    Ok(exact(re.clone(), im.clone()))
                }),
            }
        })
    } else {
        build_inexact(rsign, radix, rpart).and_then(|re| {
            build_inexact(isign, radix, ipart).map(|im| {
                inexact(re, im)
            })
        })
    }
}

fn s_to_f64(s: &str, sign: bool) -> f64 {
    let maybe_f64:Option<f64> = FromStr::from_str(s);
    match maybe_f64 {
        Some(f) => if sign { f } else { -f },
        None => fail!("failed to convert float literal: {}", s),
    }
}

fn s_to_int<T: FromStrRadix + Neg<T>>(s: &str, sign: bool, r: uint) -> T {
    let maybe_int:Option<T> = FromStrRadix::from_str_radix(s, r);
    match maybe_int {
        Some(z) => if sign { z } else { -z },
        None => fail!("failed to convert int literal {} with radix ", r),
    }
}

pub struct Parser<R> {
    reader: BufferedReader<R>,
    buf: Option<char>,
    pub line: uint,
    pub col: uint,
}

fn id_init(c: char) -> bool {
    match c {
        '!' | '$' | '%' | '&' | '*' | '/' | ':' |
        '<' | '=' | '>' | '?' | '^' | '_' | '~' |
        'a' ... 'z' | 'A' ... 'Z' => true,
        _ => false,
    }
}

impl<R:Reader> Parser<R> {
    pub fn new(reader: R) -> Parser<R> {
        Parser {
            reader: BufferedReader::new(reader),
            buf: None,
            line: 1,
            col: 1,
        }
    }

    pub fn parse<T>(&mut self) -> Result<LDatum<T>, String> {
        self.parse_datum().and_then(|v| {
            self.consume_whitespace();

            match self.lookahead() {
                Ok(_) => Err("trailing input".to_string()),
                Err(e) => if e.kind == EndOfFile {
                    Ok(v)
                } else {
                    Err(e.to_string())
                }
            }
        })
    }

    pub fn parse_datum<T>(&mut self) -> Result<LDatum<T>, String> {
        self.consume_whitespace();

        let c = try!(self.lookahead().map_err(|e| e.to_string()));
        match c {
            _ if id_init(c) =>
                Ok(LIdent(self.parse_ident())),
            '"' =>
                self.parse_string().map(|s| LString(s) ),
            '#' => {
                    self.consume_nil();
                    self.parse_sharp()
                },
            '0'...'9' =>
                self.parse_number("").map(|n| LNum(n) ),
            '(' | '[' => {
                    let delim = if c == '(' { ')' } else { ']' };
                    self.consume_nil();
                    self.consume_whitespace();
                    match self.try_consume([delim]) {
                        Some(_) => Ok(LNil),
                        None => match self.parse_datum() {
                            Err(e) => Err(e),
                            Ok(head) => self.parse_list(Rc::new(head), delim),
                        },
                    }
                },
            '+' | '-' => {
                self.consume_nil();
                match self.lookahead() {
                    Ok(c0) => match c0 {
                        '0' ... '9' | 'i' | '.' =>
                            self.parse_number(c.to_string().as_slice()).map(|n| LNum(n) ),
                        _ => {
                            println!("{:c}", c);
                            Ok(LIdent(c.to_string()))
                        },
                    },
                    Err(e) => if e.kind == EndOfFile {
                        Ok(LIdent(c.to_string()))
                    } else {
                        Err(e.to_string())
                    }
                }
            },
            '.' => {
                self.consume_nil();
                match self.parse_dot() {
                    DSingle => Err("invalid token '.' in data context".to_string()),
                    DDatum(x) => Ok(x),
                    DErr(e) => Err(e),
                }
            },
            '\'' => {
                self.consume_nil();
                self.parse_datum().map(|v| {
                    LCons(Rc::new(LIdent("quote".to_string())), Rc::new(LCons(Rc::new(v), Rc::new(LNil))))
                })
            },
            '`' => {
                self.consume_nil();
                self.parse_datum().map(|v| {
                    LCons(Rc::new(LIdent("quasiquote".to_string())), Rc::new(LCons(Rc::new(v), Rc::new(LNil))))
                })
            },
            ',' => {
                self.consume_nil();
                match self.try_consume(['@']) {
                    Some(_) => self.parse_datum().map(|v| {
                        LCons(Rc::new(LIdent("unquote-splicing".to_string())), Rc::new(LCons(Rc::new(v), Rc::new(LNil))))
                    }),
                    _ => self.parse_datum().map(|v| {
                        LCons(Rc::new(LIdent("unquote".to_string())), Rc::new(LCons(Rc::new(v), Rc::new(LNil))))
                    }),
                }
            },
            _ =>
                Err(format!("unexpected character: {:c}", c)),
        }
    }

    #[allow(unused_must_use)]
    fn consume_nil(&mut self) {
        self.consume();
    }

    fn consume(&mut self) -> Result<char, IoError> {
        let c =
        match self.buf {
            None => self.reader.read_char(),
            Some(x) => {
                self.buf = None;
                Ok(x)
            },
        };

        match c {
            Ok('\n') => {
                self.line += 1;
                self.col = 1;
            },
            Ok(_) => {
                self.col += 1;
            },
            Err(_) => ()
        };

        c
    }

    fn lookahead(&mut self) -> Result<char, IoError> {
        match self.buf {
            None => {
                let c = try!(self.reader.read_char());
                self.buf = Some(c);
                Ok(c)
            },
            Some(c) => Ok(c),
        }
    }

    fn try_consume(&mut self, v: &[char]) -> Option<char> {
        let c =
        match self.buf {
            None => match self.reader.read_char() {
                Ok(x) => {
                    self.buf = Some(x);
                    x
                },
                Err(_) => return None
            },
            Some(x) => x
        };

        if v.contains(&c) {
            self.buf = None;
            if c == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            Some(c)
        } else {
            None
        }
    }

    fn consume_whitespace(&mut self) -> bool {
        let mut consumed = false;
        loop {
            match self.try_consume([' ', '\t', '\r', '\n', ';']) {
                Some(';') => {
                    consumed = true;
                    loop {
                        match self.consume() {
                            Ok('\n') => break,
                            Ok(_) => (),
                            Err(_) => break
                        }
                    }
                },
                Some(_) => {
                    consumed = true;
                },
                None => break,
            }
        }

        consumed
    }

    fn parse_sharp<T>(&mut self) -> Result<LDatum<T>, String> {
        let c = try!(self.consume().map_err(|e| e.to_string()));
        match c {
            '\\' => self.parse_char().map(LChar),
            't' =>
                Ok(LBool(true)),
            'f' =>
                Ok(LBool(false)),
            'e' | 'i' | 'b' | 'o' | 'd' | 'x' => 
                self.parse_number(format!("#{:c}", c).as_slice()).map(|n| {
                    LNum(n)
                }),
            '(' | '[' => {
                let delim = if c == '(' { ')' } else { ']' };
                match self.parse_vector(delim) {
                    Ok(v) => Ok(LVector(Rc::new(v))),
                    Err(e) => Err(e),
                }
            },
            _ => Err(format!("unexpected character: {:c}", c)),
        }
    }

    fn parse_vector<T>(&mut self, delim: char) -> Result<Vec<Rc<LDatum<T>>>, String> {
        let mut vec = vec![];

        loop {
            self.consume_whitespace();

            match self.lookahead() {
                Ok(e) if e == delim => {
                    self.consume_nil();
                    return Ok(vec)
                },
                Ok(_) => {
                    let x = try!(self.parse_datum());
                    vec.push(Rc::new(x))
                },
                Err(e) => if e.kind == EndOfFile {
                    return Err("parenthesis not closed".to_string())
                } else {
                    return Err(e.to_string())
                }
            }
        }
    }

    fn parse_list<T>(&mut self, head: Rc<LDatum<T>>, delim: char) -> Result<LDatum<T>, String> {
        self.consume_whitespace();
        match self.lookahead() {
            Ok(c) if c == delim => {
                self.consume_nil();
                Ok(LCons(head, Rc::new(LNil)))
            },
            Ok('.') => {
                self.consume_nil();
                match self.parse_dot() {
                    DSingle => 
                        match self.parse_datum() {
                            Err(e) => Err(e),
                            Ok(tail) => {
                                self.consume_whitespace();
                                match self.try_consume([delim]) {
                                    None => Err("RPAREN not found after DOT".to_string()),
                                    Some(_) => Ok(LCons(head, Rc::new(tail))),
                                }
                            },
                        },
                    DDatum(tail1) => {
                        match self.parse_list(Rc::new(tail1), delim) {
                            Ok(tail) => Ok(LCons(head, Rc::new(tail))),
                            Err(e) => Err(e),
                        }
                    },
                    DErr(e) => Err(e),
                }
            },
            Err(e) => if e.kind == EndOfFile {
                    Err("parenthesis not closed".to_string())
                } else {
                    Err(e.to_string())
            },
            _ => match self.parse_datum() {
                Err(e) => Err(e),
                Ok(tail1) => {
                    match self.parse_list(Rc::new(tail1), delim) {
                        Ok(tail) => Ok(LCons(head, Rc::new(tail))),
                        Err(e) => Err(e),
                    }
                },
            },
        }
    }

    fn parse_dot<T>(&mut self) -> DResult<T> {
        match self.lookahead() {
            Ok('0'...'9') => match self.parse_number(".") {
                Ok(x) => DDatum(LNum(x)),
                Err(e) => DErr(e),
            },
            Ok('.') => {
                self.consume_nil();
                match self.try_consume(['.']) {
                    None => DErr("invalid token '..'".to_string()),
                    _ => DDatum(LIdent("...".to_string())),
                }
            }
            _ => DSingle
        }
    }

    fn parse_char(&mut self) -> Result<char, String> {
        let mut lit = String::new();
        let c0 = try!(self.consume().map_err(|e| { e.to_string() }));
        lit.push_char(c0);

        loop {
            match self.lookahead() {
                Ok(c) => match c {
                    'a' ... 'z' => {
                        self.consume_nil();
                        lit.push_char(c);
                    },
                    _ => break
                },
                Err(e) => if e.kind == EndOfFile {
                    break;
                } else {
                    return Err(e.to_string())
                }
            }
        }

        if lit.len() == 1 {
            Ok(lit.shift_char().unwrap())
        } else if lit.as_slice() == "space" {
            Ok(' ')
        } else if lit.as_slice() == "newline" {
            Ok('\n')
        } else {
            Err(format!("unknown character literal {}", lit))
        }
    }

    fn parse_number(&mut self, init: &str) -> Result<LNumeric, String> {
        self.parse_num_prefix(init).and_then(|pref| {
            let (exactness, radix) = pref;
            let r = match radix { None => 10, Some(d) => d };

            let rsign =
            match init {
                "+" => true,
                "-" => false,
                _ => match self.try_consume(['+', '-']) {
                    Some('-') => false,
                    _ => true,
                },
            };

            match self.parse_real(r, init == ".") {
                NErr(e) => Err(e),
                NNone => 
                    match self.try_consume(['i']) {
                        Some(_) =>
                            build_complex(exactness, r, true, &PInt("0".to_string()), rsign, &PInt("1".to_string())),
                        None =>
                            Err("empty number literal".to_string()),
                    },
                NSome(rpart) =>
                    match self.try_consume(['i', '@', '+', '-']) {
                        Some('i') => 
                            build_complex(exactness, r, true, &PInt("0".to_string()), rsign, &rpart),
                        Some('@') => {
                            build_inexact(rsign, r, &rpart).and_then(|abs| {
                                let asign = match self.try_consume(['+', '-']) {
                                    Some('-') => false,
                                    _ => true,
                                };
                                match self.parse_real(r, false) {
                                    NSome(apart) =>
                                        build_inexact(asign, r, &apart).map(|arg| {
                                            let re = abs * arg.cos();
                                            let im = abs * arg.sin();
                                            inexact(re, im)
                                        }),
                                    _ => Err("invalid polar literal".to_string()),
                                }
                            })
                        },
                        Some(s) => {
                            let isign = s == '+';
                            let res =
                            match self.parse_real(r, false) {
                                NSome(ipart) =>
                                    build_complex(exactness, r, rsign, &rpart, isign, &ipart),
                                _ =>
                                    Err("invalid complex literal".to_string()),
                            };
                            match self.try_consume(['i']) {
                                Some(_) => res,
                                None => Err("invalid complex literal".to_string()),
                            }
                        },
                        None => build_real(exactness, r, rsign, &rpart).map(|r| { NReal(r) }),
                    },
            }
        })
    }

    fn parse_real(&mut self, radix: uint, dot_start: bool) -> NilRes<PResult, String> {
        if dot_start {
            self.parse_dotnumber()
        } else if radix == 10 {
            self.parse_decimal()
        } else {
            self.parse_urational(radix)
        }
    }

    fn parse_exponent(&mut self) -> Option<String> {
        let s =
        match self.try_consume(['+', '-']) {
            Some('+') => "+",
            Some('-') => "-",
            _ => "+",
        };

        let mut exp = "".to_string();
        loop {
            let c = match self.lookahead() {
                Ok(x) => x,
                Err(_) => break
            };

            match c {
                '0' ... '9' => {
                    self.consume_nil();
                    exp.push_char(c)
                },
                _ => break,
            }
        }

        if exp.is_empty() {
            None
        } else {
            Some(format!("{}{}", s, exp))
        }
    }

    fn parse_dotnumber(&mut self) -> NilRes<PResult, String> {
        let (float_part, _) = self.parse_radix(10);

        match self.try_consume(['e', 's', 'f', 'd', 'l']) {
            Some(_) => 
                match self.parse_exponent() {
                    None => NErr("invalid exponent".to_string()),
                    Some(e) => NSome(PFloat("".to_string(), float_part, e)),
                },
            None => NSome(PFloat("".to_string(), float_part, "".to_string())),
        }
    }

    fn parse_decimal(&mut self) -> NilRes<PResult, String> {
        let (x, xr) = self.parse_radix(10);
        let int_part = x + "0".repeat(xr);

        match self.try_consume(['.', 'e', 's', 'f', 'd', 'l', '/']) {
            Some('.') => {
                let float_part =
                if xr == 0 {
                    let (y, _) = self.parse_radix(10);
                    y
                } else {
                    while self.try_consume(['#']).is_some() { () }
                    "".to_string()
                };

                match self.try_consume(['e', 's', 'f', 'd', 'l']) {
                    Some(_) => 
                        match self.parse_exponent() {
                            None => NErr("invalid exponent".to_string()),
                            Some(e) => NSome(PFloat(int_part, float_part, e)),
                        },
                    None => NSome(PFloat(int_part, float_part, "".to_string())),
                }
            },
            Some('/') => {
                let (y, yr) = self.parse_radix(10);
                if y.is_empty() {
                    NErr("invalid rational literal".to_string())
                } else {
                    println!("int_part: {}, y: {}, yr: {}", int_part, y, yr);
                    NSome(PRational(int_part, y + "0".repeat(yr)))
                }
            },
            Some(_) => {
                match self.parse_exponent() {
                    None => NErr("invalid exponent".to_string()),
                    Some(e) => NSome(PFloat(int_part, "".to_string(), e)),
                }
            },
            None => NSome(PInt(int_part))
        }
    }

    fn parse_urational(&mut self, r: uint) -> NilRes<PResult, String> {
        let (x, xr) = self.parse_radix(r);

        if x.is_empty() {
            NNone
        } else if self.try_consume(['/']).is_some() {
            self.consume_nil();
            let (y, yr) = self.parse_radix(r);
            if y.is_empty() {
                NErr("invalid rational number".to_string())
            } else {
                NSome(PRational(x + "0".repeat(xr), y + "0".repeat(yr)))
            }
        } else {
            NSome(PInt(x + "0".repeat(xr)))
        }
    }

    fn parse_radix(&mut self, radix: uint) -> (String, uint) {
        let mut sharps = 0u;
        let mut consumed = false;
        let mut sum = String::new();

        loop {
            let c = match self.lookahead() {
                Ok(x) => x,
                Err(_) => break
            };

            if c == '#' && consumed {
                while self.try_consume(['#']).is_some() {
                    sharps += 1u;
                }
                break;
            } else if c.is_digit_radix(radix) {
                consumed = true;
                self.consume_nil();
                sum.push_char(c);
            } else {
                break;
            }
        }

        (sum, sharps)
    }

    fn parse_num_prefix(&mut self, init: &str) -> Result<(Option<bool>, Option<uint>), String> {
        let exactness =
        match init {
            "#i" => Some(false),
            "#e" => Some(true),
            _ => None,
        };

        let radix =
        match init {
            "#b" => Some(2u),
            "#o" => Some(8u),
            "#d" => Some(10u),
            "#x" => Some(16u),
            _ => None,
        };

        match self.try_consume(['#']) {
            None => Ok((exactness, radix)),
            Some(_) => 
                match self.try_consume(['i','e','b','o','d','x']) {
                    Some('i') if exactness == None => Ok((Some(false), radix)),
                    Some('e') if exactness == None => Ok((Some(true), radix)),
                    Some('i') | Some('e') => Err("multiple exactness prefix".to_string()),
                    Some('b') if radix == None => Ok((exactness, Some(2u))),
                    Some('o') if radix == None => Ok((exactness, Some(8u))),
                    Some('d') if radix == None => Ok((exactness, Some(10u))),
                    Some('x') if radix == None => Ok((exactness, Some(16u))),
                    Some(_) => Err("multiple radix prefix".to_string()),
                    None => match self.lookahead() {
                        Ok(c) => Err(format!("invalid number prefix #{:c}", c)),
                        Err(e) => Err(format!("{}", e))
                    }
                },
        }
    }

    fn parse_ident(&mut self) -> String {
        let mut ident = String::new();
        loop {
            let c = match self.lookahead() {
                Ok(x) => x,
                Err(_) => {
                    return ident;
                }
            };

            match c {
                '0' ... '9' | '+' | '-' | '.' | '@' => ident.push_char(c),
                _ =>
                    if id_init(c) {
                        ident.push_char(c)
                    } else {
                        return ident;
                    },
            };

            self.consume_nil();
        }
    }

    fn parse_string(&mut self) -> Result<String, String> {
        if self.consume() != Ok('"') {
            fail!("internal parser error".to_string());
        }

        let mut lit = String::new();

        loop {
            let c = try!(self.consume().map_err(|e| format!("{}", e)));
            match c {
                '\\' => {
                    let c1 = try!(self.consume().map_err(|e| format!("{}", e)));
                    lit.push_char(c1);
                },
                '"' => break,
                _ => {
                    lit.push_char(c);
                },
            }
        }

        Ok(lit)
    }
}

#[cfg(test)]
fn test_expect(src: &str, expected: &LDatum<()>) {
    with_str_reader(src, |rdr| {
        let mut parser = Parser::new(rdr);
        match parser.parse() {
            Ok(val) => if val != *expected {
                fail!("expected {}, received {}", expected, val)
            },
            Err(e) =>
                fail!("parse failure: {}", e),
        }
    })
}

#[test]
fn test_parse_ident() {
    test_expect("a3!", &LIdent("a3!".to_string()));
}

#[test]
fn test_parse_string() {
    test_expect("\"ab\\\"c\"", &LString("ab\"c".to_string()));
}

#[test]
fn test_parse_char() {
    test_expect("#\\h", &LChar('h'));
}

#[test]
fn test_parse_space() {
    test_expect("#\\space", &LChar(' '));
}

#[test]
fn test_parse_bool() {
    test_expect("#t", &LBool(true));
}

#[cfg(test)]
fn expect_int(src: &str, n: int) {
    with_str_reader(src, |rdr| {
        let mut parser = Parser::new(rdr);
        let val = parser.parse();
        let expected: LDatum<()> = LNum(from_int(n));
        assert_eq!(val, Ok(expected));
    })
}

#[cfg(test)]
fn expect_rational(src: &str, n:int, d: int) {
    with_str_reader(src, |rdr| {
        let mut parser = Parser::new(rdr);
        let val = parser.parse();
        let _n: BigInt = FromPrimitive::from_int(n).unwrap();
        let _d: BigInt = FromPrimitive::from_int(d).unwrap();
        let expected: LDatum<()> = LNum(from_rational(Ratio::new(_n, _d)));
        assert_eq!(val, Ok(expected));
    })
}

#[cfg(test)]
fn expect_f64(src: &str, f: f64) {
    use numeric::from_f64;

    with_str_reader(src, |rdr| {
        let mut parser = Parser::new(rdr);
        let val = parser.parse();
        let expected: LDatum<()> = LNum(from_f64(f));
        assert_eq!(val, Ok(expected));
    })
}

#[cfg(test)]
fn expect_ecmplx(src: &str, re_n: int, re_d: int, im_n: int, im_d: int) {
    with_str_reader(src, |rdr| {
        let mut parser = Parser::new(rdr);
        let val = parser.parse();
        let reN: BigInt = FromPrimitive::from_int(re_n).unwrap();
        let reD: BigInt = FromPrimitive::from_int(re_d).unwrap();
        let imN: BigInt = FromPrimitive::from_int(im_n).unwrap();
        let imD: BigInt = FromPrimitive::from_int(im_d).unwrap();
        let expected: LDatum<()> = LNum(exact(Ratio::new(reN, reD),
                                                Ratio::new(imN, imD)));
        assert_eq!(val, Ok(expected));
    })
}

#[cfg(test)]
fn expect_icmplx(src: &str, re: f64, im: f64) {
    with_str_reader(src, |rdr| {
        let mut parser = Parser::new(rdr);
        let val = parser.parse();
        let expected: LDatum<()> = LNum(inexact(re, im));
        assert_eq!(val, Ok(expected));
    })
}

#[test]
fn test_parse_int() {
    expect_int("10", 10);
}

#[test]
fn test_parse_rational() {
    expect_rational("3/6", 1, 2);
}

#[test]
fn test_parse_rational_float() {
    expect_f64("#i3/6", 0.5);
}

#[test]
fn test_parse_integral_float() {
    expect_f64("#i3", 3.0);
}

#[test]
fn test_parse_decimal_float() {
    expect_f64("3.6", 3.6);
}

#[test]
fn test_parse_decimal_float_exp() {
    expect_f64("31.4e-1", 3.14);
}

#[test]
fn test_parse_exact_decimal_float() {
    expect_rational("#e3.6", 18, 5);
}

#[test]
fn test_parse_exact_decimal_float_exp() {
    expect_rational("#e31.3e-1", 313, 100);
}

#[test]
fn test_parse_complex() {
    expect_ecmplx("1+2i", 1, 1, 2, 1);
}

#[test]
fn test_parse_icomplex() {
    expect_icmplx("1.0+2.0i", 1.0, 2.0);
}

#[test]
fn test_parse_polar_complex() {
    expect_icmplx("1@0", 1f64, 0f64);
}

#[cfg(test)]
fn test_expect_list(src: &str, list: Vec<Rc<LDatum<int>>>) {
    with_str_reader(src, |rdr| {
        let mut parser:Parser<io::MemReader> = Parser::new(rdr);
        let expr = format!("{}", list);
        match parser.parse() {
            Err(e) => fail!("parse failure: {}", e),
            Ok(val) => {
                match val.to_list() {
                    Some(vs) => if vs != list {
                        fail!("expected {}, received {}", expr, vs)
                    },
                    None =>
                        fail!("expected {}, received {}", expr, val),
                }
            }
       }
    })
}

#[test]
fn test_parse_list() {
    test_expect_list("(a b 1)", vec![Rc::new(LIdent("a".to_string())), Rc::new(LIdent("b".to_string())), Rc::new(LNum(from_int(1)))]);
}

#[test]
fn test_parse_vector() {
    test_expect("#(a b 1)", &LVector(Rc::new(vec![Rc::new(LIdent("a".to_string())), Rc::new(LIdent("b".to_string())), Rc::new(LNum(from_int(1)))])));
}

#[test]
fn test_parse_cons() {
    let n = Rc::new(LNum(from_int(1)));
    test_expect("(a b . 1)", &LCons(Rc::new(LIdent("a".to_string())), Rc::new(LCons(Rc::new(LIdent("b".to_string())), n))));
}

#[test]
fn test_plus_ident() {
    test_expect_list("(+ 1)", vec![Rc::new(LIdent("+".to_string())), Rc::new(LNum(from_int(1)))]);
}

#[test]
fn test_minus_ident() {
    test_expect_list("(- 2)", vec![Rc::new(LIdent("-".to_string())), Rc::new(LNum(from_int(2)))]);
}

#[test]
fn test_dots_ident() {
    test_expect_list("(... a)", vec![Rc::new(LIdent("...".to_string())), Rc::new(LIdent("a".to_string()))]);
}

#[test]
fn test_parse_dotted_number() {
    expect_f64(".1", 0.1f64);
}

#[test]
fn test_parse_quotation() {
    test_expect("'()", &LCons(Rc::new(LIdent("quote".to_string())), Rc::new(LCons(Rc::new(LNil), Rc::new(LNil)))));
}

#[test]
fn test_parse_plus() {
    test_expect("+", &LIdent("+".to_string()));
}

#[test]
fn test_parse_exponent() {
    test_expect("#e1e10", &LNum(from_int(10000000000)));
}

#[test]
fn test_parse_char_list() {
    test_expect("(#\\a)", &LCons(Rc::new(LChar('a')), Rc::new(LNil)));
    test_expect("(#\\1)", &LCons(Rc::new(LChar('1')), Rc::new(LNil)));
}

#[test]
fn test_minus_num() {
    test_expect("-2", &LNum(from_int(-2)));
}

#[test]
fn test_minus_rational() {
    use real::RRat;

    let n:BigInt = FromPrimitive::from_int(-7).unwrap();
    let d:BigInt = FromPrimitive::from_int(2).unwrap();
    test_expect("-7/2", &LNum( NReal(RRat(Ratio::new(n, d))) ));
}

#[test]
fn test_parse_bracket() {
    test_expect_list("[]", vec![]);
    test_expect_list("[a b 1]", vec![Rc::new(LIdent("a".to_string())), Rc::new(LIdent("b".to_string())), Rc::new(LNum(from_int(1)))]);
}
