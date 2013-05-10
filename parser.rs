use rational::Rational;
use numeric::*;
use datum::*;

#[cfg(test)]
use numeric::{from_int, from_rational};

priv enum PResult {
    PFloat(~str, ~str, ~str),
    PRational(~str, ~str),
    PInt(~str),
}

priv enum NilRes<T, E> {
    NNone,
    NErr(E),
    NSome(T),
}

priv enum DResult {
    DSingle,
    DDatum(LDatum),
    DErr(~str),
}

priv fn build_exact(sign: bool, radix: uint, &part: &PResult) -> Result<Rational, ~str> {
    match part {
        PInt(zs) =>
            Ok(Rational::new(s_to_int(zs, sign, radix), 1)),
        PRational(ds, ns) => {
            let d = s_to_int(ds, sign, radix);
            let n = s_to_int(ns, sign, radix);
            if(n == 0) {
                Err(~"divide by zero")
            } else {
                Ok(Rational::new(d, n))
            }
        },
        PFloat(i, f, e) => {
            let exp =
            if e.is_empty() {
                0
            } else {
                s_to_int(e, true, 10)
            };

            let n:int = num::pow_with_uint(10, f.len());
            let d = s_to_int(i + f, sign, 10);
            let p = num::pow_with_uint(10, num::abs(exp) as uint);

            if(n == 0) {
                Err(~"divide by zero")
            } else if(exp < 0) {
                Ok(Rational::new(d, n*p))
            } else {
                Ok(Rational::new(d*p, n))
            }
        },
    }
}

priv fn build_inexact(sign: bool, radix: uint, &part: &PResult) -> Result<f64, ~str> {
    match part {
        PInt(zs) => 
            Ok(s_to_int(zs, sign, radix) as f64),
        PRational(ds, ns) => {
            let d = s_to_int(ds, sign, radix);
            let n = s_to_int(ns, sign, radix);
            if(n == 0) {
                Err(~"divide by zero")
            } else {
                Ok((d as f64) / (n as f64))
            }
        },
        PFloat(i, f, e) => {
            let s = if e.is_empty() { i + "." + f } else { i + "." + f + "e" + e };
            Ok(s_to_f64(s, sign))
        }
    }
}

priv fn is_pfloat(&pres: &PResult) -> bool {
    match pres {
        PFloat(_, _, _) => true,
        _ => false,
    }
}

priv fn build_complex(exactness: Option<bool>,
                    radix: uint,
                    rsign: bool,
                    rpart: &PResult,
                    isign: bool,
                    ipart: &PResult) -> Result<LNumeric, ~str> {
    if exactness == Some(true) || 
            (exactness == None && !is_pfloat(rpart) && !is_pfloat(ipart)) {
        do result::chain(build_exact(rsign, radix, rpart)) |re| {
            do result::map(&build_exact(isign, radix, ipart)) |&im| {
                NExact(re, im)
            }
        }
    } else {
        do result::chain(build_inexact(rsign, radix, rpart)) |re| {
            do result::map(&build_inexact(isign, radix, ipart)) |&im| {
                NInexact(re, im)
            }
        }
    }
}

priv fn s_to_f64(s: &str, sign: bool) -> f64 {
    match f64::from_str(s) {
        Some(f) => if sign { f } else { -f },
        None => fail!(~"failed to convert float literal: " + s),
    }
}

priv fn s_to_int(s: &str, sign: bool, r: uint) -> int {
    match int::from_str_radix(s, r) {
        Some(z) => if sign { z } else { -z },
        None => fail!(~"failed to convert int literal " + s + " with radix " + r.to_str()),
    }
}

struct Parser {
    priv reader: @io::Reader,
    priv buf: char,
    priv has_buf: bool,
    line: uint,
    col: uint,
}

pub fn Parser(reader: @io::Reader) -> Parser {
    Parser {
        reader: reader,
        has_buf: false,
        buf: 0 as char,
        line: 1,
        col: 1,
    }
}

priv fn id_init(c: char) -> bool {
    match(c) {
        '!' | '$' | '%' | '&' | '*' | '/' | ':' |
        '<' | '=' | '>' | '?' | '^' | '_' | '~' |
        'a' .. 'z' | 'A' .. 'Z' => true,
        _ => false,
    }
}

pub impl Parser {

    #[inline(always)]
    fn pos(&self) -> (uint, uint) {
        (self.line, self.col)
    }

    fn parse(&mut self) -> Result<LDatum, ~str> {
        do result::chain(self.parse_datum()) |v| {
            self.consume_whitespace();

            if(self.eof()) {
                Ok(v)
            } else {
                Err(~"trailing input")
            }
        }
    }

    fn parse_datum(&mut self) -> Result<LDatum, ~str> {
        self.consume_whitespace();

        if(self.eof()) {
            return Err(~"unexpected EOF");
        }

        let c = self.lookahead();
        match(c) {
            _ if id_init(c) =>
                Ok(LIdent(self.parse_ident())),
            '"' =>
                do result::map(&self.parse_string()) |&s| {
                    LString(s)
                },
            '#' => {
                    self.consume();
                    self.parse_sharp()
                },
            '0'..'9' =>
                do result::map(&self.parse_number(~"")) |&n| {
                    LNum(n)
                },
            '(' => {
                    self.consume();
                    self.consume_whitespace();
                    match self.try_consume([')']) {
                        Some(_) => Ok(LNil),
                        None => match self.parse_datum() {
                            Err(e) => Err(e),
                            Ok(head) => self.parse_list(@head),
                        },
                    }
                },
            '+' | '-' => {
                self.consume();
                if self.eof() {
                    Ok(LIdent(str::from_char(c).to_managed()))
                } else {
                    match self.lookahead() {
                        '0' .. '9' | 'i' | '.' =>
                            do result::map(&self.parse_number(str::from_char(c))) |&n| {
                                LNum(n)
                            },
                        _ =>
                            Ok(LIdent(str::from_char(c).to_managed())),
                    }
                }
            },
            '.' => {
                self.consume();
                match self.parse_dot() {
                    DSingle => Err(~"invalid token '.' in data context"),
                    DDatum(x) => Ok(x),
                    DErr(e) => Err(e),
                }
            },
            '\'' => {
                self.consume();
                do result::map(&self.parse_datum()) |&v| {
                    LQuote(@v)
                }
            },
            '`' => {
                self.consume();
                do result::map(&self.parse_datum()) |&v| {
                    LQQuote(@v)
                }
            },
            ',' => {
                self.consume();
                do result::map(&self.parse_datum()) |&v| {
                    LUnquote(@v)
                }
            },
            _ =>
                Err(~"unexpected character: " + str::from_char(c)),
        }
    }
}

priv impl Parser {
    fn eof(&mut self) -> bool {
        !self.has_buf && self.reader.eof()
    }

    fn consume(&mut self) -> char {
        if(self.has_buf) {
            self.has_buf = false;
        } else {
            self.buf = self.reader.read_char();
        }

        if(self.buf == '\n') {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }

        self.buf
    }

    fn lookahead(&mut self) -> char {
        if(!self.has_buf) {
            self.has_buf = true;
            self.buf = self.reader.read_char();
        }
        self.buf
    }

    fn try_consume(&mut self, v: &[char]) -> Option<char> {
        if !self.has_buf {
            if self.reader.eof() {
                return None;
            } else {
                self.has_buf = true;
                self.buf = self.reader.read_char();
            }
        }

        if v.contains(&self.buf) {
            self.has_buf = false;
            if(self.buf == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            Some(self.buf)
        } else {
            None
        }
    }

    fn consume_whitespace(&mut self) -> bool {
        let mut consumed = false;
        while(!self.eof()) {
            match self.try_consume([' ', '\t', '\r', '\n', ';']) {
                Some(';') => {
                    consumed = true;
                    while(!self.eof() && self.consume() != '\n') { () }
                },
                Some(_) => {
                    consumed = true;
                },
                None => break,
            }
        }

        consumed
    }

    fn parse_sharp(&mut self) -> Result<LDatum, ~str> {
        if(self.eof()) {
            return Err(~"unexpected EOF");
        }

        let c = self.consume();
        match(c) {
            '\\' =>
                match(self.parse_char()) {
                    Ok(x) => Ok(LChar(x)),
                    Err(e) => Err(e),
                },
            't' =>
                Ok(LBool(true)),
            'f' =>
                Ok(LBool(false)),
            'e' | 'i' | 'b' | 'o' | 'd' | 'x' => 
                do result::map(&self.parse_number(~"#" + str::from_char(c))) |&n| {
                    LNum(n)
                },
            _ =>
                Err(~"unexpected character: " + str::from_char(c)),
        }
    }

    fn parse_list(&mut self, head: @LDatum) -> Result<LDatum, ~str> {
        self.consume_whitespace();
        if(self.eof()) {
            Err(~"parenthesis not closed")
        } else {
            match self.lookahead() {
                ')' => {
                    self.consume();
                    Ok(LCons(head, @LNil))
                },
                '.' => {
                    self.consume();
                    match self.parse_dot() {
                        DSingle => 
                            match self.parse_datum() {
                                Err(e) => Err(e),
                                Ok(tail) => {
                                    self.consume_whitespace();
                                    match self.try_consume([')']) {
                                        None => Err(~"RPAREN not found after DOT"),
                                        Some(_) => Ok(LCons(head, @tail)),
                                    }
                                },
                            },
                        DDatum(tail1) => {
                            match self.parse_list(@tail1) {
                                Ok(tail) => Ok(LCons(head, @tail)),
                                Err(e) => Err(e),
                            }
                        },
                        DErr(e) => Err(e),
                    }
                },
                _ => match self.parse_datum() {
                    Err(e) => Err(e),
                    Ok(tail1) => {
                        match self.parse_list(@tail1) {
                            Ok(tail) => Ok(LCons(head, @tail)),
                            Err(e) => Err(e),
                        }
                    },
                },
            }
        }
    }

    fn parse_dot(&mut self) -> DResult {
        if self.eof() {
            DSingle
        } else {
            match self.lookahead() {
                '0'..'9' => match self.parse_number(~".") {
                    Ok(x) => DDatum(LNum(x)),
                    Err(e) => DErr(e),
                },
                '.' => {
                    self.consume();
                    match self.try_consume(['.']) {
                        None => DErr(~"invalid token '..'"),
                        _ => DDatum(LIdent(@"...")),
                    }
                }
                _ => DSingle
            }
        }
    }

    fn parse_char(&mut self) -> Result<char, ~str> {
        if(self.eof()) {
            return Err(~"unexpected EOF");
        }

        let lit =
        do io::with_str_writer |wr| {
            while(!self.eof()) {
                match(self.lookahead()) {
                    ' ' | '\t' | '\r' | '\n' => break,
                    _ => wr.write_char(self.consume()),
                }
            }
        };

        if(lit.len() == 1) {
            Ok(lit.char_at(0))
        } else if(lit == ~"space") {
            Ok(' ')
        } else if(lit == ~"newline") {
            Ok('\n')
        } else {
            Err(~"unknown character literal " + lit)
        }
    }

    fn parse_number(&mut self, init: &str) -> Result<LNumeric, ~str> {
        do result::chain(self.parse_num_prefix(init)) |pref| {
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
                            build_complex(exactness, r, true, &PInt(~"0"), rsign, &PInt(~"1")),
                        None =>
                            Err(~"empty number literal"),
                    },
                NSome(rpart) =>
                    match self.try_consume(['i', '@', '+', '-']) {
                        Some('i') => 
                            build_complex(exactness, r, true, &PInt(~"0"), rsign, &rpart),
                        Some('@') => {
                            do result::chain(build_inexact(rsign, r, &rpart)) |abs| {
                                let asign = match self.try_consume(['+', '-']) {
                                    Some('-') => false,
                                    _ => true,
                                };
                                match self.parse_real(r, false) {
                                    NSome(apart) =>
                                        do result::map(&build_inexact(asign, r, &apart)) |&arg| {
                                            let re = abs * f64::cos(arg);
                                            let im = abs * f64::sin(arg);
                                            NInexact(re, im)
                                        },
                                    _ => Err(~"invalid polar literal"),
                                }
                            }
                        },
                        Some(s) => {
                            let isign = s == '+';
                            let res =
                            match self.parse_real(r, false) {
                                NSome(ipart) =>
                                    build_complex(exactness, r, rsign, &rpart, isign, &ipart),
                                _ =>
                                    Err(~"invalid complex literal"),
                            };
                            match self.try_consume(['i']) {
                                Some(_) => res,
                                None => Err(~"invalid complex literal"),
                            }
                        },
                        None => 
                            build_complex(exactness, r, rsign, &rpart, true, &PInt(~"0"))
                    },
            }
        }
    }

    fn parse_real(&mut self, radix: uint, dot_start: bool) -> NilRes<PResult, ~str> {
        if dot_start {
            self.parse_dotnumber()
        } else if radix == 10 {
            self.parse_decimal()
        } else {
            self.parse_urational(radix)
        }
    }

    fn parse_exponent(&mut self) -> Option<~str> {
        let s =
        match self.try_consume(['+', '-']) {
            Some('+') => ~"+",
            Some('-') => ~"-",
            _ => return None,
        };

        let mut exp = ~"";
        while(!self.eof()) {
            match self.lookahead() {
                '0' .. '9' => exp.push_char(self.consume()),
                _ => break,
            }
        }

        if exp.is_empty() {
            None
        } else {
            Some(s + exp)
        }
    }

    fn parse_dotnumber(&mut self) -> NilRes<PResult, ~str> {
        let (float_part, _) = self.parse_radix(10);

        match self.try_consume(['e', 's', 'f', 'd', 'l']) {
            Some(_) => 
                match self.parse_exponent() {
                    None => NErr(~"invalid exponent"),
                    Some(e) => NSome(PFloat(~"", float_part, e)),
                },
            None => NSome(PFloat(~"", float_part, ~"")),
        }
    }

    fn parse_decimal(&mut self) -> NilRes<PResult, ~str> {
        let (x, xr) = self.parse_radix(10);
        let int_part = x + str::repeat("0", xr);

        match self.try_consume(['.', 'e', 's', 'f', 'd', 'l', '/']) {
            Some('.') => {
                let float_part =
                if(xr == 0) {
                    let (y, _) = self.parse_radix(10);
                    y
                } else {
                    while(self.try_consume(['#']).is_some()) { () }
                    ~""
                };

                match self.try_consume(['e', 's', 'f', 'd', 'l']) {
                    Some(_) => 
                        match self.parse_exponent() {
                            None => NErr(~"invalid exponent"),
                            Some(e) => NSome(PFloat(int_part, float_part, e)),
                        },
                    None => NSome(PFloat(int_part, float_part, ~"")),
                }
            },
            Some('/') => {
                let (y, yr) = self.parse_radix(10);
                if(y.is_empty()) {
                    NErr(~"invalid rational literal")
                } else {
                    NSome(PRational(int_part, y + str::repeat("0", yr)))
                }
            },
            Some(_) => {
                match self.parse_exponent() {
                    None => NErr(~"invalid exponent"),
                    Some(e) => NSome(PFloat(int_part, ~"", e)),
                }
            },
            None => NSome(PInt(int_part))
        }
    }

    fn parse_urational(&mut self, r: uint) -> NilRes<PResult, ~str> {
        let (x, xr) = self.parse_radix(r);

        if(x.is_empty()) {
            NNone
        } else if(self.try_consume(['/']).is_some()) {
            self.consume();
            let (y, yr) = self.parse_radix(r);
            if(y.is_empty()) {
                NErr(~"invalid rational number")
            } else {
                NSome(PRational(x + str::repeat("0", xr), y + str::repeat("0", yr)))
            }
        } else {
            NSome(PInt(x + str::repeat("0", xr)))
        }
    }

    fn parse_radix(&mut self, radix: uint) -> (~str, uint) {
        let mut sum = ~"";
        let mut sharps = 0u;
        let mut consumed = false;

        while(!self.eof()) {
            let c = self.lookahead();

            if(c == '#' && consumed) {
                while(self.try_consume(['#']).is_some()) {
                    sharps += 1u;
                }
                break;
            } else if(char::is_digit_radix(c, radix)) {
                consumed = true;
                self.consume();
                sum += str::from_char(c);
            } else {
                break;
            }
        }

        (sum, sharps)
    }

    fn parse_num_prefix(&mut self, init: &str) -> Result<(Option<bool>, Option<uint>), ~str> {
        let mut exactness =
        match(init) {
            "#i" => Some(false),
            "#e" => Some(true),
            _ => None,
        };

        let mut radix =
        match(init) {
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
                    Some('i') | Some('e') => Err(~"multiple exactness prefix"),
                    Some('b') if radix == None => Ok((exactness, Some(2u))),
                    Some('o') if radix == None => Ok((exactness, Some(8u))),
                    Some('d') if radix == None => Ok((exactness, Some(10u))),
                    Some('x') if radix == None => Ok((exactness, Some(16u))),
                    Some(_) => Err(~"multiple radix prefix"),
                    None => Err(~"invalid number prefix #" + str::from_char(self.lookahead())),
                },
        }
    }

    fn parse_ident(&mut self) -> @str {
        do io::with_str_writer |wr| {
            while(!self.eof()) {
                let c = self.lookahead();

                match(c) {
                    '0' .. '9' | '+' | '-' | '.' | '@' => wr.write_char(c),
                    _ =>
                        if id_init(c) {
                            wr.write_char(c)
                        } else {
                            break
                        },
                }

                self.consume();
            }
        }.to_managed()
    }

    fn parse_string(&mut self) -> Result<~str, ~str> {
        let mut escape_fail = false;

        if(self.eof() || self.consume() != '"') {
            fail!(~"internal parser error");
        }

        let val = do io::with_str_writer |wr| {
            while(!self.eof()) {
                let c = self.consume();
                match(c) {
                    '\\' => if self.eof() {
                                escape_fail = true;
                            } else {
                                wr.write_char(self.consume());
                            },
                    '"' => break,
                    _ => wr.write_char(c),
                }
            }
        };

        if(escape_fail) {
            Err(~"unexpected EOF")
        } else {
            Ok(val)
        }
    }
}

#[test]
fn test_parse_ident() {
    let test_src = ~"a3!";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LIdent(@"a3!")));
    }
}

#[test]
fn test_parse_string() {
    let test_src = ~"\"ab\\\"c\"";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LString(~"ab\"c")));
    }
}

#[test]
fn test_parse_char() {
    let test_src = ~"#\\h";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LChar('h')));
    }
}

#[test]
fn test_parse_space() {
    let test_src = ~"#\\space";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LChar(' ')));
    }
}

#[test]
fn test_parse_bool() {
    let test_src = ~"#t";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LBool(true)));
    }
}

#[cfg(test)]
fn expect_int(src: ~str, n: int) {
    do io::with_str_reader(src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LNum(from_int(n))));
    }
}

#[cfg(test)]
fn expect_rational(src: ~str, d:int, n: int) {
    do io::with_str_reader(src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LNum(from_rational(Rational::new(d, n)))));
    }
}

#[cfg(test)]
fn expect_f64(src: ~str, f: f64) {
    do io::with_str_reader(src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LNum(from_f64(f))));
    }
}

#[cfg(test)]
fn expect_ecmplx(src: ~str, re_d: int, re_n: int, im_d: int, im_n: int) {
    do io::with_str_reader(src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LNum(NExact(Rational::new(re_d, re_n), Rational::new(im_d, im_n)))));
    }
}

#[cfg(test)]
fn expect_icmplx(src: ~str, re: f64, im: f64) {
    do io::with_str_reader(src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LNum(NInexact(re, im))));
    }
}

#[test]
fn test_parse_int() {
    expect_int(~"10", 10);
}

#[test]
fn test_parse_rational() {
    expect_rational(~"3/6", 1, 2);
}

#[test]
fn test_parse_rational_float() {
    expect_f64(~"#i3/6", 0.5);
}

#[test]
fn test_parse_integral_float() {
    expect_f64(~"#i3", 3.0);
}

#[test]
fn test_parse_decimal_float() {
    expect_f64(~"3.6", 3.6);
}

#[test]
fn test_parse_decimal_float_exp() {
    expect_f64(~"31.4e-1", 3.14);
}

#[test]
fn test_parse_exact_decimal_float() {
    expect_rational(~"#e3.6", 18, 5);
}

#[test]
fn test_parse_exact_decimal_float_exp() {
    expect_rational(~"#e31.3e-1", 313, 100);
}

#[test]
fn test_parse_complex() {
    expect_ecmplx(~"1+2i", 1, 1, 2, 1);
}

#[test]
fn test_parse_icomplex() {
    expect_icmplx(~"1.0+2.0i", 1.0, 2.0);
}

#[test]
fn test_parse_polar_complex() {
    expect_icmplx(~"1@0", 1f64, 0f64);
}

#[test]
fn test_parse_list() {
    let test_src = ~"(a b 1)";
    let n = @LNum(from_int(1));

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        match parser.parse() {
            Err(e) => fail!(e),
            Ok(val) => assert_eq!(val.to_list(), Some(~[@LIdent(@"a"), @LIdent(@"b"), n])),
        };
    }
}

#[test]
fn test_parse_cons() {
    let test_src = ~"(a b . 1)";
    let n = @LNum(from_int(1));

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LCons(@LIdent(@"a"), @LCons(@LIdent(@"b"), n))));
    }
}

#[test]
fn test_plus_ident() {
    let test_src = ~"(+ 1)";
    let n = @LNum(from_int(1));

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LCons(@LIdent(@"+"), @LCons(n, @LNil))));
    }
}

#[test]
fn test_minus_ident() {
    let test_src = ~"(- 2)";
    let n = @LNum(from_int(2));

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LCons(@LIdent(@"-"), @LCons(n, @LNil))));
    }
}

#[test]
fn test_dots_ident() {
    let test_src = ~"(... a)";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LCons(@LIdent(@"..."), @LCons(@LIdent(@"a"), @LNil))));
    }
}

#[test]
fn test_parse_dotted_number() {
    expect_f64(~".1", 0.1f64);
}

#[test]
fn test_parse_quotation() {
    let test_src = ~"'()";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LQuote(@LNil)));
    }
}

#[test]
fn test_parse_plus() {
    let test_src = ~"+";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LIdent(@"+")));
    }
}
