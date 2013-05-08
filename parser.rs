use rational::Rational;
use complex::Complex;
use core::num::One::one;
use core::num::Zero::zero;

#[deriving(Eq)]
pub enum LDatum {
    LIdent(~str),
    LString(~str),
    LChar(char),
    LBool(bool),
    LECmplx(Complex<Rational>),
    LICmplx(Complex<f64>),
    LRational(Rational),
    LFloat(f64),
    LInt(int),
    LCons(@LDatum, @LDatum),
    LNil,
}

priv enum PResult {
    PFloat(~str, ~str, ~str),
    PRational(~str, ~str),
    PInt(~str),
    PErr(~str),
    PNone,
}

priv enum RResult {
    RInt(int),
    RRational(Rational),
    RFloat(f64),
    RErr(~str),
    RNone,
}

priv enum CResult {
    CReal(bool, RResult),
    CECmplx(bool, Rational, bool, Rational),
    CICmplx(bool, f64, bool, f64),
    CArc(bool, RResult, bool, RResult),
    CErr(~str),
}

priv fn build_complex(rpart: &RResult, ipart: &RResult) -> Result<LDatum, ~str> {
    let re =
    match *rpart {
        RInt(rz) => Left(Rational::new(rz, 1)),
        RRational(f) => Left(f),
        RFloat(f) => Right(f),
        _ => return Err(~"invalid number literal"),
    };

    let im =
    match *ipart {
        RInt(rz) => Left(Rational::new(rz, 1)),
        RRational(f) => Left(f),
        RFloat(f) => Right(f),
        _ => return Err(~"invalid number literal"),
    };

    match (re, im) {
        (Left(r), Left(i)) => Ok(LECmplx(Complex::new(r, i))),
        (Right(r), Right(i)) => Ok(LICmplx(Complex::new(r, i))),
        _ => fail!(~"internal parser error"),
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

impl ToStr for LDatum {
    fn to_str(&self) -> ~str {
        do io::with_str_writer |wr| {
            LDatum::write_ldatum(wr, self)
        }
    }

}

pub impl LDatum {
    fn to_list(&self) -> Option<~[@LDatum]> {
        match *self {
            LCons(h, t) => datum_to_list(h, t),
            LNil => Some(~[]),
            _ => None,
        }
    }
}

priv fn datum_to_list(head: @LDatum, tail: @LDatum) -> Option<~[@LDatum]> {
    let mut x: @LDatum = tail;
    let mut list_flag = false;

    let list =
    do vec::build |push| {
        push(head);
        loop {
            match *x {
                LCons(h, t) => {
                    push(h);
                    x = t;
                },
                LNil => break,
                _ => {
                    list_flag = true;
                    break
                },
            }
        }
    };

    if list_flag {
        None
    } else {
        Some(list)
    }
}

priv impl LDatum {
    fn write_ldatum(wr: @io::Writer, &v: &LDatum) {
        match v {
            LIdent(s) => wr.write_str(s),
            LString(s) => wr.write_str(fmt!("%?", copy s)),
            LChar(' ') => wr.write_str("#\\space"),
            LChar('\n') => wr.write_str("#\\newline"),
            LChar(c) => {
                wr.write_str("#\\");
                wr.write_char(c);
            }
            LBool(true) => wr.write_str("#t"),
            LBool(false) => wr.write_str("#f"),
            LRational(f) => {
                if f.is_negative() {
                    wr.write_char('-');
                }
                wr.write_str(f.denominator().to_str());
                wr.write_char('/');
                wr.write_str(f.numerator().to_str());
            },
            LFloat(f) => wr.write_str(f.to_str()),
            LInt(z) => wr.write_str(z.to_str()),
            LICmplx(c) => wr.write_str(c.to_str()),
            LECmplx(c) => wr.write_str(c.to_str()),
            LCons(head, tail) => {
                wr.write_char('(');
                LDatum::write_ldatum(wr, head);
                LDatum::write_list(wr, tail);
            },
            LNil => wr.write_str("()"),
        }
    }

    fn write_list(wr: @io::Writer, &v: &LDatum) {
        match v {
            LCons(head, tail) => {
                wr.write_str(", ");
                LDatum::write_ldatum(wr, head);
                LDatum::write_list(wr, tail);
            },
            LNil => wr.write_char(')'),
            _ => {
                wr.write_str(" . ");
                LDatum::write_ldatum(wr, &v);
                wr.write_char(')');
            },
        }
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

    fn parse_datum(&mut self) -> Result<LDatum, ~str> {
        self.consume_whitespace();

        if(self.eof()) {
            return Err(~"unexpected EOF");
        }

        let c = self.lookahead();
        match(c) {
            _ if id_init(c) =>
                Ok(LIdent(self.parse_ident())),
            '"' => {
                    match(self.parse_string()) {
                        Ok(s) => Ok(LString(s)),
                        Err(e) => Err(e),
                    }
                },
            '#' => {
                    self.consume();
                    self.parse_sharp()
                },
            '0'..'9' =>
                self.parse_number(~""),
            '(' => {
                    self.consume();
                    self.parse_list()
                },
            '+' | '-' => {
                self.consume();
                if self.consume_whitespace() {
                    Ok(LIdent(str::from_char(c)))
                } else {
                    self.parse_number(str::from_char(c))
                }
            }
            _ =>
                Err(~"unexpected character: " + str::from_char(c)),
        }
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
                self.parse_number(~"#" + str::from_char(c)),
            _ =>
                Err(~"unexpected character: " + str::from_char(c)),
        }
    }

    fn parse_list(&mut self) -> Result<LDatum, ~str> {
        self.consume_whitespace();
        if(self.eof()) {
            Err(~"parenthesis not closed")
        } else {
            match self.lookahead() {
                ')' => {
                    self.consume();
                    Ok(LNil)
                },
                _ => match self.parse_datum() {
                    Err(e) => Err(e),
                    Ok(head) => {
                        self.consume_whitespace();
                        match self.try_consume(['.']) {
                            Some(_) => match self.parse_datum() {
                                Err(e) => Err(e),
                                Ok(tail) => {
                                    self.consume_whitespace();
                                    match self.try_consume([')']) {
                                        None => Err(~"RPAREN not found after DOT"),
                                        Some(_) => Ok(LCons(@head, @tail)),
                                    }
                                },
                            },
                            None => match self.parse_list() {
                                Err(e) => Err(e),
                                Ok(tail) => Ok(LCons(@head, @tail)),
                            },
                        }
                    },
                },
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

    fn parse_number(&mut self, init: &str) -> Result<LDatum, ~str> {
        match self.parse_num_prefix(init) {
            Err(e) => Err(e),
            Ok((exactness, radix)) => {
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

                match self.parse_real(exactness, rsign, r) {
                    RErr(e) => Err(e),
                    RNone => 
                        match self.try_consume(['i']) {
                            Some(_) =>
                                match exactness {
                                    Some(false) => {
                                        Ok(LICmplx(Complex::new(0f64, if rsign { 1f64 } else { -1f64 })))
                                    },
                                    _ => {
                                        let re:Rational = zero();
                                        let im:Rational = one();
                                        if rsign {
                                            Ok(LECmplx(Complex::new(re, im)))
                                        } else {
                                            Ok(LECmplx(Complex::new(re, -im)))
                                        }
                                    },
                                },
                            None => Err(~"empty number literal"),
                        },
                    rpart =>
                        match self.try_consume(['i', '@', '+', '-']) {
                            Some('i') => match rpart {
                                    RInt(z) => Ok(LECmplx(Complex::new(zero(), Rational::new(z, 1)))),
                                    RRational(f) => Ok(LECmplx(Complex::new(zero(), f))),
                                    RFloat(f) => Ok(LICmplx(Complex::new(0.0, f))),
                                    _ => fail!(~"internal parser error"), 
                                },
                            Some('@') => {
                                    let abs = match rpart {
                                        RInt(z) => z as f64,
                                        RRational(f) => f.to_f64(),
                                        RFloat(f) => f,
                                        _ => fail!(~"internal parser error"),
                                    };
                                    let asign = match self.try_consume(['+', '-']) {
                                        Some('-') => false,
                                        _ => true,
                                    };
                                    let arg = match self.parse_real(exactness, asign, r) {
                                        RInt(z) => z as f64,
                                        RRational(f) => f.to_f64(),
                                        RFloat(f) => f,
                                        _ => return Err(~"invalid polar literal"),
                                    };
                                    let re = abs * f64::cos(arg);
                                    let im = abs * f64::sin(arg);
                                    Ok(LICmplx(Complex::new(re, im)))
                                },
                            Some(s) => {
                                    let isign = s == '+';
                                    let ipart = self.parse_real(exactness, isign, r);
                                    match self.try_consume(['i']) {
                                        Some(_) => build_complex(&rpart, &ipart),
                                        None => Err(~"invalid complex literal"),
                                    }
                                },
                            None => 
                                match rpart {
                                    RInt(z) => Ok(LInt(z)),
                                    RRational(f) => Ok(LRational(f)),
                                    RFloat(f) => Ok(LFloat(f)),
                                    _ => fail!(~"internal parser error"),
                                },
                        },
                }
            }
        }
    }

    fn parse_real(&mut self, exactness: Option<bool>, sign: bool, radix: uint) -> RResult {
        let res =
        if radix == 10 {
            self.parse_decimal()
        } else {
            self.parse_urational(radix)
        };

        match res {
            PErr(e) => RErr(e),
            PNone => RNone,
            PInt(zs) =>
                match exactness {
                    Some(false) => RFloat(s_to_int(zs, sign, radix) as f64),
                    _ => RInt(s_to_int(zs, sign, radix)),
                },
            PRational(ds, ns) => {
                    let d = s_to_int(ds, sign, radix);
                    let n = s_to_int(ns, sign, radix);
                    match exactness {
                        Some(false) => RFloat((d as f64) / (n as f64)),
                        _ => RRational(Rational::new(d, n)),
                    }
                },
            PFloat(i, f, e) => 
                match exactness {
                    Some(true) => {
                        let exp =
                        if e.is_empty() {
                            0
                        } else {
                            s_to_int(e, true, 10)
                        };

                        let n:int = num::pow_with_uint(10, f.len());
                        let d = s_to_int(i + f, sign, 10);
                        let p = num::pow_with_uint(10, num::abs(exp) as uint);

                        if(exp < 0) {
                            RRational(Rational::new(d, n*p))
                        } else {
                            RRational(Rational::new(d*p, n))
                        }
                    },
                    _ => {
                        let s = if e.is_empty() { i + "." + f } else { i + "." + f + "e" + e };
                        RFloat(s_to_f64(s, sign))
                    },
                },
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

    fn parse_decimal(&mut self) -> PResult {
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
                            None => PErr(~"invalid exponent"),
                            Some(e) => PFloat(int_part, float_part, e),
                        },
                    None => PFloat(int_part, float_part, ~""),
                }
            },
            Some('/') => {
                let (y, yr) = self.parse_radix(10);
                if(y.is_empty()) {
                    PErr(~"invalid rational literal")
                } else {
                    PRational(int_part, y + str::repeat("0", yr))
                }
            },
            Some(_) => {
                match self.parse_exponent() {
                    None => PErr(~"invalid exponent"),
                    Some(e) => PFloat(int_part, ~"", e),
                }
            },
            None => PInt(int_part)
        }
    }

    fn parse_urational(&mut self, r: uint) -> PResult {
        let (x, xr) = self.parse_radix(r);

        if(x.is_empty()) {
            PNone
        } else if(self.try_consume(['/']).is_some()) {
            self.consume();
            let (y, yr) = self.parse_radix(r);
            if(y.is_empty()) {
                PErr(~"invalid rational number")
            } else {
                PRational(x + str::repeat("0", xr), y + str::repeat("0", yr))
            }
        } else {
            PInt(x + str::repeat("0", xr))
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

    fn parse_ident(&mut self) -> ~str {
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
        }
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
        assert_eq!(val, Ok(LIdent(~"a3!")));
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

#[test]
fn test_parse_int() {
    let test_src = ~"10";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LInt(10)));
    }
}

#[test]
fn test_parse_rational() {
    let test_src = ~"3/6";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LRational(Rational::new(1,2))));
    }
}

#[test]
fn test_parse_rational_float() {
    let test_src = ~"#i3/6";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LFloat(0.5)));
    }
}

#[test]
fn test_parse_integral_float() {
    let test_src = ~"#i3";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LFloat(3.0)));
    }
}

#[test]
fn test_parse_decimal_float() {
    let test_src = ~"3.6";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LFloat(3.6)));
    }
}

#[test]
fn test_parse_decimal_float_exp() {
    let test_src = ~"31.4e-1";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LFloat(3.14)));
    }
}

#[test]
fn test_parse_exact_decimal_float() {
    let test_src = ~"#e3.6";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LRational(Rational::new(36, 10))));
    }
}

#[test]
fn test_parse_exact_decimal_float_exp() {
    let test_src = ~"#e31.4e-1";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LRational(Rational::new(314, 100))));
    }
}

#[test]
fn test_parse_complex() {
    let test_src = ~"1+2i";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LECmplx(Complex::new(Rational::new(1, 1), Rational::new(2, 1)))));
    }
}

#[test]
fn test_parse_icomplex() {
    let test_src = ~"1.0+2.0i";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LICmplx(Complex::new(1f64, 2f64))));
    }
}

#[test]
fn test_parse_polar_complex() {
    let test_src = ~"1@0";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LICmplx(Complex::new(1f64, 0f64))));
    }
}

#[test]
fn test_parse_list() {
    let test_src = ~"(a b 1)";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        match parser.parse() {
            Err(e) => fail!(e),
            Ok(val) => assert_eq!(val.to_list(), Some(~[@LIdent(~"a"), @LIdent(~"b"), @LInt(1)])),
        };
    }
}

#[test]
fn test_parse_cons() {
    let test_src = ~"(a b . 1)";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LCons(@LIdent(~"a"), @LCons(@LIdent(~"b"), @LInt(1)))));
    }
}

#[test]
fn test_plus_ident() {
    let test_src = ~"(+ 1)";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LCons(@LIdent(~"+"), @LCons(@LInt(1), @LNil))));
    }
}

#[test]
fn test_minus_ident() {
    let test_src = ~"(- 2)";

    do io::with_str_reader(test_src) |rdr| {
        let mut parser = Parser(rdr);
        let val = parser.parse();
        assert_eq!(val, Ok(LCons(@LIdent(~"-"), @LCons(@LInt(2), @LNil))));
    }
}
