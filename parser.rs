use rational::Rational;

#[deriving(Eq)]
pub enum LDatum {
    LIdent(~str),
    LString(~str),
    LChar(char),
    LBool(bool),
    LRational(Rational),
    LFloat(f64),
    LInt(int),
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

priv fn s_to_f64(s: &str) -> f64 {
    match f64::from_str(s) {
        Some(f) => f,
        None => fail!(~"failed to convert float literal: " + s),
    }
}

priv fn s_to_int(s: &str, r: uint) -> int {
    match int::from_str_radix(s, r) {
        Some(z) => z,
        None => fail!(~"failed to convert int literal " + s + " with radix " + r.to_str()),
    }
}

pub fn to_sexpr(&x: &LDatum) -> ~str {
    match(x) {
        LIdent(s) => s,
        LString(s) => fmt!("%?", copy s),
        LChar(' ') => ~"#\\space",
        LChar('\n') => ~"#\\newline",
        LChar(c) => ~"#\\" + str::from_char(c),
        LBool(true) => ~"#t",
        LBool(false) => ~"#f",
        LRational(f) => {
                let s = if f.is_negative() { ~"-" } else { ~"" };
                s + f.denominator().to_str() + "/" + f.numerator().to_str()
            },
        LFloat(f) => f.to_str(),
        LInt(z) => z.to_str(),
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
        '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '^' | '_' | '~' | 'a' .. 'z' | 'A' .. 'Z' => true,
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

    fn consume_whitespace(&mut self) {
        while(!self.eof()) {
            match(self.lookahead()) {
                ' '| '\t' | '\r' | '\n' => { self.consume(); },
                ';' =>
                    {
                        self.consume();
                        while(!self.eof() && self.consume() != '\n') { () }
                    },
                _ => break
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
                if(self.eof()) {
                    return Err(~"unexpected EOF");
                }

                let r = match radix { None => 10, Some(d) => d };

                let sign =
                match(self.lookahead()) {
                    '+' => { self.consume(); true },
                    '-' => { self.consume(); false },
                    _ => true,
                };

                match self.parse_ureal(exactness, r) {
                    RInt(z) => Ok(LInt(if sign { z } else { -z })),
                    RRational(f) => Ok(LRational(if sign { f } else { -f })),
                    RFloat(f) => Ok(LFloat(if sign { f } else { -f })),
                    RErr(e) => Err(e),
                    RNone => Err(~"empty number literal"),
                }
            }
        }
    }

    fn parse_ureal(&mut self, exactness: Option<bool>, radix: uint) -> RResult {
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
                    Some(false) => RFloat(s_to_int(zs, radix) as f64),
                    _ => RInt(s_to_int(zs, radix)),
                },
            PRational(ds, ns) =>
                match exactness {
                    Some(false) => RFloat((s_to_int(ds, radix) as f64) / (s_to_int(ns, radix) as f64)),
                    _ => RRational(Rational::new(s_to_int(ds, radix), s_to_int(ns, radix))),
                },
            PFloat(i, f, e) => 
                match exactness {
                    Some(true) => {
                        let exp =
                        if e.is_empty() {
                            0
                        } else {
                            s_to_int(e, 10)
                        };

                        let n:int = num::pow_with_uint(10, f.len());
                        let d = s_to_int(i + f, 10);
                        let p = num::pow_with_uint(10, num::abs(exp) as uint);

                        if(exp < 0) {
                            RRational(Rational::new(d, n*p))
                        } else {
                            RRational(Rational::new(d*p, n))
                        }
                    },
                    _ => {
                        let s = if e.is_empty() { i + "." + f } else { i + "." + f + "e" + e };
                        RFloat(s_to_f64(s))
                    },
                },
        }
    }

    fn parse_exponent(&mut self) -> Option<~str> {
        if self.eof() {
            None
        } else {
            let s =
            match self.lookahead() {
                '+' | '-' => str::from_char(self.consume()),
                _ => ~""
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
    }

    fn parse_decimal(&mut self) -> PResult {
        let (x, xr) = self.parse_radix(10);
        let int_part = x + str::repeat("0", xr);

        if self.eof() {
            if(int_part.is_empty()) {
                PNone
            } else {
                PInt(int_part)
            }
        } else {
            match self.lookahead() {
                '.' => {
                    self.consume();
                    let float_part =
                    if(xr == 0) {
                        let (y, _) = self.parse_radix(10);
                        y
                    } else {
                        while(!self.eof() && self.lookahead() == '#') {
                            self.consume();
                        }
                        ~""
                    };

                    if self.eof() {
                        PFloat(int_part, float_part, ~"")
                    } else {
                        match(self.lookahead()) {
                            'e' | 's' | 'f' | 'd' | 'l' => {
                                self.consume();
                                match self.parse_exponent() {
                                    None => PErr(~"invalid exponent"),
                                    Some(e) => PFloat(int_part, float_part, e),
                                }
                            },
                            _ => PFloat(int_part, float_part, ~"")
                        }
                    }
                },
                'e' | 's' | 'f' | 'd' | 'l' => {
                    self.consume();
                    match self.parse_exponent() {
                        None => PErr(~"invalid exponent"),
                        Some(e) => PFloat(int_part, ~"", e),
                    }
                },
                '/' => {
                    self.consume();
                    let (y, yr) = self.parse_radix(10);
                    if(y.is_empty()) {
                        PErr(~"invalid rational literal")
                    } else {
                        PRational(int_part, y + str::repeat("0", yr))
                    }
                },
                _ => PInt(int_part)
            }
        }
    }

    fn parse_urational(&mut self, r: uint) -> PResult {
        let (x, xr) = self.parse_radix(r);

        if(x.is_empty()) {
            PNone
        } else if(!self.eof() && self.lookahead() == '/') {
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
                while(!self.eof() && self.lookahead() == '#') {
                    sharps += 1u;
                    self.consume();
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

        if(!self.eof() && self.lookahead() == '#') {
            self.consume();

            if(self.eof()) {
                Err(~"unexpected EOF")
            } else {
                match(self.consume()) {
                    'i' if exactness == None => Ok((Some(false), radix)),
                    'e' if exactness == None => Ok((Some(true), radix)),
                    'i' | 'e' => Err(~"multiple exactness prefix"),
                    'b' if radix == None => Ok((exactness, Some(2u))),
                    'o' if radix == None => Ok((exactness, Some(8u))),
                    'd' if radix == None => Ok((exactness, Some(10u))),
                    'x' if radix == None => Ok((exactness, Some(16u))),
                    'b' | 'o' | 'd' | 'x' => Err(~"multiple radix prefix"),
                    x => Err(~"unexpected prefix #" + str::from_char(x)),
                }
            }
        } else {
            Ok((exactness, radix))
        }
    }

    fn parse_ident(&mut self) -> ~str {
        do io::with_str_writer |wr| {
            while(!self.eof()) {
                let c = self.lookahead();

                match(c) {
                    '0' .. '9' | '+' | '-' | '.' | '@' => wr.write_char(c),
                    _ if id_init(c) => wr.write_char(c),
                    _ => break,
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
