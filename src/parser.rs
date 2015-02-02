use std::mem;
use std::num::{SignedInt, FromPrimitive, Float, from_str_radix};
use std::iter::range_step;
use std::fmt::Writer;
use std::borrow::Cow;

use phf;
use unicode;
use regex::{Regex, Captures};

use real::{Real, rat2flo};
use number::Number;
use datum::{Datum, cons};
use lexer::{Token, TokenWrapper, Lexer};
use error::{ParserError, ParserErrorKind};
use num::{Zero, One};
use num::bigint::BigInt;
use num::rational::{Ratio, BigRational};
use num::complex::Complex;

/// Parser parses character stream into a Datum
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    token_buf: Option<TokenWrapper>
}

fn unexpected_token(tok: &TokenWrapper, expected: String) -> ParserError {
    ParserError {
        line: tok.line,
        column: tok.column,
        kind: ParserErrorKind::UnexpectedToken(format!("{:?}", tok.token), expected)
    }
}

fn invalid_token(tok: &TokenWrapper) -> ParserError {
    ParserError {
        line: tok.line,
        column: tok.column,
        kind: ParserErrorKind::InvalidToken(format!("{:?}", tok.token))
    }
}

static CHAR_MAP: phf::Map<&'static str, char> = phf_map! {
    "nul" => '\0',
    "alarm" => '\x07',
    "backspace" => '\x08',
    "tab" => '\t',
    "newline" => '\n',
    "linefeed" => '\n',
    "vtab" => '\x0b',
    "page" => '\x0c',
    "return" => '\r',
    "esc" => '\x1b',
    "space" => ' ',
    "delete" => '\x7f'
};

fn parse_char(ch: &str) -> Option<char> {
    match CHAR_MAP.get(ch) {
        Some(c) => return Some(*c),
        None => ()
    };

    if ch.chars().count() == 1 {
        Some(ch.char_at(0))
    } else if ch.starts_with("x") {
        from_str_radix(&ch[1..], 16).and_then(|c| unicode::char::from_u32(c))
    } else {
        None
    }
}

#[derive(Copy, PartialEq)]
enum Exactness {
    Exact,
    Inexact,
    Unspecified
}

static PREFIX_PATTERN:Regex = regex!(r"^(?i)(#([iebodx])){0,2}");

fn parse_prefix(prefix: &str) -> Result<(Exactness, usize), String> {
    let mut exactness = Exactness::Unspecified;
    let mut radix = 0;
    for i in range_step(1, prefix.len(), 2) {
        match prefix.char_at(i) {
            'i' | 'I' if exactness == Exactness::Unspecified => {
                exactness = Exactness::Inexact;
            },
            'e' | 'E' if exactness == Exactness::Unspecified => {
                exactness = Exactness::Exact;
            },
            'b' | 'B' if radix == 0 => {
                radix = 2;
            },
            'o' | 'O' if radix == 0 => {
                radix = 8;
            },
            'd' | 'D' if radix == 0 => {
                radix = 10;
            },
            'x' | 'X' if radix == 0 => {
                radix = 16;
            },
            _ => {
                return Err(format!("Invalid number prefix {}", prefix));
            }
        }
    }

    if radix == 0 {
        radix = 10;
    }

    return Ok((exactness, radix));
}

fn parse_numeric(rep: &str) -> Result<Number, String> {
    let prefix = PREFIX_PATTERN.captures(rep).unwrap();
    let num_start = match prefix.pos(0) {
        None => 0,
        Some((_, idx)) => idx
    };

    let (exactness, radix) = try!(parse_prefix(&rep[0 .. num_start]));

    parse_numerical_tower(exactness, radix, &rep[num_start ..])
}

static BIN_REAL_PATTERN:Regex = regex!(r"^[+-]?[01]+(/[01]+)?");
static OCT_REAL_PATTERN:Regex = regex!(r"^[+-]?[0-7]+(/[0-7]+)?");
static HEX_REAL_PATTERN:Regex = regex!(r"^[+-]?[0-9a-fA-F]+(/[0-9a-fA-F]+)?");

//                                         1      23         45               6     7                      8         9        10
static DEC_REAL_PATTERN:Regex = regex!(r"^([+-])?((\d+/\d+)|((\d+\.\d*|\.\d+|(\d+))([eEsSfFdDlL][+-]?\d+)?(\|\d+)?)|(nan\.0)|(inf\.0))");

fn pow(base: &BigInt, exp: usize) -> BigInt {
    if exp == 0 {
        return One::one();
    } else if exp == 1 {
        return base.clone();
    }

    let sqrt = pow(base, exp/2);
    let product = &sqrt * &sqrt;
    if exp % 2 == 0 {
        return product;
    } else {
        return product * base;
    }
}

fn parse_numerical_tower(exactness: Exactness, radix: usize, rep: &str) -> Result<Number, String> {
    if rep == "-i" {
        return Ok(Number::new_int(0, -1));
    } else if rep == "+i" {
        return Ok(Number::new_int(0, 1));
    }

    let (re, re_end) = try!(parse_real(exactness, radix, rep));

    if re_end == rep.len() {
        return Ok(Number::Real(re));
    }

    match rep.char_at(re_end) {
        '@' => {
            let arg_part = &rep[re_end+1 ..];
            let (arg, arg_end) = try!(parse_real(exactness, radix, arg_part));
            if arg_end != arg_part.len() {
                return Err("Invalid polar literal".to_string());
            }
            match exactness {
                Exactness::Exact => if arg.is_zero() {
                        Ok(Number::Real(re))
                    } else {
                        Err("Polar literal cannot be exact".to_string())
                    },
                _ => 
                    Ok(Number::ICmplx(Complex::from_polar(&re.to_f64(), &arg.to_f64())))
            }
        },
        'i' => Ok(Number::new_imag(re)),
        '+' | '-' => {
            let im_part = &rep[re_end ..];
            let (im, im_end) = try!(parse_real(exactness, radix, im_part));
            if im_end+1 == im_part.len() && im_part.char_at(im_end) == 'i' {
                Ok(Number::new(re, im))
            } else {
                Err("Suffix `i` not found at the end of complex literal".to_string())
            }
        },
        _ => Err("Invalid number literal".to_string())
    }
}

fn parse_real(exactness: Exactness, radix: usize, rep: &str) -> Result<(Real, usize), String> {
    let pattern = match radix {
        2  => &BIN_REAL_PATTERN,
        8  => &OCT_REAL_PATTERN,
        10 => &DEC_REAL_PATTERN,
        16 => &HEX_REAL_PATTERN,
        _  => panic!("Invalid radix")
    };

    let captures = match pattern.captures(rep) {
        Some(c) => c,
        None => return Err("Invalid number literal".to_string())
    };

    let re_part = captures.at(0).unwrap();
    let re = if let Some(_) = captures.at(9) {
        // [+-]nan.0
        if exactness == Exactness::Exact {
            return Err("Invalid numeric token: nan.0 can't be exact".to_string());
        } else {
            Real::Flonum(Float::nan())
        }
    } else if let Some(_) = captures.at(10) {
        // [+-]inf.0
        if exactness == Exactness::Exact {
            return Err("Invalid numeric token: inf.0 can't be exact".to_string());
        } else if rep.char_at(0) == '-' {
            Real::Flonum(Float::neg_infinity())
        } else {
            Real::Flonum(Float::infinity())
        }
    } else {
        let (r, default_exactness) = try!(parse_rational(radix, re_part, captures));
        let exact = match exactness {
            Exactness::Exact => true,
            Exactness::Unspecified => default_exactness,
            Exactness::Inexact => false
        };

        if exact {
            Real::Rational(r)
        } else {
            Real::Flonum(rat2flo(&r))
        }
    };

    return Ok((re, re_part.len()));
}

fn parse_rational(radix: usize, rep: &str, captures: Captures)
        -> Result<(BigRational, bool), String>
{
    if radix != 10 {
        // Just use Ratio::from_str_radix
        let r: BigRational = match from_str_radix(rep, radix) {
            Some(r) => r,
            None => Ratio::from_integer(from_str_radix(rep, radix).unwrap())
        };
        Ok((r, true))
    } else {
        let negative = match captures.at(1) {
            Some("-") => true,
            _ => false
        };

        // Rational
        if let Some(part) = captures.at(3) {
            let abs: BigRational = from_str_radix(part, 10).unwrap();
            let rat = if negative { -abs } else { abs };
            return Ok((rat, true));
        }

        let base: BigInt = FromPrimitive::from_uint(10).unwrap();
        let (mantissa, exactness) = if let Some(part) = captures.at(6) {
            // Integral
            let abs: BigInt = from_str_radix(part, 10).unwrap();
            let int = if negative { -abs } else { abs };
            (Ratio::from_integer(int), true)
        } else if let Some(flt_rep) = captures.at(5) {
            // Floating
            let parts: Vec<&str> = flt_rep.splitn(1, '.').collect();
            let (rep, exp) = match parts.as_slice() {
                [int_part, flt_part] => {
                    let mut int_rep = String::new();
                    int_rep.write_str(int_part).unwrap();
                    int_rep.write_str(flt_part).unwrap();
                    (int_rep, flt_part.len())
                },
                _ => panic!("Invalid floating point literal `{}`", flt_rep)
            };
            let mantissa: BigInt = from_str_radix(rep.as_slice(), 10).unwrap();
            let denom: BigInt = pow(&base, exp);
            (Ratio::new(mantissa, denom), false)
        } else {
            panic!("Path unreachable")
        };

        let ratio = if let Some(exp_rep) = captures.at(7) {
            if exp_rep.len() > 9 {
                return Err("Exponent too large".to_string());
            }

            let exp:isize = from_str_radix(&exp_rep[1..], 10).unwrap();
            let exponent = Ratio::from_integer(pow(&base, exp.abs() as usize));
            if exp < 0 {
                mantissa / exponent
            } else {
                mantissa * exponent
            }
        } else {
            mantissa
        };

        Ok((ratio, exactness))
    }
}

impl <'a> Parser<'a> {
    /// Create new parser from io::Buffer
    pub fn new<'r>(stream: &'r mut Buffer) -> Parser<'r> {
        Parser {
            lexer: Lexer::new(stream),
            token_buf: None
        }
    }

    /// Parse next datum
    pub fn parse_datum<T>(&mut self) -> Result<Datum<T>, ParserError> {
        let tok = try!(self.consume_token());
        match tok.token {
            Token::Identifier(ident) => Ok(Datum::Sym(ident)),
            Token::OpenParen => self.parse_list(),
            Token::True => Ok(Datum::Bool(true)),
            Token::False => Ok(Datum::Bool(false)),
            Token::Character(ref ch) => match parse_char(ch.as_slice()) {
                Some(c) => Ok(Datum::Char(c)),
                None => Err(invalid_token(&tok))
            },
            Token::String(s) => Ok(Datum::String(s)),
            Token::Numeric(ref rep) => match parse_numeric(rep.as_slice()) {
                Ok(n) => Ok(Datum::Num(n)),
                Err(e) => Err(ParserError {
                    line: tok.line,
                    column: tok.column,
                    kind: ParserErrorKind::InvalidToken(format!("{:?}: {}", tok.token, e))
                })
            },
            Token::Quote => self.parse_datum().map(|v|
                cons(Datum::Sym(Cow::Borrowed("quote")), cons(v, Datum::Nil))
            ),
            _ => Err(unexpected_token(&tok, "Datum or OpenParen".to_string()))
        }
    }

    fn consume_token(&mut self) -> Result<TokenWrapper, ParserError> {
        let mut tok = None;
        mem::swap(&mut self.token_buf, &mut tok);
        match tok {
            Some(t) => Ok(t),
            None => self.lexer.lex_token()
        }
    }

    fn lookahead_token<'t>(&'t mut self) -> Result<&'t TokenWrapper, ParserError> {
        if self.token_buf.is_none() {
            self.token_buf = Some(try!(self.lexer.lex_token()));
        }

        Ok(self.token_buf.as_ref().unwrap())
    }

    fn consume_if(&mut self, tok: &Token) -> Result<bool, ParserError> {
        let res = self.lookahead_token().map(|t| t.token == *tok);
        if res == Ok(true) {
            self.token_buf = None;
        }
        res
    }

    fn expect(&mut self, tok: &Token) -> Result<(), ParserError> {
        let t = try!(self.consume_token());
        if t.token == *tok {
            Ok(())
        } else {
            Err(unexpected_token(&t, format!("{:?}", tok)))
        }
    }

    fn parse_list<T>(&mut self) -> Result<Datum<T>, ParserError> {
        if try!(self.consume_if(&Token::CloseParen)) {
            return Ok(Datum::Nil);
        }

        let head = try!(self.parse_datum());

        if try!(self.consume_if(&Token::Dot)) {
            let tail = try!(self.parse_datum());
            try!(self.expect(&Token::CloseParen));
            Ok(cons(head, tail))
        } else {
            let tail = try!(self.parse_list());
            Ok(cons(head, tail))
        }
    }
}

#[cfg(test)]
mod test {
    use std::old_io::BufReader;
    use std::borrow::Cow;
    use std::num::{Float, FromPrimitive};
    use error::ParserError;
    use super::Parser;
    use datum::{Datum, cons};
    use number::Number;
    use real::Real;

    macro_rules! test_parse_ok {
        ($s:expr, $e:expr) => ({
            let mut reader = BufReader::new($s.as_bytes());
            let mut parser = Parser::new(&mut reader);
            let res: Result<Datum<()>, ParserError> = parser.parse_datum();

            assert_eq!(res, Ok($e))
        })
    }

    #[test]
    fn test_sym() {
        test_parse_ok!("lambda", sym!("lambda"));
        test_parse_ok!("list->vector", sym!("list->vector"));
        test_parse_ok!("->vector", sym!("->vector"));
        test_parse_ok!("+", sym!("+"));
    }

    #[test]
    fn test_end_of_list() {
        test_parse_ok!("(+)", list!(sym!("+")));
    }

    #[test]
    fn test_list() {
        test_parse_ok!("()", list!());
        test_parse_ok!("(a)", list!(sym!("a")));
        test_parse_ok!("(a b)", list!(sym!("a"), sym!("b")));
        test_parse_ok!("(a . b)", cons(sym!("a"), sym!("b")));
        test_parse_ok!("(a; comment!\nb)", list!(sym!("a"), sym!("b")));
    }

    #[test]
    fn test_simple_datum() {
        test_parse_ok!("#t", Datum::Bool(true));
        test_parse_ok!("#f", Datum::Bool(false));
        test_parse_ok!(r##"#\f"##, Datum::Char('f'));
        test_parse_ok!(r##"#\x3f"##, Datum::Char('\x3f'));
        test_parse_ok!(r##"#\space"##, Datum::Char(' '));
        test_parse_ok!(r##"#\nul"##, Datum::Char('\0'));
    }

    #[test]
    fn test_numeric() {
        let n2 = FromPrimitive::from_int(2).unwrap();
        test_parse_ok!("2", Datum::Num(n2));
    }

    #[test]
    fn test_string() {
        test_parse_ok!(r#""abc""#, Datum::String("abc".to_string()));
        test_parse_ok!(r#""\x41;bc""#, Datum::String("Abc".to_string()));
        test_parse_ok!(r#""\x41; bc""#, Datum::String("A bc".to_string()));
        test_parse_ok!(r#""\x41bc;""#, Datum::String("\u{41bc}".to_string()));
    }

    #[test]
    fn test_numerical_tower() {
        test_parse_ok!("3.141592F0", Datum::Num(Number::new_flonum(3.141592)));
        test_parse_ok!("1.0", Datum::Num(Number::new_flonum(1.0)));
        test_parse_ok!("#e1.0", Datum::Num(Number::Real(Real::Fixnum(1))));
        test_parse_ok!("1/2", Datum::Num(Number::new_ratio(1, 2)));
        test_parse_ok!("#i1/2", Datum::Num(Number::Real(Real::Flonum(0.5))));
        test_parse_ok!("-i", Datum::Num(Number::new_int(0, -1)));
        test_parse_ok!("+i", Datum::Num(Number::new_int(0, 1)));
        test_parse_ok!("#e1+2i", Datum::Num(Number::new_int(1, 2)));
        test_parse_ok!("#i1+2i", Datum::Num(Number::new_inexact(1.0, 2.0)));
        test_parse_ok!("#e-1-2i", Datum::Num(Number::new_int(-1, -2)));
        test_parse_ok!("#e-1/2-2/3i", Datum::Num(Number::new_exact((-1, 2), (-2, 3))));
        test_parse_ok!("#i+1/2-1/4i", Datum::Num(Number::new_inexact(0.5, -0.25)));
        test_parse_ok!("+inf.0", Datum::Num(Number::Real(Real::Flonum(Float::infinity()))));
        test_parse_ok!("-inf.0", Datum::Num(Number::Real(Real::Flonum(Float::neg_infinity()))));
    }

    #[test]
    fn test_parse_nan() {
        let mut reader = BufReader::new("+nan.0".as_bytes());
        let mut parser = Parser::new(&mut reader);
        let res: Result<Datum<()>, ParserError> = parser.parse_datum();

        match res {
            Ok(Datum::Num(Number::Real(Real::Flonum(n)))) => if !n.is_nan() {
                panic!("Expected `Ok(NaN)`, but found {:?}", res)
            },
            _ => panic!("Expected `Ok(NaN)`, but found {:?}", res)
        }
    }

    #[test]
    fn test_parse_quote() {
        test_parse_ok!("'a", list!(sym!("quote"), sym!("a")));
    }
}
