use std::io::Read;
use std::mem;
use std::fmt::Write;
use std::borrow::Cow;
use std::rc::Rc;
use std::char;

use phf;
use regex::{Regex, Captures};

use real::{Real, rat2flo};
use number::Number;
use datum::{Datum, cons};
use lexer::{Token, TokenWrapper, Lexer};
use error::{ParserError, ParserErrorKind};
use num::{Zero, One, FromPrimitive, Float, Num};
use num::bigint::BigInt;
use num::rational::{Ratio, BigRational};
use num::complex::Complex;

include!(concat!(env!("OUT_DIR"), "/char_map.rs"));

/// Parser parses character stream into a Datum
pub struct Parser<R: Read> {
    lexer: Lexer<R>,
    token_buf: Option<TokenWrapper>,
    number_parser: NumberParser
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

fn parse_char(ch: &str) -> Option<char> {
    match CHAR_MAP.get(ch) {
        Some(c) => return Some(*c),
        None => ()
    };

    let mut chrs = ch.chars();
    if let Some(c) = chrs.next() {
        if let None = chrs.next() {
            return Some(c);
        } else if c == 'x' {
            if let Ok(c) = Num::from_str_radix(&ch[1..], 16) {
                return char::from_u32(c);
            }
        }
    }
    None
}

#[derive(Clone, Copy, PartialEq)]
enum Exactness {
    Exact,
    Inexact,
    Unspecified
}

struct NumberParser {
    prefix_pattern: Regex,
    bin_real_pattern: Regex,
    oct_real_pattern: Regex,
    hex_real_pattern: Regex,
    dec_real_pattern: Regex
}

impl NumberParser {
    fn new() -> NumberParser {
        NumberParser {
            prefix_pattern: Regex::new(r"^(?i)(#([iebodx])){0,2}").unwrap(),
            bin_real_pattern: Regex::new(r"^[+-]?[01]+(/[01]+)?").unwrap(),
            oct_real_pattern: Regex::new(r"^[+-]?[0-7]+(/[0-7]+)?").unwrap(),
            hex_real_pattern: Regex::new(r"^[+-]?[0-9a-fA-F]+(/[0-9a-fA-F]+)?").unwrap(),

            //                              1      23         45               6     7                      8         9        10
            dec_real_pattern: Regex::new(r"^([+-])?((\d+/\d+)|((\d+\.\d*|\.\d+|(\d+))([eEsSfFdDlL][+-]?\d+)?(\|\d+)?)|(nan\.0)|(inf\.0))").unwrap()
        }
    }

    fn parse_numerical_tower(&self, exactness: Exactness, radix: u32, rep: &str) -> Result<Number, String> {
        if rep == "-i" {
            return Ok(Number::new_int(0, -1));
        } else if rep == "+i" {
            return Ok(Number::new_int(0, 1));
        }

        let (re, re_end) = try!(self.parse_real(exactness, radix, rep));

        if re_end == rep.len() {
            return Ok(Number::Real(re));
        }

        match rep[re_end..].chars().next() {
            Some('@') => {
                let arg_part = &rep[re_end+1 ..];
                let (arg, arg_end) = try!(self.parse_real(exactness, radix, arg_part));
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
            Some('i') => Ok(Number::new_imag(re)),
            Some('+') | Some('-') => {
                let im_part = &rep[re_end ..];
                let (im, im_end) = try!(self.parse_real(exactness, radix, im_part));
                if im_end+1 == im_part.len() && im_part[im_end..].chars().next() == Some('i') {
                    if im.is_exact() && im.is_zero() {
                        Ok(Number::Real(re))
                    } else {
                        Ok(Number::new(re, im))
                    }
                } else {
                    Err("Suffix `i` not found at the end of complex literal".to_string())
                }
            },
            _ => Err("Invalid number literal".to_string())
        }
    }

    fn parse_numeric(&self, rep: &str) -> Result<Number, String> {
        let prefix = self.prefix_pattern.captures(rep).unwrap();
        let num_start = match prefix.pos(0) {
            None => 0,
            Some((_, idx)) => idx
        };

        let (exactness, radix) = try!(parse_prefix(&rep[0 .. num_start]));

        self.parse_numerical_tower(exactness, radix, &rep[num_start ..])
    }

    fn parse_real(&self, exactness: Exactness, radix: u32, rep: &str) -> Result<(Real, usize), String> {
        let pattern = match radix {
            2  => &self.bin_real_pattern,
            8  => &self.oct_real_pattern,
            10 => &self.dec_real_pattern,
            16 => &self.hex_real_pattern,
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
            } else if rep.chars().next() == Some('-') {
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
                Real::Rational(r).reduce()
            } else {
                Real::Flonum(rat2flo(&r))
            }
        };

        return Ok((re, re_part.len()));
    }
}

fn parse_prefix(prefix: &str) -> Result<(Exactness, u32), String> {
    let mut exactness = Exactness::Unspecified;
    let mut radix = 0;
    let mut iter = prefix.chars();

    // skip the first letter
    iter.next();

    loop {
        if let Some(c) = iter.next() {
            match c {
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
            iter.next();
        } else {
            break;
        }
    }

    if radix == 0 {
        radix = 10;
    }

    return Ok((exactness, radix));
}

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

fn parse_rational(radix: u32, rep: &str, captures: Captures)
        -> Result<(BigRational, bool), String>
{
    if radix != 10 {
        // Just use Ratio::from_str_radix
        let r: BigRational = match Num::from_str_radix(rep, radix) {
            Ok(r) => r,
            Err(_) => Ratio::from_integer(Num::from_str_radix(rep, radix).unwrap())
        };
        Ok((r, true))
    } else {
        let negative = match captures.at(1) {
            Some("-") => true,
            _ => false
        };

        // Rational
        if let Some(part) = captures.at(3) {
            let abs: BigRational = Num::from_str_radix(part, 10).unwrap();
            let rat = if negative { -abs } else { abs };
            return Ok((rat, true));
        }

        let base: BigInt = FromPrimitive::from_usize(10).unwrap();
        let (mantissa, exactness) = if let Some(part) = captures.at(6) {
            // Integral
            let abs: BigInt = Num::from_str_radix(part, 10).unwrap();
            let int = if negative { -abs } else { abs };
            (Ratio::from_integer(int), true)
        } else if let Some(flt_rep) = captures.at(5) {
            // Floating
            let parts: Vec<&str> = flt_rep.splitn(2, '.').collect();
            let (rep, exp) = match parts.as_slice() {
                &[int_part, flt_part] => {
                    let mut int_rep = String::new();
                    int_rep.write_str(int_part).unwrap();
                    int_rep.write_str(flt_part).unwrap();
                    (int_rep, flt_part.len())
                },
                _ => panic!("Invalid floating point literal `{}`", flt_rep)
            };
            let mantissa: BigInt = Num::from_str_radix(rep.as_ref(), 10).unwrap();
            let denom: BigInt = pow(&base, exp);
            (Ratio::new(mantissa, denom), false)
        } else {
            panic!("Path unreachable")
        };

        let ratio = if let Some(exp_rep) = captures.at(7) {
            if exp_rep.len() > 9 {
                return Err("Exponent too large".to_string());
            }

            let exp:isize = Num::from_str_radix(&exp_rep[1..], 10).unwrap();
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

impl <R: Read + Sized> Parser<R> {
    /// Create new parser from io::Buffer
    pub fn new(stream: R) -> Parser<R> {
        Parser {
            lexer: Lexer::new(stream),
            token_buf: None,
            number_parser: NumberParser::new()
        }
    }

    pub fn parse_full<T>(&mut self) -> Result<Datum<T>, ParserError> {
        let datum = try!(self.parse_datum());
        let t = try!(self.lexer.lex_token());
        if let Token::EOF = t.token {
            Ok(datum)
        } else {
            Err(ParserError {
                line: t.line,
                column: t.column,
                kind: ParserErrorKind::TrailingInput
            })
        }
    }

    /// Parse next datum
    pub fn parse_datum<T>(&mut self) -> Result<Datum<T>, ParserError> {
        let tok = try!(self.consume_token());
        match tok.token {
            Token::Identifier(ident) => Ok(Datum::Sym(ident)),
            Token::OpenParen => self.parse_list(&Token::CloseParen),
            Token::OpenBracket => self.parse_list(&Token::CloseBracket),
            Token::OpenVectorParen => self.parse_vector().map(|v|
                Datum::Vector(Rc::new(v))
            ),
            Token::OpenBytesParen => {
                let v:Vec<Datum<T>> = try!(self.parse_vector());
                let bytes:Result<Vec<u8>, ParserError> = v.iter().map(|d|
                    match d {
                        &Datum::Num(Number::Real(Real::Fixnum(n))) if 0 <= n && n <= 0xff =>
                            Ok(n as u8),
                        _ =>
                            Err(ParserError {
                                line: tok.line,
                                column: tok.column,
                                kind: ParserErrorKind::ByteVectorElement
                            })
                    }).collect();
                bytes.map(|v| Datum::Bytes(Rc::new(v)))
            },
            Token::True => Ok(Datum::Bool(true)),
            Token::False => Ok(Datum::Bool(false)),
            Token::Character(ref ch) => match parse_char(ch.as_ref()) {
                Some(c) => Ok(Datum::Char(c)),
                None => Err(invalid_token(&tok))
            },
            Token::String(s) => Ok(Datum::String(Rc::new(s))),
            Token::Numeric(ref rep) => match self.number_parser.parse_numeric(rep.as_ref()) {
                Ok(n) => Ok(Datum::Num(n)),
                Err(e) => Err(ParserError {
                    line: tok.line,
                    column: tok.column,
                    kind: ParserErrorKind::InvalidToken(format!("{:?}: {}", tok.token, e))
                })
            },
            Token::Quote => self.parse_abbrev("quote"),
            Token::QuasiQuote => self.parse_abbrev("quasiquote"),
            Token::Comma => self.parse_abbrev("unquote"),
            Token::UnquoteSplicing => self.parse_abbrev("unquote-splicing"),
            Token::Syntax => self.parse_abbrev("syntax"),
            Token::QuasiSyntax => self.parse_abbrev("quasisyntax"),
            Token::Unsyntax => self.parse_abbrev("unsyntax"),
            Token::UnsyntaxSplicing => self.parse_abbrev("unsyntax-splicing"),
            Token::DatumComment => {
                // Treat the next datum as a comment
                try!(self.parse_datum::<T>());
                self.parse_datum()
            },
            Token::EOF => {
                Err(ParserError {
                    line: tok.line,
                    column: tok.column,
                    kind: ParserErrorKind::UnexpectedEOF
                })
            },
            _ => Err(unexpected_token(&tok, "Datum or OpenParen".to_string()))
        }
    }

    fn parse_abbrev<T>(&mut self, name: &'static str) -> Result<Datum<T>, ParserError> {
        self.parse_datum().map(|v|
            cons(Datum::Sym(Cow::Borrowed(name)), cons(v, Datum::Nil))
        )
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
        } else if t.token == Token::EOF {
            Err(ParserError {
                line: t.line,
                column: t.column,
                kind: ParserErrorKind::UnexpectedEOF
            })
        } else {
            Err(unexpected_token(&t, format!("{:?}", tok)))
        }
    }

    fn parse_list<T>(&mut self, delim: &Token) -> Result<Datum<T>, ParserError> {
        if try!(self.consume_if(delim)) {
            return Ok(Datum::Nil);
        }

        let head = try!(self.parse_datum());

        if try!(self.consume_if(&Token::Dot)) {
            let tail = try!(self.parse_datum());
            try!(self.expect(delim));
            Ok(cons(head, tail))
        } else {
            let tail = try!(self.parse_list(delim));
            Ok(cons(head, tail))
        }
    }

    fn parse_vector<T>(&mut self) -> Result<Vec<Datum<T>>, ParserError> {
        let mut vec = Vec::new();

        while !try!(self.consume_if(&Token::CloseParen)) {
            vec.push(try!(self.parse_datum()))
        }

        return Ok(vec);
    }
}

#[cfg(test)]
mod test {
    use std::borrow::Cow;
    use std::rc::Rc;

    use num::{Float, FromPrimitive};

    use error::{ParserError, ParserErrorKind};
    use super::Parser;
    use datum::{Datum, cons};
    use number::Number;
    use real::Real;

    macro_rules! test_parse_ok {
        ($s:expr, $e:expr) => ({
            let mut parser = Parser::new($s.as_bytes());
            let res: Result<Datum<()>, ParserError> = parser.parse_full();

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
        let n2 = FromPrimitive::from_isize(2).unwrap();
        test_parse_ok!("2", Datum::Num(n2));
    }

    #[test]
    fn test_string() {
        test_parse_ok!(r#""abc""#, Datum::String(Rc::new("abc".to_string())));
        test_parse_ok!(r#""\x41;bc""#, Datum::String(Rc::new("Abc".to_string())));
        test_parse_ok!(r#""\x41; bc""#, Datum::String(Rc::new("A bc".to_string())));
        test_parse_ok!(r#""\x41bc;""#, Datum::String(Rc::new("\u{41bc}".to_string())));
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
    fn test_vector() {
        test_parse_ok!("#()", Datum::Vector(Rc::new(Vec::new())));
        test_parse_ok!("#(a b)", Datum::Vector(Rc::new(vec![sym!("a"), sym!("b")])));
    }

    #[test]
    fn test_bytes() {
        test_parse_ok!("#vu8()", Datum::Bytes(Rc::new(Vec::new())));
        test_parse_ok!("#vu8(1 2 3)", Datum::Bytes(Rc::new(vec![1, 2, 3])));
    }

    #[test]
    fn test_parse_nan() {
        let mut parser = Parser::new("+nan.0".as_bytes());
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

    #[test]
    fn test_parse_quasiquote() {
        test_parse_ok!("`a", list!(sym!("quasiquote"), sym!("a")));
    }

    #[test]
    fn test_parse_unquote() {
        test_parse_ok!(",a", list!(sym!("unquote"), sym!("a")));
    }

    #[test]
    fn test_parse_unquote_splicing() {
        test_parse_ok!(",@a", list!(sym!("unquote-splicing"), sym!("a")));
    }

    #[test]
    fn test_parse_ellipsis() {
        test_parse_ok!("...", sym!("..."));
    }

    #[test]
    fn test_parse_bracket() {
        test_parse_ok!("[a b (c [d] e) f]",
                    list!(sym!("a"), sym!("b"),
                        list!(sym!("c"), list!(sym!("d")), sym!("e")),
                        sym!("f")
                    ));
    }

    #[test]
    fn test_nested_comment() {
        test_parse_ok!("(a #| (b c #|(d e)|# f) |# g h)", list!(sym!("a"), sym!("g"), sym!("h")));
    }

    #[test]
    fn test_datum_comment() {
        test_parse_ok!("(a #; (b c #|(d e)|# f) g h)", list!(sym!("a"), sym!("g"), sym!("h")));
    }

    #[test]
    fn test_parse_trailing_input() {
        let mut parser = Parser::new("(1 2 3) (4 5)".as_bytes());
        let res: Result<Datum<()>, ParserError> = parser.parse_full();
        match res {
            Ok(datum) => panic!("Expected TrailingInput error, got {:?}", datum),
            Err(e) => match e.kind {
                ParserErrorKind::TrailingInput => (),
                _ => panic!("Expected TrailingInput error, got {}", e)
            }
        }
    }

    #[test]
    fn test_parse_ellipsis_list() {
        test_parse_ok!("(a b ... c)", list!(sym!("a"), sym!("b"), sym!("..."), sym!("c")));
    }
}
