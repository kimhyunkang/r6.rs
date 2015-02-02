use std::mem;
use std::num::from_str_radix;

use phf;
use unicode;

use real::Real;
use number::Number;
use datum::{Datum, cons};
use lexer::{Token, TokenWrapper, Lexer};
use error::{ParserError, ParserErrorKind};

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

fn parse_numeric(rep: &str) -> Option<Number> {
    rep.parse().map(|n| Number::Real(Real::Fixnum(n)))
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
                Some(n) => Ok(Datum::Num(n)),
                None => Err(invalid_token(&tok))
            },
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
    use std::num::FromPrimitive;
    use super::Parser;
    use super::super::datum::{Datum, cons};

    fn test_parse(source: &str, result: Datum<()>) {
        let mut reader = BufReader::new(source.as_bytes());
        let mut parser = Parser::new(&mut reader);

        assert_eq!(parser.parse_datum(), Ok(result))
    }

    #[test]
    fn test_sym() {
        test_parse("lambda", sym!("lambda"));
        test_parse("list->vector", sym!("list->vector"));
        test_parse("->vector", sym!("->vector"));
        test_parse("+", sym!("+"));
    }

    #[test]
    fn test_list() {
        test_parse("()", list!());
        test_parse("(a)", list!(sym!("a")));
        test_parse("(a b)", list!(sym!("a"), sym!("b")));
        test_parse("(a . b)", cons(sym!("a"), sym!("b")));
        test_parse("(a; comment!\nb)", list!(sym!("a"), sym!("b")));
    }

    #[test]
    fn test_simple_datum() {
        test_parse("#t", Datum::Bool(true));
        test_parse("#f", Datum::Bool(false));
        test_parse(r##"#\f"##, Datum::Char('f'));
        test_parse(r##"#\x3f"##, Datum::Char('\x3f'));
        test_parse(r##"#\space"##, Datum::Char(' '));
        test_parse(r##"#\nul"##, Datum::Char('\0'));
    }

    #[test]
    fn test_numeric() {
        let n2 = FromPrimitive::from_int(2).unwrap();
        test_parse("2", Datum::Num(n2));
    }

    #[test]
    fn test_string() {
        test_parse(r#""abc""#, Datum::String("abc".to_string()));
        test_parse(r#""\x41;bc""#, Datum::String("Abc".to_string()));
        test_parse(r#""\x41; bc""#, Datum::String("A bc".to_string()));
        test_parse(r#""\x41bc;""#, Datum::String("\u{41bc}".to_string()));
    }
}
