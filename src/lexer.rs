use std::io::{Read, Chars, CharsError};
use std::borrow::Cow;
use std::fmt;
use std::char;

use num::Num;

use error::{ParserError, ParserErrorKind, StreamError};

/// Token types
#[derive(PartialEq)]
pub enum Token {
    /// `(`
    OpenParen,
    /// `)`
    CloseParen,
    /// `#(`
    OpenVectorParen,
    /// `#vu8(`
    OpenBytesParen,
    /// `.`
    Dot,
    /// `'`
    Quote,
    /// `\``
    QuasiQuote,
    /// `,`
    Comma,
    Identifier(Cow<'static, str>),
    /// `#t`
    True,
    /// `#f`
    False,
    /// `#\<String>`
    Character(String),
    String(String),
    Numeric(String),
    /// End of character stream
    EOF
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::OpenParen => write!(f, "OpenParen"),
            Token::OpenVectorParen => write!(f, "OpenVectorParen"),
            Token::OpenBytesParen => write!(f, "OpenBytesParen"),
            Token::CloseParen => write!(f, "CloseParen"),
            Token::Dot => write!(f, "Dot"),
            Token::Quote => write!(f, "Quote"),
            Token::QuasiQuote => write!(f, "QuasiQuote"),
            Token::Comma => write!(f, "Comma"),
            Token::Identifier(ref name) => write!(f, "Identifier({})", name),
            Token::True => write!(f, "#t"),
            Token::False => write!(f, "#f"),
            Token::Character(ref name) => write!(f, "#\\{}", name),
            Token::Numeric(ref rep) => rep.fmt(f),
            Token::String(ref rep) => write!(f, "{:?}", rep),
            Token::EOF => write!(f, "EOF"),
        }
    }
}

/// TokenWrapper provides positional information to each token
pub struct TokenWrapper {
    pub line: usize,
    pub column: usize,
    pub token: Token
}

fn wrap(line: usize, column: usize, t: Token) -> TokenWrapper {
    TokenWrapper {
        line: line,
        column: column,
        token: t
    }
}


fn is_whitespace(c: char) -> bool {
    match c {
        '\t' | '\n' | '\x0b' | '\x0c' | '\r' | ' ' => true,
        _ => false
    }
}

fn is_initial(c: char) -> bool {
    match c {
        'a'...'z' | 'A'...'Z' | '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '^' | '_' | '~' => true,
        _ => false
    }
}

fn is_subsequent(c: char) -> bool {
    if is_initial(c) {
        true
    } else {
        match c {
            '0'...'9' | '+' | '-' | '.' | '@' => true,
            _ => false
        }
    }
}

fn is_delim(c: char) -> bool {
    match c {
        '(' | ')' | '[' | ']' | '"' | ';' => true,
        _ => c.is_whitespace()
    }
}

/// Lexer transforms character stream into a token stream
pub struct Lexer<R> {
    line: usize,
    column: usize,
    stream: Chars<R>,
    lookahead_buf: Option<char>,
}

macro_rules! try_consume {
    ($this:ident) => (
        match $this.consume() {
            Some(Ok(c)) => c,
            Some(Err(e)) => return Err($this.make_error(ParserErrorKind::UnderlyingError(StreamError(e)))),
            None => return Err($this.make_error(ParserErrorKind::UnexpectedEOF))
        }
    )
}

impl <R: Read + Sized> Lexer<R> {
    /// Creates new Lexer from io::Buffer
    pub fn new(stream: R) -> Lexer<R> {
        Lexer {
            line: 1,
            column: 1,
            stream: stream.chars(),
            lookahead_buf: None,
        }
    }

    /// return next token
    pub fn lex_token(&mut self) -> Result<TokenWrapper, ParserError> {
        try!(self.consume_whitespace());

        let line = self.line;
        let col = self.column;
        let c = match self.consume() {
            Some(Ok(c)) => c,
            None => return Ok(wrap(line, col, Token::EOF)),
            Some(Err(e)) =>
                return Err(self.make_error(ParserErrorKind::UnderlyingError(StreamError(e))))
        };

        let end_of_token = try!(self.is_end_of_token());

        if is_initial(c) {
            let mut init = String::new();
            init.push(c);
            self.lex_ident(init).map(|s| wrap(line, col, Token::Identifier(Cow::Owned(s))))
        } else if c == '+' {
            if end_of_token {
                Ok(wrap(line, col, Token::Identifier(Cow::Borrowed("+"))))
            } else {
                self.lex_numeric("+".to_string()).map(|s| wrap(line, col, Token::Numeric(s)))
            }
        } else if c == '-' {
            if end_of_token {
                Ok(wrap(line, col, Token::Identifier(Cow::Borrowed("-"))))
            } else {
                match self.lookahead() {
                    Some(Ok('>')) => self.lex_ident("-".to_string()).map(|s| wrap(line, col, Token::Identifier(Cow::Owned(s)))),
                    Some(Ok(_)) => self.lex_numeric("-".to_string()).map(|s| wrap(line, col, Token::Numeric(s))),
                    Some(Err(e)) => Err(self.make_error(ParserErrorKind::UnderlyingError(StreamError(e)))),
                    None => Ok(wrap(line, col, Token::Identifier(Cow::Borrowed("-"))))
                }
            }
        } else if c == '(' {
            Ok(wrap(line, col, Token::OpenParen))
        } else if c == ')' {
            Ok(wrap(line, col, Token::CloseParen))
        } else if c == '.' && end_of_token {
            Ok(wrap(line, col, Token::Dot))
        } else if c == '\'' {
            Ok(wrap(line, col, Token::Quote))
        } else if c == '`' {
            Ok(wrap(line, col, Token::QuasiQuote))
        } else if c == ',' {
            Ok(wrap(line, col, Token::Comma))
        } else if c == '#' {
            let c0 = try_consume!(self);
            match c0 {
                't' | 'T' => Ok(wrap(line, col, Token::True)),
                'f' | 'F' => Ok(wrap(line, col, Token::False)),
                'b' | 'B' | 'o' | 'O' | 'd' | 'D' | 'x' | 'X' | 'i' | 'I' | 'e' | 'E' => {
                    let s = format!("{}{}", c, c0);
                    self.lex_numeric(s).map(|s| wrap(line, col, Token::Numeric(s)))
                },
                'v' | 'u' => {
                    let rest_prefix = try!(self.read_while(|c| !is_delim(c)));
                    let delim = try_consume!(self);
                    let prefix = format!("{}{}{}", c0, rest_prefix, delim);
                    match prefix.as_ref() {
                        "vu8(" | "u8(" =>
                            Ok(wrap(line, col, Token::OpenBytesParen)),
                        _ =>
                            Err(self.make_error(ParserErrorKind::InvalidToken(prefix)))
                    }
                },
                '\\' => self.lex_char().map(|s| wrap(line, col, Token::Character(s))),
                '(' => Ok(wrap(line, col, Token::OpenVectorParen)),
                _ => Err(self.make_error(ParserErrorKind::InvalidCharacter(c)))
            }
        } else if c == '"' {
            self.lex_string().map(|s| wrap(line, col, Token::String(s)))
        } else if c.is_numeric() {
            let s = format!("{}", c);
            self.lex_numeric(s).map(|s| wrap(line, col, Token::Numeric(s)))
        } else {
            Err(self.make_error(ParserErrorKind::InvalidCharacter(c)))
        }
    }

    fn is_end_of_token(&mut self) -> Result<bool, ParserError> {
        match self.lookahead() {
            Some(Ok(c)) => Ok(is_whitespace(c) || is_delim(c)),
            Some(Err(e)) => Err(self.make_error(ParserErrorKind::UnderlyingError(StreamError(e)))),
            None => Ok(true)
        }
    }

    fn lex_ident(&mut self, initial: String) -> Result<String, ParserError> {
        let mut s = initial;
        let sub = try!(self.read_while(is_subsequent));
        s.push_str(sub.as_ref());
        return Ok(s);
    }

    fn lex_char(&mut self) -> Result<String, ParserError> {
        let c = try_consume!(self);

        let mut s = String::new();
        s.push(c);
        let sub = try!(self.read_while(|c| c.is_alphanumeric()));
        s.push_str(sub.as_ref());
        return Ok(s);
    }

    fn lex_string(&mut self) -> Result<String, ParserError> {
        let mut s = String::new();
        loop {
            match try_consume!(self) {
                '"' => return Ok(s),
                '\\' => match try_consume!(self) {
                    'a' => s.push('\x07'),
                    'b' => s.push('\x08'),
                    't' => s.push('\t'),
                    'n' => s.push('\n'),
                    'v' => s.push('\x0b'),
                    'f' => s.push('\x0c'),
                    'r' => s.push('\r'),
                    '"' => s.push('"'),
                    '\\' => s.push('\\'),
                    'x' => {
                        let mut hex_str = String::new();
                        loop {
                            match try_consume!(self) {
                                ';' => break,
                                c => hex_str.push(c)
                            }
                        }
                        if hex_str.len() == 0 {
                            return Err(self.make_error(ParserErrorKind::InvalidStringEscape(hex_str)))
                        }
                        let code = match Num::from_str_radix(hex_str.as_ref(), 16) {
                            Ok(n) => n,
                            Err(_) => return Err(self.make_error(ParserErrorKind::InvalidStringEscape(hex_str)))
                        };
                        match char::from_u32(code) {
                            Some(c) => s.push(c),
                            None => return Err(self.make_error(ParserErrorKind::InvalidUnicodeRange(code)))
                        }
                    },
                    c =>
                        if c.is_whitespace() {
                            try!(self.read_while(|c| c != '\n' && c.is_whitespace()));
                            if c != '\n' {
                                if let Some(Ok('\n')) = self.consume() {
                                    try!(self.read_while(|c| c != '\n' && c.is_whitespace()));
                                } else {
                                    return Err(self.make_error(ParserErrorKind::InvalidStringLiteral))
                                }
                            }
                        } else {
                            let mut s = String::new();
                            s.push(c);
                            return Err(self.make_error(ParserErrorKind::InvalidStringEscape(s)))
                        }
                },
                c => s.push(c),
            }
        }
    }

    fn lex_numeric(&mut self, init: String) -> Result<String, ParserError> {
        let mut s = init;
        let sub = try!(self.read_while(|c| !is_delim(c)));
        s.push_str(sub.as_ref());
        return Ok(s);
    }

    fn make_error(&self, kind: ParserErrorKind) -> ParserError {
        ParserError {
            line: self.line,
            column: self.column,
            kind: kind
        }
    }

    fn lookahead(&mut self) -> Option<Result<char, CharsError>> {
        match self.lookahead_buf {
            Some(c) => Some(Ok(c)),
            None => {
                let c = self.stream.next();
                if let Some(Ok(ch)) = c {
                    self.lookahead_buf = Some(ch);
                }
                c
            }
        }
    }

    fn advance(&mut self, c: char) {
        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
    }

    fn read_while<F>(&mut self, f: F) -> Result<String, ParserError> where
        F: Fn(char) -> bool
    {
        let mut s = match self.lookahead_buf {
            None => String::new(),
            Some(c) => if f(c) {
                self.lookahead_buf = None;
                self.advance(c);
                let mut s = String::new();
                s.push(c);
                s
            } else {
                return Ok(String::new());
            }
        };

        loop {
            match self.stream.next() {
                Some(Ok(c)) => if f(c) {
                    self.advance(c);
                    s.push(c);
                } else {
                    self.lookahead_buf = Some(c);
                    return Ok(s);
                },
                Some(Err(e)) =>
                    return Err(self.make_error(ParserErrorKind::UnderlyingError(StreamError(e)))),
                None => return Ok(s)
            }
        }
    }

    fn consume(&mut self) -> Option<Result<char, CharsError>> {
        let c = match self.lookahead_buf {
            Some(c) => {
                self.lookahead_buf = None;
                Some(Ok(c))
            },
            None => self.stream.next()
        };

        if let Some(Ok(ch)) = c {
            self.advance(ch);
        }

        c
    }

    fn consume_whitespace(&mut self) -> Result<bool, ParserError> {
        let mut consumed = false;
        loop {
            let whitespace = try!(self.read_while(is_whitespace));
            consumed = consumed || whitespace.len() > 0;
            match self.lookahead() {
                Some(Ok(';')) => {
                    consumed = true;
                    try!(self.read_while(|c| c != '\n'));
                    if self.lookahead_buf.is_some() {
                        self.lookahead_buf = None
                    }
                },
                Some(Ok(_)) | None => return Ok(consumed),
                Some(Err(e)) =>
                    return Err(self.make_error(ParserErrorKind::UnderlyingError(StreamError(e)))),
            }
        }
    }
}
