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
    /// `[`
    OpenBracket,
    /// `]`
    CloseBracket,
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
    /// `,@`
    UnquoteSplicing,
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
            Token::OpenBracket => write!(f, "OpenBracket"),
            Token::CloseBracket => write!(f, "CloseBracket"),
            Token::Dot => write!(f, "Dot"),
            Token::Quote => write!(f, "Quote"),
            Token::QuasiQuote => write!(f, "QuasiQuote"),
            Token::Comma => write!(f, "Comma"),
            Token::UnquoteSplicing => write!(f, "UnquoteSplicing"),
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
        '\t' | '\n' | '\x0b' | '\x0c' | '\r' | ' ' | '\u{0085}' => true,
        _ => c.is_whitespace()
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
        '(' | ')' | '[' | ']' | '"' | ';' | '#' => true,
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

    pub fn lex_token(&mut self) -> Result<TokenWrapper, ParserError> {
        try!(self.consume_whitespace());

        let line = self.line;
        let col = self.column;

        let token = try!(self.lex());

        try!(self.consume_whitespace());

        Ok(wrap(line, col, token))
    }

    /// return next token
    fn lex(&mut self) -> Result<Token, ParserError> {
        let c = match try!(self.consume_eof()) {
            Some(c) => c,
            None => return Ok(Token::EOF)
        };

        let end_of_token = try!(self.is_end_of_token());

        if is_initial(c) {
            let mut init = String::new();
            init.push(c);
            self.lex_ident(init).map(|s| Token::Identifier(Cow::Owned(s)))
        } else if c == '+' {
            if end_of_token {
                Ok(Token::Identifier(Cow::Borrowed("+")))
            } else {
                self.lex_numeric("+".to_string()).map(Token::Numeric)
            }
        } else if c == '-' {
            if end_of_token {
                Ok(Token::Identifier(Cow::Borrowed("-")))
            } else {
                match try!(self.lookahead()) {
                    Some('>') => self.lex_ident("-".to_string()).map(|s| Token::Identifier(Cow::Owned(s))),
                    Some(_) => self.lex_numeric("-".to_string()).map(Token::Numeric),
                    None => Ok(Token::Identifier(Cow::Borrowed("-")))
                }
            }
        } else if c == ',' {
            match try!(self.lookahead()) {
                Some('@') => {
                    self.consume().expect("lookahead buffer error");
                    Ok(Token::UnquoteSplicing)
                },
                _ => Ok(Token::Comma)
            }
        } else if c == '(' {
            Ok(Token::OpenParen)
        } else if c == ')' {
            Ok(Token::CloseParen)
        } else if c == '[' {
            Ok(Token::OpenBracket)
        } else if c == ']' {
            Ok(Token::CloseBracket)
        } else if c == '.' {
            if end_of_token {
                Ok(Token::Dot)
            } else {
                let rest_prefix = try!(self.read_while(|c| !is_delim(c)));
                if rest_prefix == ".." {
                    Ok(Token::Identifier(Cow::Borrowed("...")))
                } else {
                    Err(self.make_error(ParserErrorKind::InvalidToken(format!("{}{}", c, rest_prefix))))
                }
            }
        } else if c == '\'' {
            Ok(Token::Quote)
        } else if c == '`' {
            Ok(Token::QuasiQuote)
        } else if c == '#' {
            let c0 = try!(self.consume());
            match c0 {
                't' | 'T' => Ok(Token::True),
                'f' | 'F' => Ok(Token::False),
                'b' | 'B' | 'o' | 'O' | 'd' | 'D' | 'x' | 'X' | 'i' | 'I' | 'e' | 'E' => {
                    let s = format!("{}{}", c, c0);
                    self.lex_numeric(s).map(Token::Numeric)
                },
                'v' | 'u' => {
                    let rest_prefix = try!(self.read_while(|c| !is_delim(c)));
                    let delim = try!(self.consume());
                    let prefix = format!("{}{}{}", c0, rest_prefix, delim);
                    match prefix.as_ref() {
                        "vu8(" | "u8(" =>
                            Ok(Token::OpenBytesParen),
                        _ =>
                            Err(self.make_error(ParserErrorKind::InvalidToken(prefix)))
                    }
                },
                '\\' => self.lex_char().map(Token::Character),
                '(' => Ok(Token::OpenVectorParen),
                _ => Err(self.make_error(ParserErrorKind::InvalidToken(format!("#{}", c))))
            }
        } else if c == '"' {
            self.lex_string().map(Token::String)
        } else if c.is_numeric() {
            let s = format!("{}", c);
            self.lex_numeric(s).map(Token::Numeric)
        } else {
            Err(self.make_error(ParserErrorKind::InvalidCharacter(c)))
        }
    }

    fn is_end_of_token(&mut self) -> Result<bool, ParserError> {
        match try!(self.lookahead()) {
            Some(c) => Ok(is_whitespace(c) || is_delim(c)),
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
        let c = try!(self.consume());
        let mut s = String::new();
        s.push(c);
        let sub = try!(self.read_while(|c| c.is_alphanumeric()));
        s.push_str(sub.as_ref());
        return Ok(s);
    }

    fn lex_string(&mut self) -> Result<String, ParserError> {
        let mut s = String::new();
        loop {
            match try!(self.consume()) {
                '"' => return Ok(s),
                '\\' => match try!(self.consume()) {
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
                        let hex_str = try!(self.read_while(|c| c != ';'));
                        if try!(self.consume()) != ';' {
                            panic!("read_while error");
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
                                if let '\n' = try!(self.consume()) {
                                    try!(self.read_while(|c| c != '\n' && c.is_whitespace()));
                                } else {
                                    return Err(self.make_error(ParserErrorKind::InvalidStringLiteral))
                                }
                            }
                        } else {
                            return Err(self.make_error(ParserErrorKind::InvalidStringEscape(format!("{}", c))))
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

    fn lookahead(&mut self) -> Result<Option<char>, CharsError> {
        match self.lookahead_buf {
            Some(c) => Ok(Some(c)),
            None => {
                match self.stream.next() {
                    Some(Ok(ch)) => {
                        self.lookahead_buf = Some(ch);
                        Ok(Some(ch))
                    },
                    Some(Err(e)) => {
                        Err(e)
                    },
                    None => {
                        Ok(None)
                    }
                }
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

    fn consume_eof(&mut self) -> Result<Option<char>, CharsError> {
        let c = match self.lookahead_buf {
            Some(c) => {
                self.lookahead_buf = None;
                Some(Ok(c))
            },
            None => self.stream.next()
        };

        match c {
            Some(Ok(ch)) => {
                self.advance(ch);
                Ok(Some(ch))
            },
            Some(Err(e)) => Err(e),
            None => Ok(None)
        }
    }

    fn consume(&mut self) -> Result<char, ParserError> {
        match try!(self.consume_eof()) {
            Some(c) => Ok(c),
            None => return Err(self.make_error(ParserErrorKind::UnexpectedEOF))
        }
    }

    fn consume_whitespace(&mut self) -> Result<bool, ParserError> {
        let mut consumed = false;
        loop {
            let whitespace = try!(self.read_while(is_whitespace));
            consumed = consumed || whitespace.len() > 0;
            if let Some(';') = try!(self.lookahead()) {
                consumed = true;
                try!(self.read_while(|c| c != '\n'));
                if self.lookahead_buf.is_some() {
                    self.lookahead_buf = None;
                }
            } else {
                return Ok(consumed);
            }
        }
    }
}
