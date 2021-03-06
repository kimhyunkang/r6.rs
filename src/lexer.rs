use std::io::{Read, Chars, CharsError};
use std::iter::Peekable;
use std::borrow::Cow;
use std::fmt;
use std::char;

use num::Num;
use unicode_categories::UnicodeCategories;

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
    /// `#;`
    DatumComment,
    /// `#'`
    Syntax,
    /// `#``
    QuasiSyntax,
    /// `#,`
    Unsyntax,
    /// `#,@`
    UnsyntaxSplicing,
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
            Token::Syntax => write!(f, "Syntax"),
            Token::QuasiSyntax => write!(f, "QuasiSyntax"),
            Token::Unsyntax => write!(f, "Unsyntax"),
            Token::UnsyntaxSplicing => write!(f, "UnsyntaxSplicing"),
            Token::Identifier(ref name) => write!(f, "Identifier({})", name),
            Token::True => write!(f, "#t"),
            Token::False => write!(f, "#f"),
            Token::Character(ref name) => write!(f, "#\\{}", name),
            Token::DatumComment => write!(f, "DatumComment"),
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
        _ => c > '\x7f' && (
            c.is_letter()                   // Lu, Ll, Lt, Lm, Lo
            || c.is_mark_nonspacing()       // Mn
            || c.is_number_letter()         // Nl
            || c.is_number_other()          // No
            || c.is_punctuation_dash()      // Pd
            || c.is_punctuation_connector() // Pc
            || c.is_punctuation_other()     // Po
            || c.is_symbol()                // Sc, Sm, Sk, So
            || c.is_other_private_use()     // Co
        )
    }
}

fn is_subsequent(c: char) -> bool {
    if is_initial(c) {
        true
    } else {
        match c {
            '0'...'9' | '+' | '-' | '.' | '@' => true,
            _ => (
                c.is_number_decimal_digit()         // Nd
                || c.is_mark_spacing_combining()    // Mc
                || c.is_mark_enclosing()            // Me
            )
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
pub struct Lexer<R: Read> {
    line: usize,
    column: usize,
    stream: Peekable<Chars<R>>
}

impl <R: Read + Sized> Lexer<R> {
    /// Creates new Lexer from io::Buffer
    pub fn new(stream: R) -> Lexer<R> {
        Lexer {
            line: 1,
            column: 1,
            stream: stream.chars().peekable()
        }
    }

    pub fn lex_token(&mut self) -> Result<TokenWrapper, ParserError> {
        self.consume_whitespace()?;

        let line = self.line;
        let col = self.column;

        let token = self.lex()?;

        self.consume_whitespace()?;

        Ok(wrap(line, col, token))
    }

    /// return next token
    fn lex(&mut self) -> Result<Token, ParserError> {
        let c = match self.consume_eof()? {
            Some(c) => c,
            None => return Ok(Token::EOF)
        };

        let end_of_token = self.is_end_of_token()?;

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
                match self.lookahead()? {
                    Some('>') => self.lex_ident("-".to_string()).map(|s| Token::Identifier(Cow::Owned(s))),
                    Some(_) => self.lex_numeric("-".to_string()).map(Token::Numeric),
                    None => Ok(Token::Identifier(Cow::Borrowed("-")))
                }
            }
        } else if c == ',' {
            match self.lookahead()? {
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
                let rest_prefix = self.read_while(|c| !is_delim(c))?;
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
            let c0 = self.consume()?;
            match c0 {
                't' | 'T' => {
                    self.expect_delimiter()?;
                    Ok(Token::True)
                },
                'f' | 'F' => {
                    self.expect_delimiter()?;
                    Ok(Token::False)
                },
                'b' | 'B' | 'o' | 'O' | 'd' | 'D' | 'x' | 'X' | 'i' | 'I' | 'e' | 'E' => {
                    let s = format!("{}{}", c, c0);
                    self.lex_numeric(s).map(Token::Numeric)
                },
                'v' | 'u' => {
                    let rest_prefix = self.read_while(|c| !is_delim(c))?;
                    let delim = self.consume()?;
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
                '|' => {
                    self.consume_nested_comment()?;
                    self.consume_whitespace()?;
                    self.lex()
                },
                ';' => Ok(Token::DatumComment),
                '\'' => Ok(Token::Syntax),
                '`' => Ok(Token::QuasiSyntax),
                ',' => if let Some('@') = self.lookahead()? {
                        self.consume().expect("lookahead buffer error");
                        Ok(Token::UnsyntaxSplicing)
                    } else {
                        Ok(Token::Unsyntax)
                    },
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
        match self.lookahead()? {
            Some(c) => Ok(is_whitespace(c) || is_delim(c)),
            None => Ok(true)
        }
    }

    fn lex_ident(&mut self, initial: String) -> Result<String, ParserError> {
        let mut s = initial;
        let sub = self.read_while(is_subsequent)?;
        self.expect_delimiter()?;
        s.push_str(sub.as_ref());
        Ok(s)
    }

    fn lex_char(&mut self) -> Result<String, ParserError> {
        let c = self.consume()?;
        let mut s = String::new();
        s.push(c);
        let sub = self.read_while(|c| c.is_alphanumeric())?;
        self.expect_delimiter()?;
        s.push_str(sub.as_ref());
        return Ok(s);
    }

    fn lex_string(&mut self) -> Result<String, ParserError> {
        let mut s = String::new();
        loop {
            match self.consume()? {
                '"' => return Ok(s),
                '\\' => match self.consume()? {
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
                        let hex_str = self.read_while(|c| c != ';')?;
                        if self.consume()? != ';' {
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
                            self.read_while(|c| c != '\n' && c.is_whitespace())?;
                            if c != '\n' {
                                if let '\n' = self.consume()? {
                                    self.read_while(|c| c != '\n' && c.is_whitespace())?;
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
        let sub = self.read_while(|c| !is_delim(c))?;
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
        match self.stream.peek() {
            Some(&Ok(c)) => Ok(Some(c)),
            Some(&Err(_)) => Err(self.stream.next().unwrap().unwrap_err()),
            None => Ok(None)
        }
    }

    fn line_count(&mut self, c: char) {
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
        let mut s = String::new();

        loop {
            match self.stream.peek() {
                Some(&Ok(c)) => if f(c) {
                    self.line_count(c);
                    s.push(c);
                    self.stream.next();
                } else {
                    return Ok(s);
                },
                Some(&Err(_)) => {
                    let e = self.stream.next().unwrap().unwrap_err();
                    return Err(self.make_error(ParserErrorKind::UnderlyingError(StreamError(e))));
                },
                None => return Ok(s)
            }
        }
    }

    fn expect_delimiter(&mut self) -> Result<(), ParserError> {
        if self.is_end_of_token()? {
            Ok(())
        } else {
            Err(self.make_error(ParserErrorKind::ExpectedDelimiter))
        }
    }

    fn consume_eof(&mut self) -> Result<Option<char>, CharsError> {
        match self.stream.peek() {
            Some(&Ok(ch)) => {
                self.line_count(ch);
                self.stream.next();
                Ok(Some(ch))
            },
            Some(&Err(_)) => Err(self.stream.next().unwrap().unwrap_err()),
            None => Ok(None)
        }
    }

    fn consume(&mut self) -> Result<char, ParserError> {
        match self.consume_eof()? {
            Some(c) => Ok(c),
            None => return Err(self.make_error(ParserErrorKind::UnexpectedEOF))
        }
    }

    fn consume_whitespace(&mut self) -> Result<(), ParserError> {
        loop {
            self.read_while(is_whitespace)?;
            if let Some(';') = self.lookahead()? {
                self.read_while(|c| c != '\n')?;
            } else {
                return Ok(());
            }
        }
    }

    fn consume_nested_comment(&mut self) -> Result<(), ParserError> {
        loop {
            match self.consume()? {
                '|' => if '#' == self.consume()? {
                    return Ok(());
                },
                '#' => if '|' == self.consume()? {
                    self.consume_nested_comment()?;
                },
                _ => ()
            }
        }
    }
}
