use std::io::CharsError;
use std::error::Error;
use std::fmt;

use compiler::Syntax;

/// Possible parser errors
#[derive(Debug, PartialEq)]
pub enum ParserErrorKind {
    /// Parser met EOF before parsing a proper datum
    UnexpectedEOF,
    /// Unexpected token: the first string describes expected token, and the second describes
    /// actual token
    UnexpectedToken(String, String),
    /// Lexer met character not allowed in source code
    InvalidCharacter(char),
    /// Lexer met unknown escape character
    InvalidStringEscape(String),
    /// Lexer met non-unicode character codepoint
    InvalidUnicodeRange(u32),
    /// Lexer met non-unicode character codepoint
    InvalidStringLiteral,
    /// Parser met un-parseable token
    InvalidToken(String),
    /// Non-byte datum found in byte vector representation
    ByteVectorElement,
    /// Parser met CharsError while reading the underlying stream
    UnderlyingError(StreamError)
}

#[derive(Debug)]
pub struct StreamError(pub CharsError);

impl PartialEq for StreamError {
    fn eq(&self, _other: &StreamError) -> bool {
        false
    }
}

/// Parser error
#[derive(Debug, PartialEq)]
pub struct ParserError {
    pub line: usize,
    pub column: usize,
    pub kind: ParserErrorKind,
}

impl Error for ParserError {
    fn description(&self) -> &str {
        ""
    }

    fn cause(&self) -> Option<&Error> {
        match self.kind {
            ParserErrorKind::UnderlyingError(StreamError(ref e)) => Some(e),
            _ => None
        }
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "line {:?}, column {:?}: {:?}", self.line, self.column, self.kind)
    }
}

impl From<CharsError> for ParserError {
    fn from(err: CharsError) -> ParserError {
        ParserError {
            line: 0,
            column: 0,
            kind: ParserErrorKind::UnderlyingError(StreamError(err))
        }
    }
}

/// Possible compiler errors
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CompileErrorKind {
    /// The syntax is not implemented yet
    NotImplemented,
    /// Trying to evaluate `()`
    NullEval,
    /// Trying to evaluate non-proper list, such as `(a b c . d)`
    DottedEval,
    /// Expression body is non-proper list, such as `(a b c . d)`
    DottedBody,
    /// Expression body is `()`
    EmptyBody,
    /// Invalid basic syntax
    BadSyntax,
    /// Trying to apply non-function constant
    NotCallable,
    /// Trying to refer a syntax variable
    SyntaxReference(Syntax),
    /// `define` is not allowed in this context
    DefineContext,
    /// `unquote` or its variants are not allowed in this context
    UnquoteContext,
    /// Trying to refer an unbound variable
    UnboundVariable
}

/// Compiler error
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct CompileError {
    pub kind: CompileErrorKind
}

/// Errors raised in runtime
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum RuntimeErrorKind {
    /// Fatal non-recoverable error
    Panic,
    /// Number of arguments did not match
    NumArgs,
    /// Argument type did not match
    InvalidType,
    /// Divide by zero
    DivideByZero,
    /// Index out of range
    IndexOutOfRange,
    /// Compile error
    CompileError
}

/// Errors raised in runtime
#[derive(Debug, PartialEq, Clone)]
pub struct RuntimeError {
    pub kind: RuntimeErrorKind,
    pub desc: String
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}: {}", self.kind, self.desc)
    }
}
