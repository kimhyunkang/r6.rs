use std::io::IoError;
use std::error::{Error, FromError};

#[derive(Show, PartialEq)]
pub enum ParserErrorKind {
    UnexpectedEOF,
    UnexpectedToken(String, String),
    InvalidCharacter(char),
    InvalidToken(String),
    UnderlyingError(IoError)
}

#[derive(Show, PartialEq)]
pub struct ParserError {
    pub line: usize,
    pub column: usize,
    pub kind: ParserErrorKind,
}

impl Error for ParserError {
    fn description(&self) -> &str {
        ""
    }

    fn detail(&self) -> Option<String> {
        None
    }

    fn cause(&self) -> Option<&Error> {
        match self.kind {
            ParserErrorKind::UnderlyingError(ref e) => Some(e as &Error),
            _ => None
        }
    }
}

impl FromError<IoError> for ParserError {
    fn from_error(err: IoError) -> ParserError {
        ParserError {
            line: 0,
            column: 0,
            kind: ParserErrorKind::UnderlyingError(err)
        }
    }
}

#[derive(Show, PartialEq, Copy)]
pub enum CompileErrorKind {
    NotImplemented,
    NullEval,
    DottedEval,
    DottedBody,
    BadLambdaSyntax,
    NotCallable,
    SyntaxReference,
    UnboundVariable
}

#[derive(Show, PartialEq, Copy)]
pub struct CompileError {
    pub kind: CompileErrorKind
}

#[derive(Show, PartialEq, Copy, Clone)]
pub enum RuntimeErrorKind {
    NumArgs,
    InvalidType
}

#[derive(Show, PartialEq, Clone)]
pub struct RuntimeError {
    pub kind: RuntimeErrorKind,
    pub desc: String
}
