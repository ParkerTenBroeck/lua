use byteyarn::YarnBox;

use crate::lua::util::sstr::Sstr;

use super::*;



#[derive(Debug, PartialEq, Clone)]
#[repr(align(8))]
pub enum Token<'a> {
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Asterick,
    /// /
    Slash,
    /// %
    Percent,
    /// ^
    Carrot,
    /// #
    Octothorp,
    /// &
    Ampersand,
    /// ~
    Tilde,
    /// |
    Pipe,
    /// <<
    LtLt,
    /// >>
    GtGt,
    /// //
    SlashSlash,
    /// ==
    EqEq,
    /// ~=
    TildeEq,
    /// <=
    LtEq,
    /// >=
    GtEq,
    /// <
    Lt,
    /// >
    Gt,
    /// =
    Eq,

    /// (
    LPar,
    /// )
    RPar,
    /// {
    LBrace,
    /// }
    RBrace,
    /// [
    LBracket,
    /// ]
    RBracket,

    /// ::
    ColonColon,
    /// ;
    Semicolon,
    /// :
    Colon,
    /// ,
    Comma,
    /// .
    Dot,
    /// ..
    DotDot,
    /// ...
    DotDotDot,

    And,
    Break,
    Do,
    Else,
    Elseif,
    End,
    False,
    For,
    Function,
    Goto,
    If,
    In,
    Local,
    Nil,
    Not,
    Or,
    Repeat,
    Return,
    Then,
    True,
    Until,
    While,

    Ident(Sstr<'a>),
    StringLiteral(YarnBox<'a, str>),
    IntegerLiteral(i64),
    FloatingLiteral(f64),

    SingleLineComment(Sstr<'a>),
    MultiLineComment(Sstr<'a>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span<T> {
    pub span: TokenMeta,
    pub val: T,
}

impl<T> Span<T> {
    pub fn new(val: T, span: TokenMeta) -> Self {
        Self { val, span }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct TokenMeta {
    pub line: u32,
    pub col: u32,
    pub offset: u32,
    pub len: u32,
}

impl TokenMeta {
    pub(super) fn start_end(start: Position, end: Position) -> Self {
        TokenMeta {
            line: start.line as u32,
            col: start.col as u32,
            offset: start.offset as u32,
            len: (end.offset - start.offset) as u32,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenizerError<'a> {
    InvalidChar(char),
    EmptyCharLiteral,
    UnclosedCharLiteral,
    CharLiteralTooBig,
    UnclosedMultiLineComment,
    InvalidEscape(&'a str),
    UnfinishedEscapeSequence(&'a str),
    UnclosedStringLiteral,
    EmptyExponent,
    InvalidBase2Digit(char),
    NoNumberAfterBasePrefix,
    // NumberParseError(NumberError),
}
