use std::{fmt::Write, iter::Peekable, str::Chars};

use str_buf::StrBuf;

use self::token::Token;

pub mod token;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(super) struct Position {
    offset: usize,
    line: usize,
    col: usize,
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

    fn start_size(start: Position, len: u32) -> Self {
        Self {
            line: start.line as u32,
            col: start.col as u32,
            offset: start.offset as u32,
            len,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    MalformedString(byteyarn::YarnBox<'a, str>),
    ParseIntError(std::num::ParseIntError),
    ParseFloatError(std::num::ParseFloatError),
    IntegerTooLargeError,
}

enum InvalidString {
    Valid,
    InvalidNewline,
}

type TokenizerResult<'a> = Result<Span<Token<'a>>, Box<Span<TokenizerError<'a>>>>;

pub struct Tokenizer<'a> {
    str: &'a str,
    chars: Peekable<Chars<'a>>,
    state: State,

    start: Position,
    current: Position,

    string_builder: SSBuilder<'a>,
    string_status: InvalidString,
    string_escape_start: Position,
    string_start: Position,
    string_skip_white: bool,

    include_comments: bool,
    allow_multiline_quote: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum StringEnd {
    Quote,
    Apostrophe,
    Complex(u32),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum State {
    Default,

    String(StringEnd),
    Eof,
    Eq,
    Gt,
    Lt,
    Minus,
    Tilde,
    Colon,
    Dot,
    DotDot,
    Ident,
    SingleLineComment,
    MinusMinus,
    MinusMinusLBrack,
    MultiLineComment,
    MultiLineCommentMinus,
    MultiLineCommentMinusMinus,
    MultiLineCommentMinusMinusRBrack,
    StringEscape(StringEnd),
    LBrack(u32),
    LBrackBackoff(u32),
    StringEnd(u32, u32),

    NumericStartZero,
    NumericStart,
    NumericDecimal,
    NumericDecimalNumberE,
    NumericDecimalNumberENumber,
    NumericBinStart,
    NumericHexStart,
    NumericDecimalNumberEPM,
    NumericBin,
    NumericHex,
}

pub enum SSBuilder<'a> {
    None,
    Ref(&'a str),
    Small(str_buf::StrBuf<15>),
    Alloc(String),
}

impl<'a> SSBuilder<'a> {
    fn take(&mut self) -> SSBuilder<'a> {
        let mut tmp = SSBuilder::None;
        std::mem::swap(self, &mut tmp);
        tmp
    }
}

fn ident(ident: &str) -> Token {
    match ident {
        "and" => Token::And,
        "break" => Token::Break,
        "do" => Token::Do,
        "else" => Token::Else,
        "elseif" => Token::Elseif,
        "end" => Token::End,
        "false" => Token::False,
        "for" => Token::For,
        "function" => Token::Function,
        "goto" => Token::Goto,
        "if" => Token::If,
        "in" => Token::In,
        "local" => Token::Local,
        "nil" => Token::Nil,
        "not" => Token::Not,
        "or" => Token::Or,
        "repeat" => Token::Repeat,
        "return" => Token::Return,
        "then" => Token::Then,
        "true" => Token::True,
        "until" => Token::Until,
        "while" => Token::While,
        o => Token::Ident(o),
    }
}

impl<'a> Tokenizer<'a> {
    pub fn new(str: &'a str) -> Self {
        Self {
            str,
            chars: str.chars().peekable(),
            state: State::Default,
            start: Position::default(),
            current: Position::default(),
            string_escape_start: Position::default(),
            string_builder: SSBuilder::None,
            include_comments: false,
            string_start: Position::default(),
            string_status: InvalidString::Valid,
            string_skip_white: false,
            allow_multiline_quote: false,
        }
    }

    pub fn include_comments(mut self) -> Self {
        self.include_comments = true;
        self
    }

    pub fn allow_multiline_quote(mut self) -> Self {
        self.allow_multiline_quote = true;
        self
    }

    fn escape_char_finish(&mut self, char: char, ret_state: StringEnd) {
        match self.string_builder.take() {
            SSBuilder::None => {
                let mut buf = StrBuf::new();
                // should never fail
                buf.write_char(char).unwrap();
                self.string_builder = SSBuilder::Small(buf)
            }
            SSBuilder::Ref(str) => {
                if str.len() + char.len_utf8() <= 15 {
                    let mut buf = StrBuf::new();
                    // should never fail
                    buf.write_str(str).unwrap();
                    buf.write_char(char).unwrap();
                    self.string_builder = SSBuilder::Small(buf)
                } else {
                    let mut buf = str.to_string();
                    buf.push(char);
                    self.string_builder = SSBuilder::Alloc(buf);
                }
            }
            SSBuilder::Small(mut buf) => {
                if buf.write_char(char).is_ok() {
                    self.string_builder = SSBuilder::Small(buf);
                } else {
                    let mut buf = buf.to_string();
                    buf.push(char);
                    self.string_builder = SSBuilder::Alloc(buf);
                }
            }
            SSBuilder::Alloc(mut buf) => {
                buf.push(char);
                self.string_builder = SSBuilder::Alloc(buf);
            }
        }
        self.state = State::String(ret_state);
    }
}

enum NumberType {
    Integer,
    Binary,
    Hex,
    Float,
    FloatHex,
}

fn parse_number(str: &str, r#type: NumberType) -> Result<Token<'_>, TokenizerError<'_>> {
    let mut buf = str_buf::StrBuf::<100>::new();
    for char in str.chars(){
        if char != '_'{
            if buf.write_char(char).is_err(){
                return Err(TokenizerError::IntegerTooLargeError)
            }
        }
    }
    let str = buf.as_str();
    match r#type {
        NumberType::Integer => str
            .parse::<i64>()
            .map(Token::IntegerLiteral)
            .map_err(TokenizerError::ParseIntError),
        NumberType::Binary => i64::from_str_radix(&str[2..], 2)
            .map(Token::IntegerLiteral)
            .map_err(TokenizerError::ParseIntError),
        NumberType::Hex => i64::from_str_radix(&str[2..], 16)
            .map(Token::IntegerLiteral)
            .map_err(TokenizerError::ParseIntError),
        NumberType::Float => str
            .parse::<f64>()
            .map(Token::FloatingLiteral)
            .map_err(TokenizerError::ParseFloatError),
        NumberType::FloatHex => str
            .parse::<f64>()
            .map(Token::FloatingLiteral)
            .map_err(TokenizerError::ParseFloatError),
    }
}

impl<'a> std::iter::Iterator for Tokenizer<'a> {
    type Item = TokenizerResult<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut ret = None;
        let mut ret_meta = None;
        let mut update_start_on_error = true;

        let mut ok_ret_state = State::Default;
        let mut err_ret_state = State::Default;

        loop {
            let c = self.chars.peek().copied();
            let mut consume = true;

            let processing = if let Some(char) = c {
                let mut tmp = self.current;
                tmp.offset += char.len_utf8();
                if char == '\n' {
                    tmp.line += 1;
                    tmp.col = 0;
                } else {
                    tmp.col += 1;
                }
                tmp
            } else {
                self.current
            };

            macro_rules! eof_none {
                ($expr:expr) => {
                    if let Some(char) = $expr {
                        char
                    } else {
                        self.state = State::Eof;
                        return None;
                    }
                };
            }

            macro_rules! unconsume_ret {
                ($sel:ident, $expr:expr) => {{
                    consume = false;
                    ret = Some($expr);
                }};
            }

            match self.state {
                State::Default => match eof_none!(c) {
                    '=' => self.state = State::Eq,
                    '>' => self.state = State::Gt,
                    '<' => self.state = State::Lt,
                    '-' => self.state = State::Minus,
                    '~' => self.state = State::Tilde,
                    '"' => {
                        self.string_start = processing;
                        self.string_status = InvalidString::Valid;
                        self.string_skip_white = false;
                        self.state = State::String(StringEnd::Quote)
                    }
                    '\'' => {
                        self.string_start = processing;
                        self.string_status = InvalidString::Valid;
                        self.string_skip_white = false;
                        self.state = State::String(StringEnd::Apostrophe)
                    }
                    ':' => self.state = State::Colon,
                    '.' => self.state = State::Dot,
                    '[' => {
                        self.string_start = processing;
                        self.string_status = InvalidString::Valid;
                        self.string_skip_white = false;
                        self.string_escape_start = self.current;
                        self.state = State::LBrack(0)
                    }

                    '+' => ret = Some(Ok(Token::Plus)),
                    '*' => ret = Some(Ok(Token::Asterick)),
                    '/' => ret = Some(Ok(Token::Slash)),
                    '%' => ret = Some(Ok(Token::Percent)),
                    '^' => ret = Some(Ok(Token::Carrot)),
                    '#' => ret = Some(Ok(Token::Octothorp)),
                    '&' => ret = Some(Ok(Token::Ampersand)),
                    '|' => ret = Some(Ok(Token::Pipe)),

                    ',' => ret = Some(Ok(Token::Comma)),
                    ';' => ret = Some(Ok(Token::Semicolon)),

                    '(' => ret = Some(Ok(Token::LPar)),
                    ')' => ret = Some(Ok(Token::RPar)),
                    '{' => ret = Some(Ok(Token::LBrace)),
                    '}' => ret = Some(Ok(Token::RBrace)),
                    ']' => ret = Some(Ok(Token::RBracket)),

                    '0' => {
                        self.state = State::NumericStartZero;
                    }
                    '1'..='9' => {
                        self.state = State::NumericStart;
                    }

                    c if c.is_whitespace() => self.start = processing,
                    c if c.is_alphabetic() || c == '_' => self.state = State::Ident,

                    c => ret = Some(Err(TokenizerError::InvalidChar(c))),
                },
                State::Ident => match c {
                    Some(c) if c.is_alphabetic() || c == '_' => {}
                    _ => unconsume_ret!(
                        self,
                        Ok(ident(&self.str[self.start.offset..self.current.offset]))
                    ),
                },
                State::Eq => match c {
                    Some('=') => ret = Some(Ok(Token::EqEq)),
                    _ => unconsume_ret!(self, Ok(Token::Eq)),
                },
                State::Gt => match c {
                    Some('=') => ret = Some(Ok(Token::GtEq)),
                    Some('>') => ret = Some(Ok(Token::GtGt)),
                    _ => unconsume_ret!(self, Ok(Token::Gt)),
                },
                State::Lt => match c {
                    Some('=') => ret = Some(Ok(Token::LtEq)),
                    Some('<') => ret = Some(Ok(Token::LtLt)),
                    _ => unconsume_ret!(self, Ok(Token::Lt)),
                },
                State::Minus => match c {
                    Some('-') => self.state = State::MinusMinus,
                    _ => unconsume_ret!(self, Ok(Token::Minus)),
                },
                State::Tilde => match c {
                    Some('=') => ret = Some(Ok(Token::TildeEq)),
                    _ => unconsume_ret!(self, Ok(Token::Tilde)),
                },
                State::Colon => match c {
                    Some(':') => ret = Some(Ok(Token::ColonColon)),
                    _ => unconsume_ret!(self, Ok(Token::Colon)),
                },
                State::Dot => match c {
                    Some('.') => self.state = State::DotDot,
                    _ => unconsume_ret!(self, Ok(Token::Dot)),
                },
                State::DotDot => match c {
                    Some('.') => ret = Some(Ok(Token::DotDotDot)),
                    _ => unconsume_ret!(self, Ok(Token::DotDot)),
                },
                State::MinusMinus => match c {
                    Some('[') => self.state = State::MinusMinusLBrack,
                    _ => {
                        consume = false;
                        self.state = State::SingleLineComment;
                    }
                },
                State::MinusMinusLBrack => match c {
                    Some('[') => self.state = State::MultiLineComment,
                    _ => {
                        consume = false;
                        self.state = State::SingleLineComment;
                    }
                },
                State::MultiLineComment => match c {
                    None => ret = Some(Err(TokenizerError::UnclosedMultiLineComment)),
                    Some('-') => self.state = State::MultiLineCommentMinus,
                    _ => {}
                },
                State::MultiLineCommentMinus => match c {
                    None => ret = Some(Err(TokenizerError::UnclosedMultiLineComment)),
                    Some('-') => self.state = State::MultiLineCommentMinusMinus,
                    _ => {
                        consume = false;
                        self.state = State::MultiLineComment;
                    }
                },
                State::MultiLineCommentMinusMinus => match c {
                    None => ret = Some(Err(TokenizerError::UnclosedMultiLineComment)),
                    Some(']') => self.state = State::MultiLineCommentMinusMinusRBrack,
                    _ => {
                        consume = false;
                        self.state = State::MultiLineComment;
                    }
                },
                State::MultiLineCommentMinusMinusRBrack => match c {
                    None => ret = Some(Err(TokenizerError::UnclosedMultiLineComment)),
                    Some(']') => {
                        ret = Some(Ok(Token::SingleLineComment(
                            &self.str[self.start.offset + 4..self.current.offset - 4],
                        )))
                    }
                    _ => {
                        consume = false;
                        self.state = State::MultiLineComment;
                    }
                },
                State::SingleLineComment => match c {
                    Some('\n') | None => {
                        ret = Some(Ok(Token::SingleLineComment(
                            &self.str[self.start.offset + 2 * '/'.len_utf8()..self.current.offset],
                        )))
                    }
                    _ => {}
                },
                State::LBrack(indent) => match c {
                    Some('[') => {
                        self.string_start = processing;
                        self.state = State::String(StringEnd::Complex(indent))
                    }
                    Some('=') => self.state = State::LBrack(indent + 1),
                    _ => {
                        // this is horrible
                        ret_meta = Some(TokenMeta::start_size(self.string_escape_start, 1));
                        self.string_escape_start.col += 1;
                        self.string_escape_start.offset += 1;
                        ret = Some(Ok(Token::LBracket));
                        ok_ret_state = State::LBrackBackoff(indent);
                    }
                },
                State::LBrackBackoff(indent) => {
                    consume = false;
                    if indent == 0 {
                        self.state = State::Default;
                    } else if indent == 1 {
                        ret_meta = Some(TokenMeta::start_size(self.string_escape_start, 1));
                        self.string_escape_start.col += 1;
                        self.string_escape_start.offset += 1;
                        ok_ret_state = State::LBrackBackoff(indent - 1);
                        ret = Some(Ok(Token::Eq))
                    } else {
                        ret_meta = Some(TokenMeta::start_size(self.string_escape_start, 2));
                        self.string_escape_start.col += 2;
                        self.string_escape_start.offset += 2;
                        ok_ret_state = State::LBrackBackoff(indent - 2);
                        ret = Some(Ok(Token::EqEq))
                    }
                }
                State::String(escape) => {
                    let end = matches!(
                        (c, escape),
                        (Some('\''), StringEnd::Apostrophe) | (Some('"'), StringEnd::Quote)
                    );

                    'exit_str: {
                        if end {
                            let yarn = match self.string_builder.take() {
                                SSBuilder::None => byteyarn::YarnBox::new(""),
                                SSBuilder::Ref(str) => byteyarn::YarnBox::from(str),
                                SSBuilder::Small(small_buf) => {
                                    byteyarn::YarnBox::new(small_buf.as_str()).immortalize()
                                }
                                SSBuilder::Alloc(string) => byteyarn::Yarn::from_string(string),
                            };
                            match self.string_status {
                                InvalidString::Valid => ret = Some(Ok(Token::StringLiteral(yarn))),
                                _ => ret = Some(Err(TokenizerError::MalformedString(yarn))),
                            }
                            break 'exit_str;
                        }
                        if matches!(
                            (c, escape),
                            (Some('\n'), StringEnd::Apostrophe | StringEnd::Quote)
                        ) && !self.allow_multiline_quote
                        {
                            self.string_status = InvalidString::InvalidNewline;
                        }
                        if self.string_skip_white {
                            if c.map(char::is_whitespace).unwrap_or(false) {
                                match self.string_builder.take() {
                                    SSBuilder::None => {
                                        self.string_builder =
                                            SSBuilder::Small(str_buf::StrBuf::new())
                                    }
                                    SSBuilder::Ref(val) => {
                                        if val.len() <= 15 {
                                            self.string_builder =
                                                SSBuilder::Small(str_buf::StrBuf::from_str(val))
                                        } else {
                                            self.string_builder = SSBuilder::Alloc(val.to_string());
                                        }
                                    }
                                    val => self.string_builder = val,
                                }
                                break 'exit_str;
                            } else {
                                self.string_skip_white = false;
                            }
                        }
                        if let (Some(']'), StringEnd::Complex(start)) = (c, escape) {
                            self.string_escape_start = self.current;
                            self.state = State::StringEnd(start, 0);
                            break 'exit_str;
                        }
                        match c {
                            Some('\\') => {
                                self.string_escape_start = self.current;
                                self.state = State::StringEscape(escape);
                            }
                            Some(c) => match self.string_builder.take() {
                                SSBuilder::None => {
                                    self.string_builder = SSBuilder::Ref(
                                        &self.str[self.string_start.offset..processing.offset],
                                    )
                                }
                                SSBuilder::Ref(_) => {
                                    self.string_builder = SSBuilder::Ref(
                                        &self.str[self.string_start.offset..processing.offset],
                                    )
                                }
                                SSBuilder::Small(mut small) => {
                                    if small.write_char(c).is_ok() {
                                        self.string_builder = SSBuilder::Small(small);
                                    } else {
                                        let mut string = small.to_string();
                                        string.push(c);
                                        self.string_builder = SSBuilder::Alloc(string);
                                    }
                                }
                                SSBuilder::Alloc(mut string) => {
                                    string.push(c);
                                    self.string_builder = SSBuilder::Alloc(string);
                                }
                            },
                            None => ret = Some(Err(TokenizerError::UnclosedStringLiteral)),
                        }
                    }
                }
                State::StringEnd(start, level) => match c {
                    Some('=') => self.state = State::StringEnd(start, level + 1),
                    Some(']') if start == level => {
                        let yarn = match self.string_builder.take() {
                            SSBuilder::None => byteyarn::YarnBox::new(""),
                            SSBuilder::Ref(str) => byteyarn::YarnBox::from(str),
                            SSBuilder::Small(small_buf) => {
                                byteyarn::YarnBox::new(small_buf.as_str()).immortalize()
                            }
                            SSBuilder::Alloc(string) => byteyarn::Yarn::from_string(string),
                        };
                        match self.string_status {
                            InvalidString::Valid => ret = Some(Ok(Token::StringLiteral(yarn))),
                            _ => ret = Some(Err(TokenizerError::MalformedString(yarn))),
                        }
                    }
                    _ => {
                        match self.string_builder.take() {
                            SSBuilder::None => {
                                self.string_builder = SSBuilder::Ref(
                                    &self.str[self.string_start.offset..processing.offset],
                                )
                            }
                            SSBuilder::Ref(_) => {
                                self.string_builder = SSBuilder::Ref(
                                    &self.str[self.string_start.offset..processing.offset],
                                )
                            }
                            SSBuilder::Small(mut small) => {
                                let str =
                                    &self.str[self.string_escape_start.offset..processing.offset];
                                if small.remaining() >= str.len() {
                                    small.push_str(str);
                                    self.string_builder = SSBuilder::Small(small);
                                } else {
                                    let mut string = small.to_string();
                                    string.push_str(str);
                                    self.string_builder = SSBuilder::Alloc(string);
                                }
                            }
                            SSBuilder::Alloc(mut string) => {
                                let str =
                                    &self.str[self.string_escape_start.offset..processing.offset];
                                string.push_str(str);
                                self.string_builder = SSBuilder::Alloc(string);
                            }
                        }
                        self.state = State::String(StringEnd::Complex(start));
                    }
                },
                State::StringEscape(end) => match c {
                    Some('0') => self.escape_char_finish('\0', end),
                    Some('n') => self.escape_char_finish('\n', end),
                    Some('r') => self.escape_char_finish('\r', end),
                    Some('t') => self.escape_char_finish('\t', end),
                    Some('\\') => self.escape_char_finish('\\', end),
                    Some('\'') => self.escape_char_finish('\'', end),
                    Some('"') => self.escape_char_finish('"', end),
                    Some('z') => {
                        self.state = State::String(end);
                        self.string_skip_white = true;
                    }
                    Some(_) => {
                        consume = false;
                        err_ret_state = State::String(end);
                        update_start_on_error = false;
                        ret_meta = Some(TokenMeta::start_end(self.string_escape_start, processing));
                        ret = Some(Err(TokenizerError::InvalidEscape(
                            &self.str[self.string_escape_start.offset..processing.offset],
                        )))
                    }
                    None => {
                        err_ret_state = State::Eof;
                        ret = Some(Err(TokenizerError::UnfinishedEscapeSequence(
                            &self.str[self.string_escape_start.offset..processing.offset],
                        )))
                    }
                },

                State::NumericStart => match c {
                    Some('0'..='9') => {}
                    Some('.') => self.state = State::NumericDecimal,
                    Some('e'|'E') => {
                        self.state = State::NumericDecimalNumberE;
                    }
                    Some('_') => {}
                    _ => {
                        consume = false;
                        ret = Some(parse_number(
                            &self.str[self.start.offset..self.current.offset],
                            NumberType::Integer,
                        ));
                    }
                },
                State::NumericStartZero => match c {
                    Some('b') => {
                        self.state = State::NumericBinStart;
                    }
                    Some('x') => {
                        self.state = State::NumericHexStart;
                    }
                    Some('0'..='9') => {
                        self.state = State::NumericStart;
                    }
                    Some('.') => self.state = State::NumericDecimal,
                    Some('_') => {}
                    _ => {
                        consume = false;
                        ret = Some(parse_number(
                            &self.str[self.start.offset..self.current.offset],
                            NumberType::Integer,
                        ));
                    }
                },
                State::NumericDecimal => match c {
                    Some('0'..='9') => {}
                    Some('e'|'E') => {
                        self.state = State::NumericDecimalNumberE;
                    }
                    Some('_') => {}
                    _ => {
                        consume = false;
                        ret = Some(parse_number(
                            &self.str[self.start.offset..self.current.offset],
                            NumberType::Float,
                        ));
                    }
                },
                State::NumericDecimalNumberE => match c {
                    Some('0'..='9') => {
                        self.state = State::NumericDecimalNumberENumber;
                    }
                    Some('+' | '-') => {
                        self.state = State::NumericDecimalNumberEPM;
                    }
                    Some('_') => {}
                    _ => {
                        consume = false;
                        ret = Some(Err(TokenizerError::EmptyExponent));
                    }
                },
                State::NumericDecimalNumberEPM => match c {
                    Some('0'..='9') => {
                        self.state = State::NumericDecimalNumberENumber;
                    }
                    Some('_') => {}
                    _ => {
                        consume = false;
                        ret = Some(Err(TokenizerError::EmptyExponent));
                    }
                },
                State::NumericDecimalNumberENumber => match c {
                    Some('0'..='9' | '_') => {}
                    _ => {
                        consume = false;
                        ret = Some(parse_number(
                            &self.str[self.start.offset..self.current.offset],
                            NumberType::Float,
                        ));
                    }
                },
                State::NumericBinStart => match c {
                    Some('0'..='1') => {
                        self.state = State::NumericBin;
                    }
                    Some('_') => {}
                    Some(c @ '2'..='9') => {
                        err_ret_state = State::NumericBin;
                        ret_meta = Some(TokenMeta::start_end(self.current, processing));
                        update_start_on_error = false;
                        ret = Some(Err(TokenizerError::InvalidBase2Digit(c)))
                    }
                    _ => {
                        consume = false;
                        ret = Some(Err(TokenizerError::NoNumberAfterBasePrefix))
                    }
                },
                State::NumericBin => match c {
                    Some('0'..='1') => {}
                    Some('_') => {}
                    Some(c @ '2'..='9') => {
                        err_ret_state = State::NumericBin;
                        ret_meta = Some(TokenMeta::start_end(self.current, processing));
                        update_start_on_error = false;
                        ret = Some(Err(TokenizerError::InvalidBase2Digit(c)))
                    }
                    _ => {
                        consume = false;
                        ret = Some(parse_number(
                            &self.str[self.start.offset..self.current.offset],
                            NumberType::Binary,
                        ));
                    }
                },
                State::NumericHexStart => match c {
                    Some('0'..='9' | 'a'..='f' | 'A'..='F') => {
                        self.state = State::NumericHex;
                    }
                    Some('_') => {}
                    _ => {
                        consume = false;
                        ret = Some(Err(TokenizerError::NoNumberAfterBasePrefix))
                    }
                },
                State::NumericHex => match c {
                    Some('0'..='9' | 'a'..='f' | 'A'..='F') => {}
                    Some('_') => {}
                    _ => {
                        consume = false;
                        ret = Some(parse_number(
                            &self.str[self.start.offset..self.current.offset],
                            NumberType::Hex,
                        ));
                    }
                },
                State::Eof => return None,
            }

            if consume {
                self.chars.next();
                self.current = processing;
            }

            if let Some(ret_some) = ret {
                match ret_some {
                    Ok(token) => {
                        let meta =
                            ret_meta.unwrap_or(TokenMeta::start_end(self.start, self.current));
                        self.start = self.current;
                        self.state = ok_ret_state;
                        if matches!(
                            token,
                            Token::MultiLineComment(_) | Token::SingleLineComment(_)
                        ) && !self.include_comments
                        {
                            ret = None;
                            continue;
                        }
                        return Some(Ok(Span::new(token, meta)));
                    }
                    Err(err) => {
                        let meta =
                            ret_meta.unwrap_or(TokenMeta::start_end(self.start, self.current));
                        if update_start_on_error {
                            self.start = self.current;
                        }
                        self.state = err_ret_state;
                        return Some(Err(Box::new(Span::new(err, meta))));
                    }
                }
            }
        }
    }
}

#[test]
pub fn test_tokenizer_full() {
    let data = r#"

    and       break     do        else      elseif    end
    false     for       function  goto      if        in
    local     nil       not       or        repeat    return
    then      true      until     while

    +     -     *     /     %     ^     #
    &     ~     |     <<    >>    //
    ==    ~=    <=    >=    <     >     =
    (     )     {     }     [     ]     ::
    ;     :     ,     .     ..    ...

    -- no action (comment)
    --[[
        print(10)         --> 10
    --]]

    a = 'alo\n123"'
    a = "alo\n123\""
    a = '\97lo\10\04923"'
    a = "bruh \z
    asdasd\z            
    "
    [
    [=
    [==
    [===
    [====
    a = [[alo
    123"]]
    a = [==[
    alo ]] ]=] ]===]
    123"]==]

    a = b + c(print or io.write)('done')
     
    a = b + c; (print or io.write)('done')

    0 1 2 3 4 5 6 7 8 9 10 55 1234567890
0x55 0xFF 0x1234567890abcdefABCDEF
0b110101011010
1.
0.
0.0
0.1
1234567890.1234567890
1234567890.1234567890e0
1234567890.1234567890e1234567890
1234567890.1234567890e+1234567890
1234567890.1234567890e-1234567890

0xFF_FF_FF
0_0_0_1_2
0b_1_0_1
12_45_._43_e_-_1

    3   345   0xff   0xBEBADA
    3.0     3.1416     314.16e-2     0.31416E1     34e1
    0x0.1E  0xA23p-4   0X1.921FB54442D18P+1
    "#;

    let tokenizer = Tokenizer::new(data).include_comments();

    for token in tokenizer {
        match token {
            Ok(ok) => {
                let repr = &data[ok.span.offset as usize..(ok.span.offset + ok.span.len) as usize];
                println!("{:?} => {:?}", repr, ok)
            }
            Err(err) => {
                let repr =
                    &data[err.span.offset as usize..(err.span.offset + err.span.len) as usize];
                println!("Error {:?}: {:?}", repr, err)
            }
        }
    }
}

#[test]
pub fn test_tokenizer_syntax() {
    let data = r#"

    stat ::= goto Name
    stat ::= label
    label ::= '::' Name '::'
    
    
    stat ::= for Name '=' exp ',' exp [',' exp] do block end
    
     for v = e1, e2, e3 do block end
    
    
     do
       local var, limit, step = tonumber(e1), tonumber(e2), tonumber(e3)
       if not (var and limit and step) then error() end
       var = var - step
       while true do
         var = var + step
         if (step >= 0 and var > limit) or (step < 0 and var < limit) then
           break
         end
         local v = var
         block
       end
     end
    
    
    stat ::= for namelist in explist do block end
    namelist ::= Name {',' Name}
    
     for var_1, ···, var_n in explist do block end
    
    
     do
       local f, s, var = explist
       while true do
         local var_1, ···, var_n = f(s, var)
         if var_1 == nil then break end
         var = var_1
         block
       end
     end
    
     function foo (a)
       print("foo", a)
       return coroutine.yield(2*a)
     end
     
     co = coroutine.create(function (a,b)
           print("co-body", a, b)
           local r = foo(a+1)
           print("co-body", r)
           local r, s = coroutine.yield(a+b, a-b)
           print("co-body", r, s)
           return b, "end"
     end)
     
     print("main", coroutine.resume(co, 1, 10))
     print("main", coroutine.resume(co, "r"))
     print("main", coroutine.resume(co, "x", "y"))
     print("main", coroutine.resume(co, "x", "y"))
    
    
    stat ::= while exp do block end
    stat ::= repeat block until exp
    stat ::= if exp then block {elseif exp then block} [else block] end
    "#;

    let tokenizer = Tokenizer::new(data).include_comments();

    for token in tokenizer {
        match token {
            Ok(ok) => {
                let repr = &data[ok.span.offset as usize..(ok.span.offset + ok.span.len) as usize];
                println!("{:?} => {:?}", repr, ok)
            }
            Err(err) => {
                let repr =
                    &data[err.span.offset as usize..(err.span.offset + err.span.len) as usize];
                println!("Error {:?}: {:?}", repr, err)
            }
        }
    }
}

#[test]
pub fn test_tokenizer_errors() {
    let data = [
        r#"""#, r#""  "#, r#"'"#, r#"' "#, r#"/*"#, r#"/* *"#, r#"/* "#, r#"/* *"#, r#"'\'"#,
        r#""\"#, r#""\"#, r#""\9"#, "0b1234", "0x", "0b", "1.0e", "1.0e+", "1.0e-",
    ];

    for data in data {
        println!("{:?}", data);
        let tokenizer = Tokenizer::new(data);

        for token in tokenizer {
            match token {
                Ok(ok) => {
                    let repr =
                        &data[ok.span.offset as usize..(ok.span.offset + ok.span.len) as usize];
                    println!("{:?} => {:?}", repr, ok)
                }
                Err(err) => {
                    let repr =
                        &data[err.span.offset as usize..(err.span.offset + err.span.len) as usize];
                    println!("Error {:?}: {:?}", repr, err)
                }
            }
        }
        println!();
    }
}
