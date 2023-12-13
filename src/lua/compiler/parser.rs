use std::{collections::{HashMap, VecDeque}, iter::Peekable};

use crate::lua::tokenizer::{
    token::{SpanFmt, Token},
    Span, Tokenizer, TokenizerError,
};

#[derive(Debug, Default, Clone, PartialEq)]
pub enum LuaType {
    #[default]
    Nil,
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(std::rc::Rc<String>),
    Map(std::rc::Rc<HashMap<String, LuaType>>),
}

pub enum Ast {
    // Ident(<'a>)
}
#[derive(Debug, Clone)]
pub struct Block(Chunk);
#[derive(Debug, Clone)]
pub struct Chunk(Vec<Statement>);

#[derive(Debug, Clone)]
pub enum Statement {
    Assign(Vec<(Var, Expr)>),
    FunctionCall(),
    Label(),
    Break,
    Goto(String),
    Block(Block),
    While(Expr, Block),

    If(Expr, Block, Vec<(Expr, Block)>, Option<(Expr, Block)>),
    Function(String, FunctionBody),
    LocalFunction(String, FunctionBody),
    LocalVerDef(AttNameList),
    LocalVerDec(AttNameList, ExprList),
}

#[derive(Debug, Clone)]
pub struct ExprList(Vec<Expr>);

#[derive(Debug, Clone)]
pub struct AttNameList(Vec<()>);

#[derive(Debug, Clone)]
pub struct Attrib(Name);

#[derive(Debug, Clone)]
pub struct FunctionBody(ParamList, Block);

#[derive(Debug, Clone)]
pub struct Var(String);

#[derive(Debug, Clone)]
pub struct NameList(Vec<String>);

#[derive(Debug, Clone)]
pub struct ParamList {
    list: NameList,
    trailing: bool,
}

#[derive(Debug, Clone)]
pub struct FuncName(Name, Vec<Name>, Option<Name>);

#[derive(Debug, Clone)]
pub struct Name(String);

#[derive(Debug, Clone)]
pub enum Expr {
    Or(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    LtEq(Box<Expr>, Box<Expr>),
    GtEq(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Neq(Box<Expr>, Box<Expr>),
    DotDot(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Not(Box<Expr>),
    Len(Box<Expr>),
    Neg(Box<Expr>),
    Power(Box<Expr>, Box<Expr>),

    Literal(LuaType),
}

pub struct Parser<'a> {
    tokenizer_errors: Vec<Span<TokenizerError<'a>>>,
    tokenizer: Peekable<Tokenizer<'a>>,
}
// pub type ParseResult = Result<Ast>

macro_rules! next_match {

    ($self:expr, $tok:ident, $span:ident, if $pat:pat, $($block:block $(else $else:block)?)?) => {{
        let $tok = $self.next_tok();
        $(if let Some($crate::lua::tokenizer::Span{span: $span, val: $pat }) = $tok{
            $block
        }else{
            $($else)?
        })?
    }};
}

macro_rules! next_if_match {
    ($self:expr, if $pat:pat, $($block:block $(else $else:block)?)?) => {
        next_if_match!($self, span, if $pat, $($block $(else $else)?)? )
    };
    ($self:expr, $span:ident, if $pat:pat, $($block:block $(else $else:block)?)?) => {
        if let Some($crate::lua::tokenizer::Span{val: $pat, .. }) = $self.peek_tok(){
            let tok = $self.next_tok();
            $(if let Some($crate::lua::tokenizer::Span{span: $span, val: $pat }) = tok{
                $block
            }else{
                unreachable!()
            })?
        }else{
            $($($else)?)?
        }
    };
}

impl<'a> Parser<'a> {
    pub fn new(tokenizer: Tokenizer<'a>) -> Self {
        Self {
            tokenizer: tokenizer.peekable(),
            tokenizer_errors: Default::default(),
        }
    }

    fn next_n<const N: usize>(&mut self) -> [Option<Span<Token<'a>>>; N]{
        // None.unwrap
        std::array::from_fn(|_|{self.next_tok()})
    }

    fn test(&mut self){
        macro_rules! tok {
            ($pat:pat) => {
                Some(Span{val: $pat, ..})
            };
            ($pat:pat, $span:ident) => {
                Some(Span{val: $pat, span: $span})
            };
        }
        match self.next_n::<3>(){
            [tok!(Token::Lt, ltspan), tok!(Token::Ident(ident), identspan), tok!(Token::Gt, gtspan)] => {
                println!("{}", ident)
            }
            _ => {}
        }
    }

    fn peek_tok(&mut self) -> Option<&Span<Token<'a>>> {
        
        loop {
            let tok = self.tokenizer.peek()?;
            if tok.is_ok() {
                // stupid lifetime hack but whatever
                return self.tokenizer.peek()?.as_ref().ok();
            }

            self.tokenizer_errors.push(*self.tokenizer.next()?.err()?);
        }
    }

    fn next_tok(&mut self) -> Option<Span<Token<'a>>> {
        let mut tok = self.tokenizer.next()?;
        while tok.is_err() {
            self.tokenizer_errors.push(*tok.err()?);
            tok = self.tokenizer.next()?;
        }
        tok.ok()
    }

    pub fn parse_str(str: &'a str) -> Result<Vec<Statement>, ()> {
        let mut myself = Self::new(Tokenizer::new(str));
        myself.parse()
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>, ()> {
        let top_level = Vec::new();
        while self.peek_tok().is_some() {}
        Ok(top_level)
    }

    pub fn parse_statement_list(&mut self) -> Vec<Statement> {
        let mut vec = Vec::new();

        while let Some(statement) = self.parse_statement() {
            vec.push(statement.val);
        }

        vec
    }

    pub fn parse_statement(&mut self) -> Option<Span<Statement>> {
        let Span {
            mut span,
            val: start,
        } = if let Some(some) = self.peek_tok() {
            some
        } else {
            return None;
        };
        let state = match start {
            Token::Break => Statement::Break,
            Token::Goto => {
                next_match!(self, tok, nspan,
                    if Token::Ident(ident), {
                        span = span.extend_range(nspan);
                        Statement::Goto(ident.to_string())
                    }else{
                        //invalid token
                        todo!();
                    }
                )
            }
            _ => return None,
        };
        Some(Span::new(state, span))
    }

    fn parse_args(&mut self) {}

    fn parse_attrib(&mut self) -> Option<Span<Attrib>>{
        None
    }
    
    fn name_list(&mut self) -> Option<Span<NameList>> {
        let mut vec = Vec::new();

        let mut span = next_match!(self, tok, span, if Token::Ident(ident), {
            vec.push(ident.to_string());
            span
        }else{
            return None;
        });

        loop {
            next_if_match!(self, nspan, if Token::Comma, {
                span = span.extend_range(nspan);
            }else{
                break;
            });
            next_match!(self, tok, nspan, if Token::Ident(ident), {
                span = span.extend_range(nspan);
                vec.push(ident.to_string());
            }else{
                //invalid token
                todo!();
                break;
            });
        }

        Some(Span::new(NameList(vec), span))
    }

    fn param_list(&mut self) -> Option<Span<ParamList>> {
        let mut vec = Vec::new();
        let trailing;

        let mut span = next_match!(self, tok, span, if Token::Ident(ident), {
            vec.push(ident.to_string());
            span
        }else{
            if let Some(Span{span, val: Token::DotDotDot}) = tok{
                return Some(Span::new(
                    ParamList {
                        list: NameList(vec),
                        trailing: true,
                    },
                    span,
                ));
            }else{
                return None;
            }
        });

        loop {
            next_if_match!(self, nspan, if Token::Comma, {
                span = span.extend_range(nspan);
            }else{
                trailing = false;
                break;
            });
            next_match!(self, tok, nspan, if Token::Ident(ident), {
                span = span.extend_range(nspan);
                vec.push(ident.to_string());
            }else{
                if let Some(Span{span: nspan, val: Token::DotDotDot}) = tok{
                    span = span.extend_range(nspan);
                    trailing = true;
                    break;
                }else{
                    //invalid token
                    todo!();
                }
            });
        }

        Some(Span::new(
            ParamList {
                list: NameList(vec),
                trailing,
            },
            span,
        ))
    }
}

#[test]
fn test() {
    let lua = r#"...
    hello
    hello,...
    list,two,three
    hello, this, is, a...
        hello, this, is, a,...
    "#;
    let mut parser = Parser::new(Tokenizer::new(lua));
    while let Some(list) = parser.param_list() {
        let fmt = SpanFmt {
            span: list.span,
            original: lua,
            message: Some(" Cool :3"),
            kind: crate::lua::tokenizer::token::Kind::Error(55),
            filename: "nothing/here/to/see.rs",
        };
        print!("{}", fmt);
        println!("{:#?}\n", list.val)
    }
}