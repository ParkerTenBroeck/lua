use std::{
    collections::{HashMap, VecDeque},
    iter::Peekable,
};

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
pub struct Block(Vec<Statement>);

#[derive(Debug, Clone)]
pub enum Statement {
    Assign(Vec<(Var, Expr)>),
    FunctionCall(),
    Label(Label),
    Break,
    Goto(Name),
    Block(Block),
    While(Expr, Block),

    If(Expr, Block, Vec<(Expr, Block)>, Option<(Expr, Block)>),
    Function(Name, FunctionBody),
    LocalFunction(Name, FunctionBody),
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
pub struct Label(Name);

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
    tokenizer: Tokenizer<'a>,
    queue: VecDeque<Option<Span<Token<'a>>>>,
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
macro_rules! tok {
    ($pat:pat) => {
        Some(Span { val: $pat, .. })
    };
    ($pat:pat, $span:ident) => {
        Some(Span {
            val: $pat,
            span: $span,
        })
    };
}

impl<'a> Parser<'a> {
    pub fn new(tokenizer: Tokenizer<'a>) -> Self {
        Self {
            tokenizer: tokenizer,
            queue: VecDeque::new(),
            tokenizer_errors: Default::default(),
        }
    }

    fn next_n<const N: usize>(&mut self) -> [Option<Span<Token<'a>>>; N] {
        std::array::from_fn(|_| self.queue.pop_front().unwrap_or_else(|| self.next_tok()))
    }

    fn peek_n<const N: usize>(&mut self) -> [&Option<Span<Token<'a>>>; N] {
        while self.queue.len() < N {
            self.queue.push_back(match self.tokenizer.next() {
                Some(some) => match some {
                    Ok(ok) => Some(ok),
                    Err(err) => {
                        self.tokenizer_errors.push(*err);
                        continue;
                    }
                },
                None => None,
            })
        }
        std::array::from_fn(|n| self.queue.get(n).unwrap())
    }

    fn peek_tok(&mut self) -> &Option<Span<Token<'a>>> {
        self.peek_n::<1>()[0]
    }

    fn next_tok(&mut self) -> Option<Span<Token<'a>>> {
        if !self.queue.is_empty(){
            return self.queue.pop_front().unwrap();
        }
        let mut tok = self.tokenizer.next()?;
        while tok.is_err() {
            self.tokenizer_errors.push(*tok.err()?);
            tok = self.tokenizer.next()?;
        }
        tok.ok()
    }

    // fn give_n_back<const N: usize>(&mut self, tokens: [Option<Span<Token<'a>>>; N]){
    //     for tok in tokens.into_iter().rev(){
    //         self.queue.push_front(tok)
    //     }
    // }

    // fn give_back(&mut self, token: Option<Span<Token<'a>>>){
    //     self.give_n_back([token])
    // }

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

    fn parse_block(&mut self) -> Option<Span<Block>>{
        let mut vec = Vec::new();
        let first = self.parse_statement()?;
        vec.push(first.val);
        let mut span = first.span; 
        while let Some(next) = self.parse_statement(){
            vec.push(next.val);
            span = span.extend_range(&next.span); 
        }
        Some(Span::new(Block(vec), span))
    }

    fn parse_statement(&mut self) -> Option<Span<Statement>> {
        let Span {
            mut span,
            val: start,
        } = loop {
            if let Some(some) = self.peek_tok() {
                if some.val != Token::Semicolon{
                    break some;
                }
                self.next_tok();
            } else {
                return None;
            }
        };
        let state = match start {
            Token::Break |
            Token::Goto |
            Token:: Do |
            Token::While |
            Token::Repeat |
            Token::If |
            Token::For |
            Token::Function |
            Token::Local => {
                match self.next_tok().unwrap().val{
                    Token::Break => Statement::Break,
                    Token::Goto => match self.next_tok(){
                        tok!(Token::Ident(name)) => 
                            Statement::Goto(Name(name.into())),
                        // error cases
                        None => todo!(),
                        Some(_tok) => todo!()
                    }
                    Token::Do => todo!(),
                    Token::While => todo!(),
                    Token::Repeat => todo!(),
                    Token::If => todo!(),
                    Token::For => todo!(),
                    Token::Function => match self.next_tok(){
                        tok!(Token::Ident(name), ident_span) => {
                            let body =  self.parse_function_body();
                            span = span.extend_range(&ident_span).extend_range(&body.span);
                            Statement::LocalFunction(Name(name.into()),body.val)
                        }
                        Some(_) => {todo!()}
                        None => {todo!()}
                    },
                    Token::Local => {
                        match self.peek_n::<2>(){
                            [tok!(Token::Ident(_) | Token::Lt), _] => 
                                todo!(),

                            [tok!(Token::Function, func_span), tok!(Token::Ident(name), ident_span)] => {
                                let name = Name(name.into());
                                let func_span = *func_span;
                                let ident_span = *ident_span;
                                self.next_n::<2>();
                                let body =  self.parse_function_body();
                                span = span.extend_range(&func_span).extend_range(&ident_span).extend_range(&body.span);
                                Statement::LocalFunction(name,body.val)
                            },
                            
                            // error cases
                            [tok!(Token::Function), _] => 
                                todo!(),
                            [None, _] => todo!(),
                            [Some(_tok), _] => todo!()
                        }
                    }
                    _ => unreachable!(),
                }
            }
            Token::ColonColon => {
                if let Some(label) = self.parse_label(){
                    span = span.extend_range(&label.span);
                    Statement::Label(label.val)
                }else{
                    Statement::Label(Label(Name(String::new())))
                }
            }
            _ => return None,
        };
        Some(Span::new(state, span))
    }

    fn parse_function_body(&mut self) -> Span<FunctionBody>{
        let mut span;
        match self.next_tok(){
            tok!(Token::LPar, lpar_span) => {
                span = lpar_span;
            }
            Some(_) => {todo!()}
            None => {todo!()}
        }

        let param = if let Some(some) = self.param_list(){
            span = span.extend_range(&some.span);
            some.val
        }else{
            todo!()
        };

        match self.next_tok(){
            tok!(Token::RPar, rpar_span) => {
                span = span.extend_range(&rpar_span);
            }
            Some(_) => {todo!()}
            None => {todo!()}
        }
        
        let body = self.parse_block();
        let body = if let Some(body) = body{
            span = span.extend_range(&body.span);
            body.val
        }else{
            Block(Vec::new())
        };

        match self.next_tok(){
            tok!(Token::End, rpar_span) => {
                span = span.extend_range(&rpar_span);
            }
            Some(_) => {todo!()}
            None => {todo!()}
        }

        Span::new(FunctionBody(param, body), span)
    }


    fn parse_attrib(&mut self) -> Option<Span<Attrib>> {
        match self.peek_n::<3>() {
            [tok!(Token::Lt, ltspan), tok!(Token::Ident(ident), _identspan), tok!(Token::Gt, gtspan)] =>
            {
                let span = ltspan.extend_range(gtspan);
                let ret = Some(Span::new(Attrib(Name(ident.into())), span));
                // consume the accepted tokens
                self.next_n::<3>();
                ret
            }
            // error cases we can report on 
            [tok!(Token::Lt, _ltspan), tok!(Token::Ident(_), _identspan), None] =>{
                todo!()
            }
            [tok!(Token::Lt, _ltspan), tok!(Token::Ident(_), _identspan), tok!(_, _gtspan)] =>{
                todo!()
            }
            _ => None,
        }
    }

    fn parse_label(&mut self) -> Option<Span<Label>> {
        match self.peek_n::<3>() {
            [tok!(Token::ColonColon, ltspan), tok!(Token::Ident(ident), identspan), tok!(Token::ColonColon, gtspan)] =>
            {
                let span = ltspan.extend_range(gtspan);
                let ret = Some(Span::new(Label(Name(ident.into())), span));
                // consume the accepted tokens
                self.next_n::<3>();
                ret
            }
            // error cases we can report on 
            [tok!(Token::ColonColon, ltspan), tok!(Token::Ident(ident), identspan), None] =>{
                todo!()
            }
            [tok!(Token::ColonColon, ltspan), tok!(Token::Ident(ident), identspan), tok!(_, gtspan)] =>{
                todo!()
            }
            _ => None,
        }
    }

    // fn parse_fnname(&mut self) -> O 

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
                span = span.extend_range(&nspan);
            }else{
                break;
            });
            next_match!(self, tok, nspan, if Token::Ident(ident), {
                span = span.extend_range(&nspan);
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
                span = span.extend_range(&nspan);
            }else{
                trailing = false;
                break;
            });
            next_match!(self, tok, nspan, if Token::Ident(ident), {
                span = span.extend_range(&nspan);
                vec.push(ident.to_string());
            }else{
                if let Some(Span{span: nspan, val: Token::DotDotDot}) = tok{
                    span = span.extend_range(&nspan);
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
    let lua = r#"
    ::bruh_34::
    function bruh (a,c,b,...) 
        ::bruh_44::
        ::bruh_22::
    end
    "#;
    // " ...
    // hello
    // hello,...
    // list,two,three
    // hello, this, is, a...
    //     hello, this, is, a,...";

    let mut parser = Parser::new(Tokenizer::new(lua));
    while let Some(list) = parser.parse_block() {
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
