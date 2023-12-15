use std::{
    collections::{HashMap, VecDeque},
    iter::Peekable,
};

use crate::lua::tokenizer::{
    token::{SpanFmt, Token},
    Span, SpanD, Tokenizer, TokenizerError,
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

#[derive(Debug, Clone)]
pub struct Block(pub Vec<Span<Statement>>);

#[derive(Debug, Clone)]
pub enum Statement {
    Assign(Span<Vec<(Span<Var>, Span<Expr>)>>),
    FunctionCall(),
    Label(Label),
    Break,
    Goto(Span<Name>),
    Block(Span<Block>),
    While(Span<Expr>, Span<Block>),
    If(
        Span<Expr>,
        Span<Block>,
        Span<Vec<Span<(Expr, Block)>>>,
        Option<Span<Block>>,
    ),
    Function(Span<FuncName>, Span<FunctionBody>),
    LocalFunction(Span<Name>, Span<FunctionBody>),
    LocalVerDef(Span<AttNameList>),
    LocalVerDec(Span<AttNameList>, Span<ExprList>),
}

#[derive(Debug, Clone)]
pub struct ExprList(pub Vec<Span<Expr>>);

#[derive(Debug, Clone)]
pub struct AttNameList(pub Vec<()>);

#[derive(Debug, Clone)]
pub struct Attrib(pub Span<Name>);

#[derive(Debug, Clone)]
pub struct Label(pub Span<Name>);

#[derive(Debug, Clone)]
pub struct FunctionBody(pub Span<ParamList>, pub Span<Block>);

#[derive(Debug, Clone)]
pub enum Var {
    SimpleName(Name),
}

#[derive(Debug, Clone)]
pub struct NameList(pub Vec<Span<Name>>);

#[derive(Debug, Clone)]
pub struct ParamList {
    pub list: Option<Span<NameList>>,
    pub trailing: bool,
}

#[derive(Debug, Clone)]
pub struct FuncName(pub Span<Name>, pub Span<Vec<Name>>, pub Span<Option<Name>>);

#[derive(Debug, Clone)]
pub struct Name(pub String);

type BoxExpr = Span<Box<Expr>>;

#[derive(Debug, Clone)]
pub enum Expr {
    Or(BoxExpr, BoxExpr),
    And(BoxExpr, BoxExpr),
    Lt(BoxExpr, BoxExpr),
    Gt(BoxExpr, BoxExpr),
    LtEq(BoxExpr, BoxExpr),
    GtEq(BoxExpr, BoxExpr),
    Eq(BoxExpr, BoxExpr),
    Neq(BoxExpr, BoxExpr),
    DotDot(BoxExpr, BoxExpr),
    Add(BoxExpr, BoxExpr),
    Sub(BoxExpr, BoxExpr),
    Mul(BoxExpr, BoxExpr),
    Div(BoxExpr, BoxExpr),
    Mod(BoxExpr, BoxExpr),
    Not(BoxExpr),
    Len(BoxExpr),
    Neg(BoxExpr),
    Power(BoxExpr, BoxExpr),

    DotDotDot,
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
        if !self.queue.is_empty() {
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

    pub fn parse_block(&mut self) -> Option<Span<Block>> {
        let mut vec = Vec::new();
        let first = self.parse_statement()?;
        let mut span = first.span;
        vec.push(first);
        while let Some(next) = self.parse_statement() {
            span = span.extend_range(&next.span);
            vec.push(next);
        }
        Some(Span::new(Block(vec), span))
    }

    fn parse_statement(&mut self) -> Option<Span<Statement>> {
        let Span {
            mut span,
            val: start,
        } = loop {
            if let Some(some) = self.peek_tok() {
                if some.val != Token::Semicolon {
                    break some;
                }
                self.next_tok();
            } else {
                return None;
            }
        };
        let state = match start {
            Token::Break
            | Token::Goto
            | Token::Do
            | Token::While
            | Token::Repeat
            | Token::If
            | Token::For
            | Token::Function
            | Token::Local => {
                match self.next_tok().unwrap().val {
                    Token::Break => Statement::Break,
                    Token::Goto => match self.next_tok() {
                        tok!(Token::Ident(name), name_span) => {
                            span = span.extend_range(&name_span);
                            Statement::Goto(Span::new(Name(name.into()), name_span))
                        }
                        // error cases
                        None => todo!(),
                        Some(_tok) => todo!(),
                    },
                    Token::Do => todo!(),
                    Token::While => todo!(),
                    Token::Repeat => todo!(),
                    Token::If => todo!(),
                    Token::For => todo!(),
                    Token::Function => match self.next_tok() {
                        tok!(Token::Ident(name), ident_span) => {
                            let body = self.parse_function_body();
                            span = span.extend_range(&ident_span).extend_range(&body.span);
                            Statement::LocalFunction(Span::new(Name(name.into()), ident_span), body)
                        }
                        Some(_) => {
                            todo!()
                        }
                        None => {
                            todo!()
                        }
                    },
                    Token::Local => {
                        match self.peek_n::<2>() {
                            [tok!(Token::Ident(_) | Token::Lt), _] => todo!(),

                            [tok!(Token::Function, func_span), tok!(Token::Ident(name), ident_span)] =>
                            {
                                let name = Span::new(Name(name.into()), *ident_span);
                                let func_span = *func_span;
                                let ident_span = *ident_span;
                                self.next_n::<2>();
                                let body = self.parse_function_body();
                                span = span
                                    .extend_range(&func_span)
                                    .extend_range(&ident_span)
                                    .extend_range(&body.span);
                                Statement::LocalFunction(name, body)
                            }

                            // error cases
                            [tok!(Token::Function), _] => todo!(),
                            [None, _] => todo!(),
                            [Some(_tok), _] => todo!(),
                        }
                    }
                    _ => unreachable!(),
                }
            }
            Token::ColonColon => {
                if let Some(label) = self.parse_label() {
                    span = span.extend_range(&label.span);
                    Statement::Label(label.val)
                } else {
                    // Statement::Label(Label(Name(String::new())))
                    todo!()
                }
            }
            _ => return None,
        };
        Some(Span::new(state, span))
    }

    fn parse_function_body(&mut self) -> Span<FunctionBody> {
        let mut span;
        match self.next_tok() {
            tok!(Token::LPar, lpar_span) => {
                span = lpar_span;
            }
            Some(_) => {
                todo!()
            }
            None => {
                todo!()
            }
        }

        let param = if let Some(some) = self.param_list() {
            span = span.extend_range(&some.span);
            some
        } else {
            todo!()
        };

        match self.next_tok() {
            tok!(Token::RPar, rpar_span) => {
                span = span.extend_range(&rpar_span);
            }
            Some(_) => {
                todo!()
            }
            None => {
                todo!()
            }
        }

        let body = self.parse_block();
        let body = if let Some(body) = body {
            span = span.extend_range(&body.span);
            body
        } else {
            // Block(Vec::new())
            todo!()
        };

        match self.next_tok() {
            tok!(Token::End, rpar_span) => {
                span = span.extend_range(&rpar_span);
            }
            Some(_) => {
                todo!()
            }
            None => {
                todo!()
            }
        }

        Span::new(FunctionBody(param, body), span)
    }

    fn parse_attrib(&mut self) -> Option<Span<Attrib>> {
        match self.peek_n::<3>() {
            [tok!(Token::Lt, ltspan), tok!(Token::Ident(ident), identspan), tok!(Token::Gt, gtspan)] =>
            {
                let span = ltspan.extend_range(gtspan);
                let ret = Some(Span::new(
                    Attrib(Span::new(Name(ident.into()), *identspan)),
                    span,
                ));
                // consume the accepted tokens
                self.next_n::<3>();
                ret
            }
            // error cases we can report on
            [tok!(Token::Lt, _ltspan), tok!(Token::Ident(_), _identspan), None] => {
                todo!()
            }
            [tok!(Token::Lt, _ltspan), tok!(Token::Ident(_), _identspan), tok!(_, _gtspan)] => {
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
                let ret = Some(Span::new(
                    Label(Span::new(Name(ident.into()), *identspan)),
                    span,
                ));
                // consume the accepted tokens
                self.next_n::<3>();
                ret
            }
            // error cases we can report on
            [tok!(Token::ColonColon, ltspan), tok!(Token::Ident(ident), identspan), None] => {
                todo!()
            }
            [tok!(Token::ColonColon, ltspan), tok!(Token::Ident(ident), identspan), tok!(_, gtspan)] =>
            {
                todo!()
            }
            _ => None,
        }
    }

    fn name_list(&mut self) -> Option<Span<NameList>> {
        let mut vec = Vec::new();

        let mut span = next_match!(self, tok, span, if Token::Ident(ident), {
            vec.push(Span::new(Name(ident.to_string()), span));
            span
        }else{
            return None;
        });

        loop {
            match self.peek_n::<2>() {
                [tok!(Token::Comma, comma_span), tok!(Token::Ident(name), name_span)] => {
                    span = span.extend_range(&comma_span);
                    span = span.extend_range(&name_span);
                    vec.push(Span::new(Name(name.to_string()), span));
                    self.next_n::<2>();
                }
                _ => break,
            }
        }

        Some(Span::new(NameList(vec), span))
    }

    fn param_list(&mut self) -> Option<Span<ParamList>> {
        let list = self.name_list();

        let trailing;
        let span = match (&list, self.peek_n::<2>()) {
            (Some(v), [tok!(Token::Comma, comma_span), tok!(Token::DotDotDot, dot_span)]) => {
                trailing = true;
                let span = v.span.extend_range(comma_span).extend_range(dot_span);
                self.next_n::<2>();
                span
            }
            (None, [tok!(Token::DotDotDot, dot_span), _]) => {
                trailing = true;
                let span = *dot_span;
                self.next_n::<1>();
                span
            }

            (Some(v), _) => {
                trailing = false;
                v.span
            }
            (None, _) => return None,
        };

        Some(Span::new(ParamList { list, trailing }, span))
    }
}

#[test]
fn test() {
    let lua = r#"
    ::bruh_34::
    function bruh (a,c,b,...) 
        ::bruh_44::
        ::bruh_22::
        function inner_func (aa,cc,bb) 
         break;
         goto WOWOOWOWOOW
        end
    end
    "#;

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