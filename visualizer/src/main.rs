use std::collections::VecDeque;

use bad_lua::lua::{compiler::parser::*, tokenizer::{SpanD, Span, Tokenizer}};

use rocket::{*, fs::{FileServer, Options}};

// this is our get route which will be requested at the "/" location wherever it is mounted
#[get("/compile/<lua>")]
fn build_html(lua: &str) -> String {
    let mut string = String::new();
    let mut visitor = Visitor::new(lua, &mut string);
    let mut parser = Parser::new(Tokenizer::new(lua));
    if let Some(list) = parser.parse_block() {
        visitor.visit_block(&list);
    }
    // visitor.end();
    string
}

#[get("/tokenize/<lua>")]
fn tokenize(lua: &str) -> String {
    let mut string = String::new();
    let tokenizer = Tokenizer::new(lua).include_comments();
    let mut errors = VecDeque::new();

    let mut last: usize = 0;
    for token in tokenizer.into_iter(){
        let token = match token{
            Ok(ok) => ok,
            Err(err) => {
                errors.push_back(*err);
                continue;
            },
        };

        while let Some(Span{ span, ..}) = errors.front(){
            if span.offset < token.span.offset{
                if last < span.offset as usize{
                    string.push_str(&lua[last..span.offset as usize]);
                    last = span.offset as usize;
                }
                string.push_str("<span style=\"color:#ff6464\" title='");

                use std::fmt::Write;
                let next = errors.pop_front().unwrap();
                write!(&mut string, "{:?}", next.val).unwrap();
                string.push_str("'>");
                string.push_str(&lua[last..last + next.span.len as usize]);
                last += next.span.len as usize;
                string.push_str("</span>");
            }else{
                break;
            }
        }

        use bad_lua::lua::tokenizer::token::Token as T;

        let color = match token.val{
            T::Plus | 
            T::Minus | 
            T::Asterick | 
            T::Slash | 
            T::Percent | 
            T::Carrot | 
            T::Octothorp | 
            T::Ampersand | 
            T::Tilde | 
            T::Pipe | 
            T::LtLt | 
            T::GtGt | 
            T::SlashSlash | 
            T::EqEq | 
            T::TildeEq | 
            T::LtEq | 
            T::GtEq | 
            T::Lt | 
            T::Gt | 
            T::Eq | 
            T::LPar | 
            T::RPar | 
            T::LBrace | 
            T::RBrace | 
            T::LBracket | 
            T::RBracket | 
            T::ColonColon | 
            T::Semicolon | 
            T::Colon | 
            T::Comma | 
            T::Dot | 
            T::DotDot | 
            T::DotDotDot => "#d4d4d4",

            T::And | 
            T::Break | 
            T::Do | 
            T::Else | 
            T::Elseif | 
            T::End | 
            T::False | 
            T::For | 
            T::Function | 
            T::Goto | 
            T::If | 
            T::In | 
            T::Local | 
            T::Nil | 
            T::Not | 
            T::Or | 
            T::Repeat | 
            T::Return | 
            T::Then | 
            T::True | 
            T::Until | 
            T::While => "#b084c0",

            T::Ident(_) => "#9cdcfe",

            T::StringLiteral(_) => "#ce9178",
            T::IntegerLiteral(_) => "#dcdcaa",
            T::FloatingLiteral(_) => "#dcdcaa",
            T::SingleLineComment(_) => "#638a3e",
            T::MultiLineComment(_) => "#638a3e",
        };

        if last < token.span.offset as usize{
            string.push_str(&lua[last..token.span.offset as usize]);
            last = token.span.offset as usize;
        }
        if last < token.span.offset as usize + token.span.len as usize{
            string.push_str("<span style=\"color:");
            string.push_str(color);
            string.push_str("\" title='");
            use std::fmt::Write;
            write!(&mut string, "{:?}", token.val).unwrap();
            string.push_str("'>");

            while let Some(Span{ span, ..}) = errors.front(){
                if span.offset < token.span.offset + token.span.len{
                    if last < span.offset as usize{
                        string.push_str(&lua[last..span.offset as usize]);
                        last = span.offset as usize;
                    }
                    string.push_str("<span style=\"color:#ff6464\" title='");
    
                    use std::fmt::Write;
                    let next = errors.pop_front().unwrap();
                    write!(&mut string, "{:?}", next.val).unwrap();
                    string.push_str("'>");
                    string.push_str(&lua[last..last + next.span.len as usize]);
                    last += next.span.len as usize;
                    string.push_str("</span>");
                }else{
                    break;
                }
            }
            if last < token.span.len as usize + token.span.offset as usize{
                string.push_str(&lua[last..last + token.span.len as usize]);
                last = token.span.len as usize + token.span.offset as usize;
            }
            string.push_str("</span>");
        }
        
    }

    while let Some(Span{ span, val: next}) = errors.pop_front(){
            if last < span.offset as usize{
                string.push_str(&lua[last..span.offset as usize]);
                last = span.offset as usize;
            }
            if last < span.offset as usize + span.len as usize{
                string.push_str("<span style=\"color:#ff6464\" title='");
                use std::fmt::Write;
                write!(&mut string, "{:?}", next).unwrap();
                string.push_str("'>");
                string.push_str(&lua[last..last + span.len as usize]);
                last += span.len as usize;
                string.push_str("</span>");
            }
    }
        
    string

}

// start the web server and mount our get route at "/api". Can replace /api with anything
// or just leave it as "/" as the default location
#[launch]
fn rocket() -> _ {
  rocket::build().mount("/api", routes![build_html, tokenize])
  .mount(
    "/",
    FileServer::new(
        "./visualizer/static/",
        Options::DotFiles | Options::Index | Options::IndexFile,
    )
    .rank(5),
)
}


struct Visitor<'a, T: std::fmt::Write> {
    og: &'a str,
    out: T,
}

impl<'a, T: std::fmt::Write> Visitor<'a, T> {
    pub fn new(og: &'a str, out: T) -> Self {
        Self { og, out }
    }
    fn out(&mut self, data: &str) {
        self.out.write_str(data).unwrap()
    }

    fn leaf(&mut self, text: &str, span: &SpanD) {
        self.out("<li>");
        write!(
            self.out,
            "<code col={} len={} line={} offset={}>",
            span.col, span.len, span.line, span.offset
        )
        .unwrap();
        self.out(text);
        self.out("</code></li>");
    }

    fn a_leaf(&mut self, text: &str) {
        self.out("<li><code>");
        self.out(text);
        self.out("</code></li>");
    }

    fn non_leaf(&mut self, text: &str, span: &SpanD, inside: impl FnOnce(&mut Self)) {
        write!(
            self.out,
            "<li><code col={} len={} line={} offset={}>",
            span.col, span.len, span.line, span.offset
        )
        .unwrap();
        self.out(text);
        self.out("</code><ul>");
        inside(self);
        self.out("</ul></li>");
    }
    fn a_non_leaf(&mut self, text: &str, inside: impl FnOnce(&mut Self)) {
        self.out("<li><code>");
        self.out(text);
        self.out("</code><ul>");
        inside(self);
        self.out("</ul></li>");
    }

    pub fn visit_block(&mut self, block: &Span<Block>) {
        self.non_leaf("block", &block.span, |myself| {
            for statement in &block.val.0 {
                myself.visit_statement(statement);
            }
        });
    }

    pub fn visit_statement(&mut self, statement: &Span<Statement>) {
        match &statement.val {
            Statement::Assign(_) => todo!(),
            Statement::FunctionCall() => todo!(),
            Statement::Label(label) => self.non_leaf("label", &statement.span, |myself| {
                myself.leaf(&label.0.val.0, &label.0.span)
            }),
            Statement::Break => self.leaf("break", &statement.span),
            Statement::Goto(label) => self.non_leaf("goto", &statement.span, |myself| {
                myself.leaf(&label.val.0, &label.span)
            }),
            Statement::Block(block) => self.visit_block(block),
            Statement::While(_, _) => todo!(),
            Statement::If(_, _, _, _) => todo!(),
            Statement::Function(name, body) =>  self.non_leaf("funcdef", &statement.span, |myself| {
                myself.visit_funcname(name);
                myself.visit_funcbody(body);
            }),
            Statement::LocalFunction(name, body) => self.non_leaf("localfuncdef", &statement.span, |myself| {
                myself.leaf(&name.val.0, &name.span);
                myself.visit_funcbody(body);
            }),
            Statement::LocalVerDef(_) => todo!(),
            Statement::LocalVerDec(_, _) => todo!(),
        }
    }

    pub fn visit_funcname(&mut self, name: &Span<FuncName>){
        self.leaf("funcname", &name.span)
    }
    pub fn visit_funcbody(&mut self, body: &Span<FunctionBody>){
        self.non_leaf("funcbody", &body.span, |myself|{
            
            myself.visit_paramlist(&body.val.0);
            myself.visit_block(&body.val.1);
        })

    }

    fn visit_paramlist(&mut self, list: &Span<ParamList>) {
        self.non_leaf("paramlist", &list.span, |myself|{
        
            if let Some(list) = &list.val.list{
                myself.visit_namelist(list);
            };
            if list.val.trailing{
                myself.a_leaf("trailing");
            };
        })
    }

    fn visit_namelist(&mut self, list: &Span<NameList>) {
        self.non_leaf("namelist", &list.span, |myself|{
            for name in &list.val.0{
                myself.leaf(&name.val.0, &name.span);
            }
        })
    }
}
