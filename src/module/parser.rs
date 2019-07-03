use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Clone)]
pub struct FnDef {
    pub name: String,
    pub params: Vec<String>,
    pub body: Box<Statment>,
}

#[derive(Debug, Clone)]
pub enum Statment {
    If(Box<Expr>, Box<Statment>),
    IfElse(Box<Expr>, Box<Statment>, Box<Statment>),
    While(Box<Expr>, Box<Statment>),
    Loop(Box<Statment>),
    Var(String, Option<Box<Expr>>),
    Block(Vec<Statment>),
    Expr(Box<Expr>),
    Break,
    Return,
    ReturnWithVal(Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    IntConst(i64),
    FloatConst(f64),
    Identifier(String),
    CharConst(char),
    StringConst(String),
    FnCall(String, Vec<Expr>),
    Assignment(Box<Expr>, Box<Expr>),
    Dot(Box<Expr>, Box<Expr>),
    Index(String, Box<Expr>),
    Array(Vec<Expr>),
    True,
    False,
    Unit,
}

#[derive(Debug, Clone)]
pub enum Token {
    IntConst(i64),
    FloatConst(f64),
    Identifier(String),
    CharConst(char),
    StringConst(String),
    LCurly,
    RCurly,
    LParen,
    RParen,
    LSquare,
    RSquare,
    Plus,
    UnaryPlus,
    Minus,
    UnaryMinus,
    Multiply,
    Divide,
    Semicolon,
    Colon,
    Comma,
    Period,
    Equals,
    True,
    False,
    Var,
    If,
    Else,
    While,
    Loop,
    LessThan,
    GreaterThan,
    Bang,
    LessThanEqual,
    GreaterThanEqual,
    EqualTo,
    NotEqualTo,
    Pipe,
    Or,
    Ampersand,
    And,
    Fn,
    Break,
    Return,
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,
    LeftShiftAssign,
    RightShiftAssign,
    AndAssign,
    OrAssign,
    XOrAssign,
    LeftShift,
    RightShift,
    XOr,
    Modulo,
    ModuloAssign,
    PowerOf,
    PowerOfAssign,
    Nothing,
    LexErr,
}
pub fn parse(input: &mut str) {
    //let mut statement = Vec::new();
    //let mut fn_def = Vec::new();

    let mut token_iterator = TokenIterator::new(input);
    while let Some(a) = token_iterator.char_stream.peek() {
        //println!("{}",a);
        match token_iterator.last {
            Token::Fn => (),
            _ => {
                parse_statement(&mut token_iterator);
            }
        }
        token_iterator.next();
    }
}
pub fn parse_statement(input: &mut TokenIterator) -> Result<Statment, ()> {
    match input.last {
        Token::If => Err(()),
        Token::While => Err(()),
        Token::Loop => Err(()),
        Token::Break => Err(()),
        Token::Return => Err(()),
        Token::LCurly => Err(()),
        Token::Var => Err(()),
        _ => Err(()),
    }
}
fn parse_var<'a>(input: &mut Peekable<Chars<'a>>, last_token: &mut Token) {}
pub struct TokenIterator<'a> {
    last: Token,
    char_stream: Peekable<Chars<'a>>,
}
impl<'a> TokenIterator<'a> {
    fn new(input: &'a str) -> Self {
        TokenIterator {
            last: Token::Nothing,
            char_stream: input.chars().peekable(),
        }
    }
    fn next(&mut self) {
        self.last =  match self.inner_next() {
            Some(c) => c,
            None => Token::Nothing,
        };

    }
    fn inner_next(&mut self) -> Option<Token> {
        while let Some(c) = self.char_stream.next() {
            match c {
                '0'...'9' => {
                    let mut result = Vec::new();
                    let mut radix_base: Option<u32> = None;
                    result.push(c);

                    while let Some(&nxt) = self.char_stream.peek() {
                        match nxt {
                            '0'...'9' => {
                                result.push(nxt);
                                self.char_stream.next();
                            }
                            '.' => {
                                result.push(nxt);
                                self.char_stream.next();
                                while let Some(&nxt_float) = self.char_stream.peek() {
                                    match nxt_float {
                                        '0'...'9' => {
                                            result.push(nxt_float);
                                            self.char_stream.next();
                                        }
                                        _ => break,
                                    }
                                }
                            }
                            _ => break,
                        }
                    }

                    let out:String = result.into_iter().collect();
                    if let Ok(val) = out.parse::<i64>() {
                        println!("{}", val);
                        return Some(Token::IntConst(val));
                    } else if let Ok(val) = out.parse::<f64>() {
                        println!("{}", val);
                        return Some(Token::FloatConst(val));
                    }
                    return Some(Token::LexErr);
                }

                _x if _x.is_whitespace() => (),
                _ => return Some(Token::LexErr),
            }
        }
        None
    }
}
