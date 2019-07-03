
use std::str::Chars;
use std::iter::Peekable;

#[derive(Debug, Clone)]
pub struct FnDef{
    pub name : String,
    pub params: Vec<String>,
    pub body:Box<Statment>,
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


    let mut last_token:Token = Token::Nothing;
    let mut input_iter = input.chars().peekable();
    while let Some(_) = input_iter.peek(){
        match last_token {
            Token::Fn => (),
            _ => {parse_statement(&mut input_iter, &mut last_token);},
        }
        input_iter.next();
        
    }
    println!("");
}
pub fn parse_statement(input: &mut Peekable<Chars<>>, last_token:&mut Token)->Result<Statment, ()>{
    match last_token {
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
fn parse_var(input: &mut Peekable<Chars<>>, last_token:&mut Token){

}
fn next_token(input: &mut Peekable<Chars<>>, last_token:&mut Token){
    
}