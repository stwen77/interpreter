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
    TempStub,
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
    let mut statement: Vec<Statment> = Vec::new();
    //let mut fn_def = Vec::new();

    let mut token_iterator = TokenIterator::new(input);
    while let Some(a) = token_iterator.char_stream.peek() {
        //println!("{}",a);
        match token_iterator.last {
            Token::Fn => (),
            _ => {
                statement.push(parse_statement(&mut token_iterator).unwrap());
            }
        }
        token_iterator.next();
    }
    println!("parsed statment vec :{:?}", statement);
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
        _ => parse_express_statement(input),
    }
}
fn parse_var<'a>(input: &mut Peekable<Chars<'a>>) {}
fn parse_express_statement<'a>(input: &mut TokenIterator<'a>) -> Result<Statment, ()> {
    let express = match input.last {
        Token::RParen => Expr::Unit,
        _ => {
            let lhs = parse_unary(input).unwrap_or(Expr::IntConst(0));
            parse_binary_operation(input, 0, lhs).unwrap()
            //println!("lhs {:?}", lhs);error
        }
    };
    println!("expr res {:?}", express);
    Ok(Statment::Expr(Box::new(express)))
}
fn parse_binary_operation<'a>(
    input: &mut TokenIterator<'a>,
    prec: i32,
    lhs: Expr,
) -> Result<Expr, ()> {
    let mut lhs_curr = lhs;

    loop {
        let mut curr_prec = -1;

        if let curr_op = input.last.clone() {
            curr_prec = get_precedence(&curr_op);
        }

        if curr_prec < prec {
            return Ok(lhs_curr);
        }

        if let Some(op_token) = input.next() {
            let mut rhs = r#try!(parse_unary(input));

            let mut next_prec = -1;

            if let next_op = input.last.clone() {
                next_prec = get_precedence(&next_op);
            }

            if curr_prec < next_prec {
                rhs = r#try!(parse_binary_operation(input, curr_prec + 1, rhs));
            } else if curr_prec >= 100 {
                // Always bind right to left for precedence over 100
                rhs = r#try!(parse_binary_operation(input, curr_prec, rhs));
            }

            lhs_curr = match op_token {
                Token::Plus => Expr::FnCall("+".to_string(), vec![lhs_curr, rhs]),
                Token::Minus => Expr::FnCall("-".to_string(), vec![lhs_curr, rhs]),
                Token::Multiply => Expr::FnCall("*".to_string(), vec![lhs_curr, rhs]),
                Token::Divide => Expr::FnCall("/".to_string(), vec![lhs_curr, rhs]),
                Token::Equals => Expr::Assignment(Box::new(lhs_curr), Box::new(rhs)),
                Token::PlusAssign => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FnCall("+".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::MinusAssign => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FnCall("-".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::Period => Expr::Dot(Box::new(lhs_curr), Box::new(rhs)),
                Token::EqualTo => Expr::FnCall("==".to_string(), vec![lhs_curr, rhs]),
                Token::NotEqualTo => Expr::FnCall("!=".to_string(), vec![lhs_curr, rhs]),
                Token::LessThan => Expr::FnCall("<".to_string(), vec![lhs_curr, rhs]),
                Token::LessThanEqual => Expr::FnCall("<=".to_string(), vec![lhs_curr, rhs]),
                Token::GreaterThan => Expr::FnCall(">".to_string(), vec![lhs_curr, rhs]),
                Token::GreaterThanEqual => Expr::FnCall(">=".to_string(), vec![lhs_curr, rhs]),
                Token::Or => Expr::FnCall("||".to_string(), vec![lhs_curr, rhs]),
                Token::And => Expr::FnCall("&&".to_string(), vec![lhs_curr, rhs]),
                Token::XOr => Expr::FnCall("^".to_string(), vec![lhs_curr, rhs]),
                Token::OrAssign => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FnCall("|".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::AndAssign => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FnCall("&".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::XOrAssign => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FnCall("^".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::MultiplyAssign => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FnCall("*".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::DivideAssign => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FnCall("/".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::Pipe => Expr::FnCall("|".to_string(), vec![lhs_curr, rhs]),
                Token::LeftShift => Expr::FnCall("<<".to_string(), vec![lhs_curr, rhs]),
                Token::RightShift => Expr::FnCall(">>".to_string(), vec![lhs_curr, rhs]),
                Token::LeftShiftAssign => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FnCall("<<".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::RightShiftAssign => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FnCall(">>".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::Ampersand => Expr::FnCall("&".to_string(), vec![lhs_curr, rhs]),
                Token::Modulo => Expr::FnCall("%".to_string(), vec![lhs_curr, rhs]),
                Token::ModuloAssign => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FnCall("%".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::PowerOf => Expr::FnCall("~".to_string(), vec![lhs_curr, rhs]),
                Token::PowerOfAssign => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FnCall("~".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                _ => return Err(()),
            };
        }
    }
}

fn get_precedence(token: &Token) -> i32 {
    match *token {
        Token::Equals
        | Token::PlusAssign
        | Token::MinusAssign
        | Token::MultiplyAssign
        | Token::DivideAssign
        | Token::LeftShiftAssign
        | Token::RightShiftAssign
        | Token::AndAssign
        | Token::OrAssign
        | Token::XOrAssign
        | Token::ModuloAssign
        | Token::PowerOfAssign => 10,
        Token::Or | Token::XOr | Token::Pipe => 11,
        Token::And | Token::Ampersand => 12,
        Token::LessThan
        | Token::LessThanEqual
        | Token::GreaterThan
        | Token::GreaterThanEqual
        | Token::EqualTo
        | Token::NotEqualTo => 15,
        Token::Plus | Token::Minus => 20,
        Token::Divide | Token::Multiply | Token::PowerOf => 40,
        Token::LeftShift | Token::RightShift => 50,
        Token::Modulo => 60,
        Token::Period => 100,
        _ => -1,
    }
}
fn parse_unary<'a>(input: &mut TokenIterator<'a>) -> Result<Expr, ()> {
    let tok = input.last.clone();

    match tok {
        Token::UnaryMinus => {
            input.next();
            Ok(Expr::FnCall("-".to_string(), vec![parse_primary(input)?]))
        }
        Token::UnaryPlus => {
            input.next();
            parse_primary(input)
        }
        Token::Bang => {
            input.next();
            Ok(Expr::FnCall("!".to_string(), vec![parse_primary(input)?]))
        }
        _ => parse_primary(input),
    }
}
fn parse_primary<'a>(input: &mut TokenIterator<'a>) -> Result<Expr, ()> {
    if let Some(token) = input.next() {
        match token {
            Token::IntConst(ref x) => Ok(Expr::IntConst(*x)),
            Token::FloatConst(ref x) => Ok(Expr::FloatConst(*x)),
            Token::StringConst(ref s) => Ok(Expr::StringConst(s.clone())),
            Token::CharConst(ref c) => Ok(Expr::CharConst(*c)),
            //Token::Identifier(ref s) => parse_ident_expr,
            //Token::LParen => parse_paren_expr,
            //Token::LSquare => parse_array_expr,
            Token::True => Ok(Expr::True),
            Token::False => Ok(Expr::False),
            Token::LexErr => {
                println!("Error: primary");
                Err(())
            }
            _ => {
                println!("Can't parse: {:?}", token);
                Err(())
            }
        }
    } else {
        Err(())
    }
}
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
    fn next(&mut self) -> Option<Token> {
        self.last = match self.inner_next() {
            Some(c) => c,
            None => Token::Nothing,
        };
        //println!("self last restored : {:?}", self.last);
        Some(self.last.clone())
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

                    let out: String = result.into_iter().collect();
                    if let Ok(val) = out.parse::<i64>() {
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
