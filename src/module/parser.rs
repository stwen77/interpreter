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
impl Token {
    pub fn is_next_unary(&self) -> bool {
        use self::Token::*;

        match *self {
            LCurly           | // (+expr) - is unary
            // RCurly           | {expr} - expr not unary & is closing
            LParen           | // {-expr} - is unary
            // RParen           | (expr) - expr not unary & is closing
            LSquare          | // [-expr] - is unary
            // RSquare          | [expr] - expr not unary & is closing
            Plus             |
            UnaryPlus        |
            Minus            |
            UnaryMinus       |
            Multiply         |
            Divide           |
            Colon            |
            Comma            |
            Period           |
            Equals           |
            LessThan         |
            GreaterThan      |
            Bang             |
            LessThanEqual    |
            GreaterThanEqual |
            EqualTo          |
            NotEqualTo       |
            Pipe             |
            Or               |
            Ampersand        |
            And              |
            If               |
            While            |
            PlusAssign       |
            MinusAssign      |
            MultiplyAssign   |
            DivideAssign     |
            LeftShiftAssign  |
            RightShiftAssign |
            AndAssign        |
            OrAssign         |
            XOrAssign        |
            LeftShift        |
            RightShift       |
            XOr              |
            Modulo           |
            ModuloAssign     |
            Return           |
            PowerOf          |
            PowerOfAssign => true,
            _ => false,
        }
    }

    #[allow(dead_code)]
    pub fn is_bin_op(&self) -> bool {
        use self::Token::*;

        match *self {
            RCurly           |
            RParen           |
            RSquare          |
            Plus             |
            Minus            |
            Multiply         |
            Divide           |
            Comma            |
            // Period           | <- does period count?
            Equals           |
            LessThan         |
            GreaterThan      |
            LessThanEqual    |
            GreaterThanEqual |
            EqualTo          |
            NotEqualTo       |
            Pipe             |
            Or               |
            Ampersand        |
            And              |
            PowerOf => true,
            _ => false,
        }
    }

    #[allow(dead_code)]
    pub fn is_un_op(&self) -> bool {
        use self::Token::*;

        match *self {
            UnaryPlus | UnaryMinus | Equals | Bang | Return => true,
            _ => false,
        }
    }
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
    match input.peek() {
        Some(Token::If) => Err(()),
        Some(Token::While) => Err(()),
        Some(Token::Loop) => Err(()),
        Some(Token::Break) => Err(()),
        Some(Token::Return) => Err(()),
        Some(Token::LCurly) => Err(()),
        Some(Token::Var) => Err(()),
        _ => parse_express_statement(input),
    }
}
fn parse_var<'a>(input: &mut Peekable<Chars<'a>>) {}
fn parse_expr<'a>(input: &mut TokenIterator<'a>) -> Result<Expr, ()> {
    match input.peek() {
        Some(Token::RParen) => Ok(Expr::Unit),
        _ => {
            let lhs = r#try!(parse_unary(input));

            parse_binary_operation(input, 0, lhs)
        }
    }
}
fn parse_express_statement<'a>(input: &mut TokenIterator<'a>) -> Result<Statment, ()> {
    let express = match input.peek() {
        Some(Token::RParen) => Expr::Unit,
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
    //let tok = input.last.clone();
    let tok = match input.peek() {
        Some(tok) => tok.clone(),
        None => return Err(()),
    };

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
fn parse_index_expr<'a>(id: String,
                        input: &mut TokenIterator<'a>)
                        -> Result<Expr, ()> {
    if let Ok(idx) = parse_expr(input) {
        println!("!!!parse {:?}",input.last);
        match input.peek() {
            Some(Token::RSquare) => {
                input.next();
                return Ok(Expr::Index(id, Box::new(idx)));
            }
            _ => return Err(()),
        }
    } else {
        return Err(());
    }
}
fn parse_ident_expr<'a>(id: String,
                        input: &mut TokenIterator<'a>)
                        -> Result<Expr, ()> {
    println!("!!!parse {:?}",input.last);
    match input.peek() {
        Some(Token::LParen) => {
            input.next();
            //parse_call_expr(id, input)
            Err(())
        }
        Some(Token::LSquare) => {
            input.next();
            parse_index_expr(id, input)
        }
        _ => Ok(Expr::Identifier(id)),
    }
}
fn parse_primary<'a>(input: &mut TokenIterator<'a>) -> Result<Expr, ()> {
    if let Some(token) = input.next() {
        match token {
            Token::IntConst(ref x) => Ok(Expr::IntConst(*x)),
            Token::FloatConst(ref x) => Ok(Expr::FloatConst(*x)),
            Token::StringConst(ref s) => Ok(Expr::StringConst(s.clone())),
            Token::CharConst(ref c) => Ok(Expr::CharConst(*c)),
            Token::Identifier(ref s) => parse_ident_expr(s.clone(), input),
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
#[derive(Clone)]
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
    fn peek(&mut self) -> Option<Token> {
        let mut temp_iter = self.clone();
        let temp_last = temp_iter.next();
        temp_last
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
                'A'...'Z' | 'a'...'z' | '_' => {
                    let mut result = Vec::new();
                    result.push(c);

                    while let Some(&nxt) = self.char_stream.peek() {
                        match nxt {
                            x if x.is_alphanumeric() || x == '_' => {
                                result.push(x);
                                self.char_stream.next();
                            }
                            _ => break,
                        }
                    }

                    let out: String = result.iter().cloned().collect();
                    match out.as_ref() {
                        "true" => return Some(Token::True),
                        "false" => return Some(Token::False),
                        "let" => return Some(Token::Var),
                        "if" => return Some(Token::If),
                        "else" => return Some(Token::Else),
                        "while" => return Some(Token::While),
                        "loop" => return Some(Token::Loop),
                        "break" => return Some(Token::Break),
                        "return" => return Some(Token::Return),
                        "fn" => return Some(Token::Fn),
                        x => return Some(Token::Identifier(x.to_string())),
                    }
                }
                '"' => match self.parse_string_const('"') {
                    Ok(out) => return Some(Token::StringConst(out)),
                    Err(e) => return Some(Token::LexErr),
                },
                '{' => return Some(Token::LCurly),
                '}' => return Some(Token::RCurly),
                '(' => return Some(Token::LParen),
                ')' => return Some(Token::RParen),
                '[' => return Some(Token::LSquare),
                ']' => return Some(Token::RSquare),
                '+' => {
                    return match self.char_stream.peek() {
                        Some(&'=') => {
                            self.char_stream.next();
                            Some(Token::PlusAssign)
                        }
                        _ if self.last.is_next_unary() => Some(Token::UnaryPlus),
                        _ => Some(Token::Plus),
                    }
                }
                '-' => {
                    return match self.char_stream.peek() {
                        Some(&'=') => {
                            self.char_stream.next();
                            Some(Token::MinusAssign)
                        }
                        _ if self.last.is_next_unary() => Some(Token::UnaryMinus),
                        _ => Some(Token::Minus),
                    }
                }
                '*' => {
                    return match self.char_stream.peek() {
                        Some(&'=') => {
                            self.char_stream.next();
                            Some(Token::MultiplyAssign)
                        }
                        _ => Some(Token::Multiply),
                    }
                }
                ';' => return Some(Token::Semicolon),
                ':' => return Some(Token::Colon),
                ',' => return Some(Token::Comma),
                '.' => return Some(Token::Period),
                '=' => match self.char_stream.peek() {
                    Some(&'=') => {
                        self.char_stream.next();
                        return Some(Token::EqualTo);
                    }
                    _ => return Some(Token::Equals),
                },
                '<' => match self.char_stream.peek() {
                    Some(&'=') => {
                        self.char_stream.next();
                        return Some(Token::LessThanEqual);
                    }
                    Some(&'<') => {
                        self.char_stream.next();
                        return match self.char_stream.peek() {
                            Some(&'=') => {
                                self.char_stream.next();
                                Some(Token::LeftShiftAssign)
                            }
                            _ => {
                                self.char_stream.next();
                                Some(Token::LeftShift)
                            }
                        };
                    }
                    _ => return Some(Token::LessThan),
                },
                '>' => match self.char_stream.peek() {
                    Some(&'=') => {
                        self.char_stream.next();
                        return Some(Token::GreaterThanEqual);
                    }
                    Some(&'>') => {
                        self.char_stream.next();
                        return match self.char_stream.peek() {
                            Some(&'=') => {
                                self.char_stream.next();
                                Some(Token::RightShiftAssign)
                            }
                            _ => {
                                self.char_stream.next();
                                Some(Token::RightShift)
                            }
                        };
                    }
                    _ => return Some(Token::GreaterThan),
                },
                '!' => match self.char_stream.peek() {
                    Some(&'=') => {
                        self.char_stream.next();
                        return Some(Token::NotEqualTo);
                    }
                    _ => return Some(Token::Bang),
                },
                '|' => match self.char_stream.peek() {
                    Some(&'|') => {
                        self.char_stream.next();
                        return Some(Token::Or);
                    }
                    Some(&'=') => {
                        self.char_stream.next();
                        return Some(Token::OrAssign);
                    }
                    _ => return Some(Token::Pipe),
                },
                '&' => match self.char_stream.peek() {
                    Some(&'&') => {
                        self.char_stream.next();
                        return Some(Token::And);
                    }
                    Some(&'=') => {
                        self.char_stream.next();
                        return Some(Token::AndAssign);
                    }
                    _ => return Some(Token::Ampersand),
                },
                '^' => match self.char_stream.peek() {
                    Some(&'=') => {
                        self.char_stream.next();
                        return Some(Token::XOrAssign);
                    }
                    _ => return Some(Token::XOr),
                },
                '%' => match self.char_stream.peek() {
                    Some(&'=') => {
                        self.char_stream.next();
                        return Some(Token::ModuloAssign);
                    }
                    _ => return Some(Token::Modulo),
                },
                '~' => match self.char_stream.peek() {
                    Some(&'=') => {
                        self.char_stream.next();
                        return Some(Token::PowerOfAssign);
                    }
                    _ => return Some(Token::PowerOf),
                },
                _x if _x.is_whitespace() => (),
                _ => return Some(Token::LexErr),
            }
        }
        None
    }
    pub fn parse_string_const(&mut self, enclosing_char: char) -> Result<String, ()> {
        let mut result = vec![];
        while let Some(nxt) = self.char_stream.next() {
            match nxt {
                x if enclosing_char == x => break,
                _ => result.push(nxt),
            }
        }

        let out: String = result.into_iter().collect();
        Ok(out)
    }
}
