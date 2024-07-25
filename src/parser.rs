use std::{
    fmt::{self},
    rc::Rc,
};

use crate::{
    core::LErr,
    lexer::{CodeLoc, LocToken, Token},
    ObjectType,
};

// Precedence order, can/will be extended.
// None = 0,
// Assignment = 1,
// Or = 2,
// And = 3,
// Equality = 4,
// Comparison = 5,
// Term = 6,
// Factor = 7,
// Unary = 8,
// Call = 9,
// Primary = 10,

#[derive(Debug, Clone)]
pub struct LumiExpr {
    pub start: CodeLoc,
    pub end: CodeLoc,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    True,
    False,
    Nil,
}

#[derive(Debug, Clone)]
pub enum GetType {
    Property,
    Function,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64),
    Float(f64),
    String(String),
    Identifier(String),
    Declare(String, ObjectType, Option<Box<LumiExpr>>),
    Literal(LiteralValue),
    Unary(Token, Box<LumiExpr>),
    Logical(Box<LumiExpr>, Token, Box<LumiExpr>),
    If(Box<LumiExpr>, Box<LumiExpr>, Option<Box<LumiExpr>>),
    For(
        String,
        Box<LumiExpr>,
        Box<LumiExpr>,
        Box<LumiExpr>,
        Vec<Box<LumiExpr>>,
    ),
    Fn(String, Rc<Vec<Box<String>>>, Rc<Vec<Box<LumiExpr>>>), // fn name, params, expressions
    Call(Box<LumiExpr>, Option<Vec<Box<LumiExpr>>>),          // name, arguments
    Namespace(String, CodeLoc, CodeLoc, bool), // namespace only needs the name of the internal function, start codeloc, end codeloc and wether or not to include or exclude it
    Get(Box<LumiExpr>, String, Option<Vec<Box<LumiExpr>>>, GetType),
    Binary(Box<LumiExpr>, Token, Box<LumiExpr>),
    Assign(Box<LumiExpr>, Box<LumiExpr>),
    Sequence(Vec<Box<LumiExpr>>),
    Block(Vec<Box<LumiExpr>>),
    List(Vec<Box<LumiExpr>>),
    Index(String, Box<LumiExpr>),
    Return(Option<Box<LumiExpr>>), // Make this Option, and LErr can throw Break and Return Err so we "cancel" the rest of the expression.
    Print(Box<LumiExpr>),
    Struct(String, Rc<Vec<Box<String>>>, Rc<Vec<Box<LumiExpr>>>),
}

impl fmt::Display for LumiExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expr)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Int(val) => write!(f, "INT {}", val),
            Expr::Float(val) => write!(f, "FLOAT {}", val),
            Expr::String(val) => write!(f, "STRING {}", val),
            Expr::Identifier(name) => write!(f, "IDNETIFIER {}", name),
            Expr::Unary(token, expr) => write!(f, "UNARY {:?}({})", token, expr),
            Expr::Logical(left, op, right) => write!(f, "LOGIXAL ({} {:?} {})", left, op, right),
            Expr::Binary(left, op, right) => write!(f, "BINARY ({} {:?} {})", left, op, right),
            Expr::Assign(lhs, rhs) => write!(f, "ASSIGN ({} = {})", lhs, rhs),
            Expr::Sequence(expressions) => {
                write!(f, "SEQ [")?;
                let mut iter = expressions.iter();
                if let Some(expr) = iter.next() {
                    write!(f, "{}", expr)?;
                    for expr in iter {
                        write!(f, ", {}", expr)?;
                    }
                }
                write!(f, "]")
            }
            Expr::Block(expressions) => {
                write!(f, "BLOCK [")?;
                let mut iter = expressions.iter();
                if let Some(expr) = iter.next() {
                    write!(f, "{}", expr)?;
                    for expr in iter {
                        write!(f, ", {}", expr)?;
                    }
                }
                write!(f, "]")
            }
            Expr::Literal(literal) => match literal {
                LiteralValue::False => write!(f, "FALSE"),
                LiteralValue::True => write!(f, "TRUE"),
                LiteralValue::Nil => write!(f, "NIL"),
            },
            Expr::Print(expr) => write!(f, "PRINT {}", expr),
            Expr::Declare(t, _obj_type, expr) => write!(f, "DECLARE {} = {:?}", t, expr),
            Expr::List(exprs) => {
                write!(f, "LIST: ")?;
                for expr in exprs {
                    write!(f, "{}", expr)?;
                }
                Ok(())
            }
            Expr::Index(var_name, expr) => write!(f, "VAR {} INDEX {}", var_name, expr),
            Expr::If(condition, body, else_branch) => match else_branch {
                Some(end) => write!(f, "CONDITION {} BODY {} ELSE? {}", condition, body, end),
                None => write!(f, "CONDITION {} BODY {}", condition, body),
            },
            Expr::Fn(fn_name, _parameters, _expressions) => write!(f, "FN NAME {}", fn_name),
            Expr::Call(callee, arguments) => {
                write!(f, "callee {:?} arguments {:?}", callee, arguments)
            }
            Expr::For(index, from, to, step, body) => {
                write!(
                    f,
                    "from {} to {} step {}. body {:?}, indexer {}",
                    from, to, step, body, index
                )
            }
            Expr::Return(expr) => {
                write!(f, "RETURN {:?}", expr)
            }
            Expr::Struct(name, params, body) => {
                write!(f, "name {} params {:?}, body {:?}", name, params, body)
            }
            Expr::Get(e, callee, arguments, get_type) => {
                write!(
                    f,
                    "expr: {} callee: {:?} arguments?: {:?} arguments get type: {:?}",
                    e, callee, arguments, get_type
                )
            }
            Expr::Namespace(name, _start, _end, _bool) => write!(f, "NAMESPACE {:?}", name),
        }
    }
}

pub struct Parser {
    tokens: Vec<LocToken>,
    i: usize,
}

impl Parser {
    pub fn new(tokens: Vec<LocToken>) -> Self {
        Self { tokens, i: 0 }
    }

    fn is_at_end(&self) -> bool {
        self.i == self.tokens.len()
    }

    fn advance(&mut self) -> Option<LocToken> {
        if !self.is_at_end() {
            self.i += 1;
        }
        return self.previous();
    }

    fn peek(&self) -> Option<LocToken> {
        self.tokens.get(self.i).cloned()
    }

    fn peek_token(&self) -> Option<Token> {
        match self.tokens.get(self.i) {
            Some(t) => {
                return Some(t.token.clone());
            }
            None => None,
        }
    }

    fn peek_next_token(&self) -> Option<Token> {
        match self.tokens.get(self.i + 1) {
            Some(t) => {
                return Some(t.token.clone());
            }
            None => None,
        }
    }

    fn previous(&self) -> Option<LocToken> {
        self.tokens.get(self.i - 1).cloned()
    }

    fn current(&self) -> Option<LocToken> {
        self.tokens.get(self.i).cloned()
    }

    fn current_token(&self) -> Option<Token> {
        match self.tokens.get(self.i) {
            Some(t) => Some(t.token.clone()),
            None => None,
        }
    }

    fn end_loc(&self) -> CodeLoc {
        match self.previous() {
            Some(t) => t.end,
            None => CodeLoc { line: 0, index: 0 }, // Should not be possible.
        }
    }

    fn peek_loc(&self) -> CodeLoc {
        match self.peek() {
            Some(t) => t.start,
            None => CodeLoc { line: 0, index: 0 },
        }
    }

    fn consume(&mut self, token: Token, msg: String, loc_token: LocToken) -> Result<(), LErr> {
        if self.check(token) {
            self.advance();
            return Ok(());
        }

        return Err(LErr::parsing_error(msg, loc_token));
    }

    fn check(&self, token: Token) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.peek_token() == Some(token.clone()) {
            return true;
        }

        false
    }

    fn check_next(&self, token: Token) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.peek_next_token() == Some(token.clone()) {
            return true;
        }

        false
    }

    fn matcher(&mut self, tokens: &[Token]) -> bool {
        for token in tokens {
            if self.check(token.clone()) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn primary(&mut self) -> Result<LumiExpr, LErr> {
        let start = self.peek_loc();

        match self.current_token() {
            Some(Token::Int(value)) => {
                self.advance();
                return Ok(LumiExpr {
                    start,
                    end: self.end_loc(),
                    expr: Expr::Int(value),
                });
            }
            Some(Token::Float(value)) => {
                self.advance();
                return Ok(LumiExpr {
                    start,
                    end: self.end_loc(),
                    expr: Expr::Float(value),
                });
            }
            Some(Token::String(value)) => {
                self.advance();
                return Ok(LumiExpr {
                    start,
                    end: self.end_loc(),
                    expr: Expr::String(value),
                });
            }
            Some(Token::Identifier(ident)) => {
                self.advance();
                if self.matcher(&[Token::Colon]) {
                    match self.current_token() {
                        Some(t) => match t {
                            Token::IdentifierType(obj_type) => {
                                self.advance();
                                if self.matcher(&[Token::Declare]) {
                                    // FIXME re-assigning a list a = [1,2,3] does not work atm.
                                    if self.matcher(&[Token::LeftBracket]) {
                                        // TODO be able to define data type
                                        self.parse_list_expr(ident, start, obj_type)
                                    } else {
                                        let expr = self.unary()?;
                                        return Ok(LumiExpr {
                                            start,
                                            end: expr.start,
                                            expr: Expr::Declare(
                                                ident,
                                                obj_type,
                                                Some(Box::new(expr)),
                                            ),
                                        });
                                    }
                                } else {
                                    return Ok(LumiExpr {
                                        start,
                                        end: self.end_loc(),
                                        expr: Expr::Declare(ident, obj_type, None),
                                    });
                                }
                            }
                            Token::Int(i) => {
                                self.advance();
                                // for loop
                                while self.matcher(&[Token::To]) {
                                    let from = LumiExpr {
                                        start,
                                        end: self.end_loc(),
                                        expr: Expr::Int(i),
                                    };
                                    let to = match self.primary() {
                                        Ok(expr) => match expr.expr {
                                            Expr::Int(_) => expr,
                                            Expr::Identifier(_) => expr,
                                            _ => {
                                                return Err(LErr::parsing_error(
                                                    "Expected int value after 'to' keyword"
                                                        .to_string(),
                                                    self.previous().unwrap(),
                                                ))
                                            }
                                        },
                                        Err(e) => return Err(e),
                                    };
                                    self.consume(
                                        Token::Step,
                                        "Expect 'step' keyword after for declaration".to_string(),
                                        self.previous().unwrap(),
                                    )?;
                                    let step = match self.primary() {
                                        Ok(expr) => match expr.expr {
                                            Expr::Int(_) => expr,
                                            _ => {
                                                return Err(LErr::parsing_error(
                                                    "Expected int value after 'step' keyword"
                                                        .to_string(),
                                                    self.previous().unwrap(),
                                                ))
                                            }
                                        },
                                        Err(e) => return Err(e),
                                    };
                                    self.consume(
                                        Token::LeftBrace,
                                        "Expect '{' before for body".to_string(),
                                        self.previous().unwrap(),
                                    )?;

                                    let mut exprs: Vec<Box<LumiExpr>> = Vec::new();

                                    while !self.matcher(&[Token::RightBrace]) {
                                        match self.current_token() {
                                            Some(_) => {
                                                exprs.push(Box::new(self.statement()?));
                                            }
                                            None => {
                                                self.consume(
                                                    self.previous().unwrap().token,
                                                    "Expect '}' after for body.".to_string(),
                                                    self.previous().unwrap(),
                                                )?;
                                            }
                                        }
                                    }

                                    return Ok(LumiExpr {
                                        start,
                                        end: to.end,
                                        expr: Expr::For(
                                            ident,
                                            Box::new(from),
                                            Box::new(to),
                                            Box::new(step),
                                            exprs,
                                        ),
                                    });
                                }
                                return Err(LErr::parsing_error(
                                    "expected start of for?".to_string(),
                                    self.previous().unwrap(),
                                ));
                            }
                            _ => {
                                return Err(LErr::parsing_error(
                                    "something something".to_string(),
                                    self.previous().unwrap(),
                                ))
                            }
                        },

                        None => {
                            return Err(LErr::parsing_error(
                                "Expect a type after ':'".to_string(),
                                self.previous().unwrap(),
                            ))
                        }
                    }
                } else if self.matcher(&[Token::Declare]) {
                    if self.matcher(&[Token::LeftBracket]) {
                        self.parse_list_expr(ident, start, ObjectType::List)
                    } else {
                        let expr = self.unary()?;
                        return Ok(LumiExpr {
                            start,
                            end: expr.end,
                            expr: Expr::Declare(ident, ObjectType::None, Some(Box::new(expr))),
                        });
                    }
                } else if self.matcher(&[Token::LeftBracket]) {
                    let e = self.assignment()?;
                    match self.consume(
                        Token::RightBracket,
                        "Expect ']' after index.".to_string(),
                        self.previous().unwrap(),
                    ) {
                        Ok(_) => {
                            return Ok(LumiExpr {
                                start,
                                end: e.end,
                                expr: Expr::Index(ident, Box::new(e)),
                            })
                        }
                        Err(e) => Err(e),
                    }
                } else {
                    return Ok(LumiExpr {
                        start,
                        end: self.end_loc(),
                        expr: Expr::Identifier(ident),
                    });
                }
            }
            Some(Token::True) => {
                self.advance();
                return Ok(LumiExpr {
                    start,
                    end: self.end_loc(),
                    expr: Expr::Literal(LiteralValue::True),
                });
            }
            Some(Token::False) => {
                self.advance();
                return Ok(LumiExpr {
                    start,
                    end: self.end_loc(),
                    expr: Expr::Literal(LiteralValue::False),
                });
            }
            Some(Token::Nil) => {
                self.advance();
                return Ok(LumiExpr {
                    start,
                    end: self.end_loc(),
                    expr: Expr::Literal(LiteralValue::Nil),
                });
            }
            None | _ => {
                return Err(LErr::parsing_error(
                    "Expect expression".to_string(),
                    self.previous().unwrap(),
                ))
            }
        }
    }

    fn parse_list_expr(
        &mut self,
        value: String,
        start: CodeLoc,
        obj_type: ObjectType,
    ) -> Result<LumiExpr, LErr> {
        let end = self.peek_loc();
        let mut exprs: Vec<Box<LumiExpr>> = Vec::new();
        while !self.matcher(&[Token::RightBracket]) {
            match self.current_token() {
                Some(t) => {
                    if t != Token::Comma {
                        let e = self.primary()?;
                        exprs.push(Box::new(e));
                    } else {
                        self.advance();
                    }
                }
                None => {
                    self.consume(
                        self.previous().unwrap().token,
                        "Expect ']' after list declaration".to_string(),
                        self.previous().unwrap(),
                    )?;
                }
            };
        }
        let list_expr = LumiExpr {
            start,
            end,
            expr: Expr::List(exprs),
        };
        return Ok(LumiExpr {
            start,
            end,
            expr: Expr::Declare(value, obj_type, Some(Box::new(list_expr))),
        });
    }

    fn call(&mut self) -> Result<LumiExpr, LErr> {
        let mut expr = self.primary()?;
        let start = self.peek_loc();

        loop {
            if self.matcher(&[Token::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.matcher(&[Token::Dot]) {
                let name = match self.current_token() {
                    Some(Token::Identifier(n)) => n,
                    _ => {
                        return Err(LErr::parsing_error(
                            "Expect property name after '.'".to_string(),
                            self.previous().unwrap(),
                        ))
                    }
                };

                let property: bool = !self.check_next(Token::LeftParen);

                if property {
                    return Ok(LumiExpr {
                        start,
                        end: self.peek_loc(),
                        expr: Expr::Get(Box::new(expr), name, None, GetType::Property),
                    });
                } else {
                    self.advance();
                    self.consume(
                        Token::LeftParen,
                        "Expected '(' after property call.".to_string(),
                        self.previous().unwrap(),
                    )?;

                    let mut arguments: Vec<Box<LumiExpr>> = Vec::new();

                    // Parse all arguments if anys
                    if !self.check(Token::RightParen) {
                        loop {
                            if self.matcher(&[Token::Comma]) {
                                continue;
                            }
                            if arguments.len() >= 255 {
                                return Err(LErr::parsing_error(
                                    "Can't have more than 255 arguments.".to_string(),
                                    self.previous().unwrap(),
                                ));
                            }
                            arguments.push(Box::new(self.assignment()?));
                            if self.matcher(&[Token::RightParen]) {
                                break;
                            }
                        }
                    } else {
                        // No arguments look for closing parentheses
                        self.consume(
                            Token::RightParen,
                            "Expected ')' after property call.".to_string(),
                            self.previous().unwrap(),
                        )?;
                    }

                    return Ok(LumiExpr {
                        start,
                        end: self.peek_loc(),
                        expr: Expr::Get(Box::new(expr), name, Some(arguments), GetType::Function),
                    });
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: LumiExpr) -> Result<LumiExpr, LErr> {
        let mut arguments: Vec<Box<LumiExpr>> = Vec::new();
        let start = self.peek_loc();
        if !self.check(Token::RightParen) {
            loop {
                if self.matcher(&[Token::Comma]) {
                    continue;
                }
                if arguments.len() >= 255 {
                    return Err(LErr::parsing_error(
                        "Can't have more than 255 arguments.".to_string(),
                        self.previous().unwrap(),
                    ));
                }
                arguments.push(Box::new(self.assignment()?));
                if self.matcher(&[Token::RightParen]) {
                    break;
                }
            }
        }

        let call_args = match arguments.is_empty() {
            true => {
                self.advance();
                None
            }
            false => Some(arguments),
        };
        return Ok(LumiExpr {
            start,
            end: self.peek_loc(),
            expr: Expr::Call(Box::new(callee), call_args),
        });
    }

    fn unary(&mut self) -> Result<LumiExpr, LErr> {
        let start = self.peek_loc();
        if self.matcher(&[Token::Bang, Token::Minus]) {
            match self.previous() {
                Some(op) => {
                    let right = self.unary()?;
                    return Ok(LumiExpr {
                        start,
                        end: right.end,
                        expr: Expr::Unary(op.token, Box::new(right)),
                    });
                }
                None => {
                    return Err(LErr::parsing_error(
                        "No operator was found.".to_string(),
                        self.previous().unwrap(),
                    ))
                }
            }
        }

        return self.call();
    }

    fn factor(&mut self) -> Result<LumiExpr, LErr> {
        let start = self.peek_loc();
        let mut expr = self.unary()?;

        while self.matcher(&[Token::Slash, Token::Star]) {
            match self.previous() {
                Some(op) => {
                    let right = self.unary()?;
                    expr = LumiExpr {
                        start,
                        end: expr.end,
                        expr: Expr::Binary(Box::new(expr), op.token, Box::new(right)),
                    }
                }
                None => {
                    return Err(LErr::parsing_error(
                        "No operator was found.".to_string(),
                        self.previous().unwrap(),
                    ))
                }
            }
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<LumiExpr, LErr> {
        let start = self.peek_loc();
        let mut expr = self.factor()?;

        while self.matcher(&[Token::Plus, Token::Minus]) {
            match self.previous() {
                Some(op) => {
                    let right = self.term()?;
                    expr = LumiExpr {
                        start,
                        end: expr.end,
                        expr: Expr::Binary(Box::new(expr), op.token, Box::new(right)),
                    }
                }
                None => {
                    return Err(LErr::parsing_error(
                        "No operator was found.".to_string(),
                        self.previous().unwrap(),
                    ))
                }
            }
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<LumiExpr, LErr> {
        let start = self.peek_loc();
        let mut expr = self.term()?;

        while self.matcher(&[
            Token::Greater,
            Token::GreaterEqual,
            Token::Less,
            Token::LessEqual,
        ]) {
            match self.previous() {
                Some(op) => {
                    let right = self.term()?;
                    expr = LumiExpr {
                        start,
                        end: expr.end,
                        expr: Expr::Binary(Box::new(expr), op.token, Box::new(right)),
                    }
                }
                None => {
                    return Err(LErr::parsing_error(
                        "No operator token found.".to_string(),
                        self.previous().unwrap(),
                    ))
                }
            }
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<LumiExpr, LErr> {
        let start = self.peek_loc();
        let mut expr = self.comparison()?;

        while self.matcher(&[Token::BangEqual, Token::EqualEqual]) {
            match self.previous() {
                Some(op) => {
                    let right = self.comparison()?;
                    expr = LumiExpr {
                        start,
                        end: expr.end,
                        expr: Expr::Binary(Box::new(expr), op.token, Box::new(right)),
                    }
                }
                None => {
                    return Err(LErr::parsing_error(
                        "No operator token found.".to_string(),
                        self.previous().unwrap(),
                    ))
                }
            }
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<LumiExpr, LErr> {
        let start = self.peek_loc();
        let mut expr = self.equality()?;

        while self.matcher(&[Token::And]) {
            match self.previous() {
                Some(op) => {
                    let right = self.equality()?;
                    expr = LumiExpr {
                        start,
                        end: expr.end,
                        expr: Expr::Logical(Box::new(expr), op.token, Box::new(right)),
                    }
                }
                None => {
                    return Err(LErr::parsing_error(
                        "No operator token found.".to_string(),
                        self.previous().unwrap(),
                    ))
                }
            }
        }

        Ok(expr)
    }

    fn or(&mut self) -> Result<LumiExpr, LErr> {
        let start = self.peek_loc();
        let mut expr = self.and()?;

        while self.matcher(&[Token::Or]) {
            match self.previous() {
                Some(op) => {
                    let right = self.and()?;
                    expr = LumiExpr {
                        start: start.clone(),
                        end: expr.end,
                        expr: Expr::Logical(Box::new(expr), op.token, Box::new(right)),
                    }
                }
                None => {
                    return Err(LErr::parsing_error(
                        "No operator token found.".to_string(),
                        self.previous().unwrap(),
                    ))
                }
            }
        }

        Ok(expr)
    }

    fn assignment(&mut self) -> Result<LumiExpr, LErr> {
        let start = self.peek_loc();
        let mut expr = self.or()?;

        while self.matcher(&[Token::Equal]) {
            match self.previous() {
                Some(_) => {
                    let value = self.assignment()?;
                    expr = LumiExpr {
                        start: start.clone(),
                        end: expr.end,
                        expr: Expr::Assign(Box::new(expr), Box::new(value)),
                    }
                }
                None => {
                    return Err(LErr::parsing_error(
                        "Invalid assignment target.".to_string(),
                        self.previous().unwrap(),
                    ))
                }
            }
        }
        Ok(expr)
    }

    fn statement(&mut self) -> Result<LumiExpr, LErr> {
        let start = self.peek_loc();

        // return statement.
        if self.matcher(&[Token::Return]) {
            let start = self.peek_loc();

            if self.check(Token::Semicolon) {
                self.advance();
                return Ok(LumiExpr {
                    start,
                    end: self.end_loc(),
                    expr: Expr::Return(None),
                });
            } else {
                let value = self.statement()?;

                return Ok(LumiExpr {
                    start,
                    end: self.end_loc(),
                    expr: Expr::Return(Some(Box::new(value))),
                });
            }
        }
        // struct declaration.
        if self.matcher(&[Token::Struct]) {
            let _start = self.peek_loc();
            let struct_name = match self.current_token() {
                Some(Token::Identifier(name)) => name,
                _ => {
                    return Err(LErr::parsing_error(
                        "Expect struct name".to_string(),
                        self.previous().unwrap(),
                    ))
                }
            };
            self.advance();
            self.consume(
                Token::LeftParen,
                "Expect '(' after struct name.".to_string(),
                self.previous().unwrap(),
            )?;
            // TODO: parsing parameters and body is the same as a function, can be moved.
            let mut parameters: Vec<Box<String>> = Vec::new();
            if !self.check(Token::RightParen) {
                loop {
                    if self.matcher(&[Token::Comma]) {
                        continue;
                    }
                    let param_name = match self.current_token() {
                        Some(Token::Identifier(name)) => name,
                        _ => {
                            return Err(LErr::parsing_error(
                                "Expect parameter name".to_string(),
                                self.previous().unwrap(),
                            ))
                        }
                    };
                    parameters.push(Box::new(param_name));
                    self.advance();
                    if self.matcher(&[Token::RightParen]) {
                        break;
                    }
                }
                self.consume(
                    Token::LeftBrace,
                    "Expect '{' before struct body.".to_string(),
                    self.previous().unwrap(),
                )?;
                let body = self.block()?;
                return Ok(LumiExpr {
                    start,
                    end: self.peek_loc(),
                    expr: Expr::Struct(struct_name, Rc::new(parameters), Rc::new(body)),
                });
            } else {
                self.advance();
                self.consume(
                    Token::LeftBrace,
                    "Expect '{' before struct body.".to_string(),
                    self.previous().unwrap(),
                )?;
                let body = self.block()?;

                return Ok(LumiExpr {
                    start,
                    end: self.peek_loc(),
                    expr: Expr::Struct(struct_name, Rc::new(parameters), Rc::new(body)),
                });
            }
        }
        // include (namespace) statement.
        if self.matcher(&[Token::Include]) {
            let start = self.peek_loc();
            let namespace_name = match self.current_token() {
                Some(Token::Identifier(name)) => name,
                _ => {
                    return Err(LErr::parsing_error(
                        "Expect namespace name".to_string(),
                        self.previous().unwrap(),
                    ))
                }
            };
            let end = self.peek_loc();
            return Ok(LumiExpr {
                start,
                end,
                expr: Expr::Namespace(namespace_name, start, self.peek_loc(), true),
            });
        }
        if self.matcher(&[Token::Exclude]) {
            let start = self.peek_loc();
            let namespace_name = match self.current_token() {
                Some(Token::Identifier(name)) => name,

                _ => {
                    return Err(LErr::parsing_error(
                        "Expect namespace name".to_string(),
                        self.previous().unwrap(),
                    ))
                }
            };
            let end = self.peek_loc();
            return Ok(LumiExpr {
                start,
                end,
                expr: Expr::Namespace(namespace_name, start, end, false),
            });
        }
        // function statement.
        if self.matcher(&[Token::Fn]) {
            let start = self.peek_loc();
            let fn_name = match self.current_token() {
                Some(Token::Identifier(name)) => name,
                _ => {
                    return Err(LErr::parsing_error(
                        "Expect function name".to_string(),
                        self.previous().unwrap(),
                    ))
                }
            };
            self.advance();
            self.consume(
                Token::LeftParen,
                "Expect '(' after function name".to_string(),
                self.previous().unwrap(),
            )?;
            let mut parameters: Vec<Box<String>> = Vec::new();
            if !self.check(Token::RightParen) {
                loop {
                    if self.matcher(&[Token::Comma]) {
                        continue;
                    }
                    let param_name = match self.current_token() {
                        Some(Token::Identifier(name)) => name,
                        _ => {
                            return Err(LErr::parsing_error(
                                "Expect parameter name.".to_string(),
                                self.previous().unwrap(),
                            ))
                        }
                    };
                    if parameters.len() >= 255 {
                        return Err(LErr::parsing_error(
                            "Can't have more than 255 parameters.".to_string(),
                            self.previous().unwrap(),
                        ));
                    }
                    parameters.push(Box::new(param_name));
                    self.advance();
                    // here we also "consume" the right parentheses
                    if self.matcher(&[Token::RightParen]) {
                        break;
                    }
                }
                self.consume(
                    Token::LeftBrace,
                    "Expect '{' before function body.".to_string(),
                    self.current().unwrap(),
                )?;
                let expressions = self.block()?;
                return Ok(LumiExpr {
                    start,
                    end: self.peek_loc(),
                    expr: Expr::Fn(fn_name, Rc::new(parameters), Rc::new(expressions)),
                });
            } else {
                self.advance();
                self.consume(
                    Token::LeftBrace,
                    "Expect '{' before function body.".to_string(),
                    self.current().unwrap(),
                )?;
                let expressions = self.block()?;
                return Ok(LumiExpr {
                    start,
                    end: self.peek_loc(),
                    expr: Expr::Fn(fn_name, Rc::new(Vec::new()), Rc::new(expressions)),
                });
            }
        }
        // if statement.
        if self.matcher(&[Token::If]) {
            self.consume(
                Token::LeftParen,
                "Expect '(' after if.".to_string(),
                self.current().unwrap(),
            )?;
            let condition = self.assignment()?;
            self.consume(
                Token::RightParen,
                "Expect ')' after if condition".to_string(),
                self.current().unwrap(),
            )?;
            let body = self.statement()?;
            let else_branch = if self.matcher(&[Token::Else]) {
                Some(Box::new(self.statement()?))
            } else {
                None
            };

            return Ok(LumiExpr {
                start,
                end: self.peek_loc(),
                expr: Expr::If(Box::new(condition), Box::new(body), else_branch),
            });
        }
        // print statement.
        if self.matcher(&[Token::Print]) {
            let expr = self.assignment()?;

            return Ok(LumiExpr {
                start,
                end: expr.end,
                expr: Expr::Print(Box::new(expr)),
            });
        }
        // block statement (wrapped by '{ }' ).
        if self.matcher(&[Token::LeftBrace]) {
            let mut exprs: Vec<Box<LumiExpr>> = Vec::new();
            let start = self.peek_loc();
            while !self.check(Token::RightBrace) && !self.is_at_end() {
                exprs.push(Box::new(self.statement()?));
            }
            self.consume(
                Token::RightBrace,
                "Expect '}' after block.".to_string(),
                self.current().unwrap(),
            )?;
            return Ok(LumiExpr {
                start,
                end: self.peek_loc(),
                expr: Expr::Block(exprs),
            });
        }

        self.assignment()
    }

    pub fn parse(&mut self) -> Result<LumiExpr, LErr> {
        self.remove_comments();
        if self.tokens.len() == 0 {
            return Err(LErr::parsing_error(
                "No tokens found.".to_string(),
                self.previous().unwrap(),
            ));
        }
        let mut expr = vec![Box::new(self.statement()?)];

        let start = self.peek_loc();
        let end = start;

        while !self.is_at_end() {
            let e = self.statement()?;
            expr.push(Box::new(e));
        }

        Ok(LumiExpr {
            start,
            end,
            expr: Expr::Sequence(expr),
        })
    }

    fn remove_comments(&mut self) {
        self.tokens.retain_mut(|c| match &mut c.token {
            Token::Comment(_s) => {
                // do something with the comments here?
                false
            }
            _ => true,
        });
    }

    fn block(&mut self) -> Result<Vec<Box<LumiExpr>>, LErr> {
        let mut exprs: Vec<Box<LumiExpr>> = Vec::new();
        // let start = self.peek_loc();
        while !self.check(Token::RightBrace) && !self.is_at_end() {
            exprs.push(Box::new(self.statement()?));
        }
        self.consume(
            Token::RightBrace,
            "Expect '}' after block.".to_string(),
            self.current().unwrap(),
        )?;

        return Ok(exprs);
    }
}
