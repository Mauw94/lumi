use std::fmt::{self};

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
pub enum Expr {
    Int(i64),
    Float(f64),
    String(String),
    Identifier(String),
    Declare(String, ObjectType, Box<LumiExpr>),
    Literal(LiteralValue),
    Unary(Token, Box<LumiExpr>),
    Logical(Box<LumiExpr>, Token, Box<LumiExpr>),
    Binary(Box<LumiExpr>, Token, Box<LumiExpr>),
    Assign(Box<LumiExpr>, Box<LumiExpr>),
    Sequence(Vec<Box<LumiExpr>>),
    List(Vec<Box<LumiExpr>>),
    Index(String, Box<LumiExpr>),
    Print(Box<LumiExpr>),
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
            Expr::Literal(literal) => match literal {
                LiteralValue::False => write!(f, "FALSE"),
                LiteralValue::True => write!(f, "TRUE"),
                LiteralValue::Nil => write!(f, "NIL"),
            },
            Expr::Print(expr) => write!(f, "PRINT {}", expr),
            Expr::Declare(t, _obj_type, expr) => write!(f, "DECLARE {} = {}", t, expr),
            Expr::List(exprs) => {
                write!(f, "LIST: ")?;
                for expr in exprs {
                    write!(f, "{}", expr)?;
                }
                Ok(())
            }
            Expr::Index(var_name, expr) => write!(f, "VAR {} INDEX {}", var_name, expr),
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

    fn previous(&self) -> Option<LocToken> {
        self.tokens.get(self.i - 1).cloned()
    }

    #[allow(dead_code)]
    fn current(&self) -> Option<LocToken> {
        self.tokens.get(self.i).cloned()
    }

    fn current_token(&self) -> Option<Token> {
        match self.tokens.get(self.i) {
            Some(t) => Some(t.token.clone()),
            None => None,
        }
    }

    fn peek_loc(&self) -> CodeLoc {
        match self.peek() {
            Some(t) => t.start,
            None => CodeLoc { line: 0, index: 0 },
        }
    }

    #[allow(dead_code)]
    fn consume(&mut self, token: Token, msg: String, code_loc: CodeLoc) -> Result<(), LErr> {
        if self.check(token) {
            self.advance();
            return Ok(());
        }

        return Err(LErr::parsing_error(msg, code_loc));
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
                    end: self.peek_loc(),
                    expr: Expr::Int(value),
                });
            }
            Some(Token::Float(value)) => {
                self.advance();
                return Ok(LumiExpr {
                    start,
                    end: self.peek_loc(),
                    expr: Expr::Float(value),
                });
            }
            Some(Token::String(value)) => {
                self.advance();
                return Ok(LumiExpr {
                    start,
                    end: self.peek_loc(),
                    expr: Expr::String(value),
                });
            }
            Some(Token::Identifier(value)) => {
                self.advance();
                if self.matcher(&[Token::Colon]) {
                    match self.current_token() {
                        Some(Token::IdentifierType(obj_type)) => {
                            self.advance();
                            if self.matcher(&[Token::Declare]) {
                                // FIXME re-assigning a list a = [1,2,3] does not work atm.
                                if self.matcher(&[Token::LeftBracket]) {
                                    self.parse_list_expr(value, start, obj_type)
                                } else {
                                    let expr = self.unary()?;
                                    return Ok(LumiExpr {
                                        start,
                                        end: expr.end,
                                        expr: Expr::Declare(value, obj_type, Box::new(expr)),
                                    });
                                }
                            } else {
                                return Err(LErr::parsing_error(
                                    format!(
                                        "Expect '{}' declaration after type definition",
                                        obj_type.get_type_name(),
                                    ),
                                    start,
                                ));
                            }
                        }
                        None | _ => {
                            return Err(LErr::parsing_error(
                                "Expect a type after ':'".to_string(),
                                start,
                            ))
                        }
                    }
                } else if self.matcher(&[Token::Declare]) {
                    if self.matcher(&[Token::LeftBracket]) {
                        self.parse_list_expr(value, start, ObjectType::List)
                    } else {
                        let expr = self.unary()?;
                        return Ok(LumiExpr {
                            start,
                            end: expr.end,
                            expr: Expr::Declare(value, ObjectType::None, Box::new(expr)),
                        });
                    }
                } else if self.matcher(&[Token::LeftBracket]) {
                    let e = self.primary()?;
                    match self.consume(
                        Token::RightBracket,
                        "Expect ']' after index.".to_string(),
                        self.peek_loc(),
                    ) {
                        Ok(_) => {
                            return Ok(LumiExpr {
                                start,
                                end: e.end,
                                expr: Expr::Index(value, Box::new(e)),
                            })
                        }
                        Err(e) => Err(e),
                    }
                } else {
                    return Ok(LumiExpr {
                        start,
                        end: self.peek_loc(),
                        expr: Expr::Identifier(value),
                    });
                }
            }
            Some(Token::True) => {
                self.advance();
                return Ok(LumiExpr {
                    start,
                    end: self.peek_loc(),
                    expr: Expr::Literal(LiteralValue::True),
                });
            }
            Some(Token::False) => {
                self.advance();
                return Ok(LumiExpr {
                    start,
                    end: self.peek_loc(),
                    expr: Expr::Literal(LiteralValue::False),
                });
            }
            Some(Token::Nil) => {
                self.advance();
                return Ok(LumiExpr {
                    start,
                    end: self.peek_loc(),
                    expr: Expr::Literal(LiteralValue::Nil),
                });
            }
            None | _ => {
                return Err(LErr::parsing_error(
                    "Expect expression".to_string(),
                    self.peek_loc(),
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
                        // TODO: check if types are correct, we can't mix ints and strings in a list.
                        exprs.push(Box::new(e));
                    } else {
                        self.advance();
                    }
                }
                None => {}
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
            expr: Expr::Declare(value, obj_type, Box::new(list_expr)),
        });
    }

    fn call(&mut self) -> Result<LumiExpr, LErr> {
        let expr = self.primary()?;

        loop {
            if self.matcher(&[Token::LeftParen]) {
            } else if self.matcher(&[Token::Dot]) {
            } else {
                break;
            }
        }

        Ok(expr)
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
                        self.peek_loc(),
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
                        self.peek_loc(),
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
                        self.peek_loc(),
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
                        self.peek_loc(),
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
                        self.peek_loc(),
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
                        self.peek_loc(),
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
                        self.peek_loc(),
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
                        self.peek_loc(),
                    ))
                }
            }
        }
        Ok(expr)
    }

    fn expression(&mut self) -> Result<LumiExpr, LErr> {
        let expr = self.assignment()?;

        let start = self.peek_loc();
        let end = start;

        Ok(LumiExpr {
            start,
            end,
            expr: Expr::Sequence(vec![Box::new(expr)]),
        })
    }

    fn statement(&mut self) -> Result<LumiExpr, LErr> {
        let start = self.peek_loc();
        if self.matcher(&[Token::Print]) {
            let expr = self.expression()?;
            return Ok(LumiExpr {
                start,
                end: expr.end,
                expr: Expr::Print(Box::new(expr)),
            });
        }

        self.expression()
    }

    pub fn parse(&mut self) -> Result<LumiExpr, LErr> {
        self.remove_comments();
        if self.tokens.len() == 0 {
            return Err(LErr::parsing_error(
                "No tokens found.".to_string(),
                CodeLoc { line: 1, index: 0 },
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
}
