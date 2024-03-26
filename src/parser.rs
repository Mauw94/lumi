use crate::{core::LErr, lexer::Token};

#[allow(dead_code)]
#[derive(Debug)]
enum Precedence {
    None = 0,
    Assignment = 1,
    Or = 2,
    And = 3,
    Equality = 4,
    Comparison = 5,
    Term = 6,
    Factor = 7,
    Unary = 8,
    Call = 9,
    Primary = 10,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64),
    Float(f64),
    Identifier(String),
    Binary(Box<Expr>, Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
    Sequence(Vec<Box<Expr>>),
}

pub struct Parser {
    tokens: Vec<Token>,
    i: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, i: 0 }
    }

    fn advance(&mut self) {
        self.i += 1;
    }

    fn peek(&self) -> Option<Token> {
        self.tokens.get(self.i).cloned()
    }

    // fn assignment() -> Result<Expr, LErr> {

    // }

    fn operand(&mut self) -> Result<Expr, LErr> {
        let operand = self.peek();

        match operand {
            Some(Token::Plus) => {
                let lvalue = self.tokens.get(self.i - 1).cloned();
                let rvalue = self.tokens.get(self.i + 1).cloned();

                let l: i64;
                let r: i64;

                match lvalue {
                    Some(Token::Int(value)) => l = value,
                    None | _ => {
                        return Err(LErr::parsing_error("Something went wrong".to_string(), 0))
                    }
                }

                match rvalue {
                    Some(Token::Int(value)) => r = value,
                    None | _ => {
                        return Err(LErr::parsing_error("Something went wrong".to_string(), 0))
                    }
                }

                Ok(Expr::Int(l + r))
            }
            None | _ => {
                self.advance();
                self.expression()
            }
        }

        // Ok()
    }

    pub fn expression(&mut self) -> Result<Expr, LErr> {
        let expr = vec![Box::new(self.operand()?)];

        Ok(Expr::Sequence(expr))
    }
}

/*
    parse expressions in order of precedence
    e.g. 2 + 2
    we first find a number token and store it in a Expr.Binary as left,
    when we find a Plus token we need to find the right side of the expression, starting from a certain point of order of precedence again.
    this next token could be a ( defining a new term.
*/
