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
    Binary(Box<Expr>, Token, Box<Expr>),
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

    fn is_at_end(&self) -> bool {
        false
    }

    fn advance(&mut self) -> Option<Token> {
        if !self.is_at_end() {
            println!("increasing current");
            self.i += 1;
        }
        return self.previous();
    }

    fn peek(&self) -> Option<Token> {
        self.tokens.get(self.i).cloned()
    }

    fn previous(&self) -> Option<Token> {
        println!("{}", self.i);
        self.tokens.get(self.i - 1).cloned()
    }

    fn current(&self) -> Option<Token> {
        self.tokens.get(self.i).cloned()
    }

    fn check(&self, token: Token) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.peek() == Some(token.clone()) {
            println!("token matched [{:?}]", token);
            return true;
        }

        false
    }

    fn matcher(&mut self, tokens: &[Token]) -> bool {
        println!("Matching tokens..");
        for token in tokens {
            if self.check(token.clone()) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn primary(&mut self) -> Result<Expr, LErr> {
        // TODO: rest of logic
        match self.current() {
            Some(Token::Int(value)) => {
                self.advance();
                return Ok(Expr::Int(value));
            }
            None | _ => return Err(LErr::parsing_error("Expect expression".to_string(), 1)),
        }
    }

    fn call(&mut self) -> Result<Expr, LErr> {
        let expr = self.primary();

        // TODO: logic

        return expr;
    }

    fn unary(&mut self) -> Result<Expr, LErr> {
        // let expr = self.or();

        // TODO: logic

        return self.call();
    }

    fn factor(&mut self) -> Result<Expr, LErr> {
        let expr = self.unary();

        // TODO: logic

        return expr;
    }

    fn term(&mut self) -> Result<Expr, LErr> {
        let mut expr = self.factor()?;

        while self.matcher(&[Token::Plus, Token::Minus]) {
            match self.previous() {
                Some(op) => {
                    let right = self.term()?;
                    expr = Expr::Binary(Box::new(expr), op, Box::new(right));
                }
                None | _ => return Err(LErr::parsing_error("Something kaput".to_string(), 1)),
            }
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, LErr> {
        let expr = self.term();

        // TODO: logic

        return expr;
    }

    fn equality(&mut self) -> Result<Expr, LErr> {
        let expr = self.comparison();

        // TODO: logic

        return expr;
    }

    fn and(&mut self) -> Result<Expr, LErr> {
        let expr = self.equality();

        // TODO: logic

        return expr;
    }

    fn or(&mut self) -> Result<Expr, LErr> {
        let expr = self.and();

        // TODO: logic

        return expr;
    }

    fn assignment(&mut self) -> Result<Expr, LErr> {
        let expr = self.or();

        // TODO: logic

        return expr;
    }

    pub fn expression(&mut self) -> Result<Expr, LErr> {
        let expr = vec![Box::new(self.term()?)];

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
