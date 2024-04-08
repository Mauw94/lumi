use std::collections::HashMap;

use crate::{core::LErr, ObjectType};

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct CodeLoc {
    pub line: usize,
    pub index: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LocToken {
    pub token: Token,
    pub start: CodeLoc,
    pub end: CodeLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Float(f64),
    Int(i64),
    String(String),
    Identifier(String),
    IdentifierType(ObjectType),
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Equal,
    EqualEqual,
    BangEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Bang,
    Comma,
    Dot,
    Colon,
    Semicolon,
    Plus,
    Minus,
    Star,
    Slash,
    Nil,
    And,
    Or,
    For,
    Step,
    While,
    If,
    Else,
    To,
    Return,
    True,
    False,
    Fn,
    Print,
    Every,
    Declare,
    Struct,
    Comment(String),
    Invalid(String),
}

pub struct Lexer<'a> {
    code: std::iter::Peekable<std::str::Chars<'a>>,
    start: CodeLoc,
    cur: CodeLoc,
    pub tokens: Vec<LocToken>,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            code: code.chars().peekable(),
            start: CodeLoc { line: 1, index: 0 },
            cur: CodeLoc { line: 1, index: 0 },
            tokens: Vec::new(),
        }
    }

    pub fn lex(&mut self) -> Result<Vec<LocToken>, LErr> {
        while let Some(c) = self.next() {
            match c {
                '(' => self.emit(Token::LeftParen),
                ')' => self.emit(Token::RightParen),
                '{' => self.emit(Token::LeftBrace),
                '}' => self.emit(Token::RightBrace),
                '[' => self.emit(Token::LeftBracket),
                ']' => self.emit(Token::RightBracket),
                ',' => self.emit(Token::Comma),
                '.' => self.emit(Token::Dot),
                ';' => self.emit(Token::Semicolon),
                ':' => self.emit(Token::Colon),
                '+' => self.emit(Token::Plus),
                '-' => {
                    // TODO: undeclare statement?
                    if self.check_next_is_equal('>') {
                        self.emit(Token::Declare);
                        self.next();
                    } else {
                        self.emit(Token::Minus);
                    }
                }
                '*' => self.emit(Token::Star),
                ' ' | '\n' | '\r' => self.start = self.cur,
                '=' => {
                    if self.check_next_is_equal('=') {
                        self.emit(Token::EqualEqual);
                        self.next();
                    } else {
                        self.emit(Token::Equal);
                    }
                }
                '!' => {
                    if self.check_next_is_equal('=') {
                        self.emit(Token::BangEqual);
                        self.next();
                    } else {
                        self.emit(Token::Bang);
                    }
                }
                '<' => {
                    if self.check_next_is_equal('=') {
                        self.emit(Token::LessEqual);
                        self.next();
                    } else {
                        self.emit(Token::Less);
                    }
                }
                '>' => match self.tokens.last() {
                    Some(t) => {
                        if t.token == Token::Declare {
                            {};
                        }
                    }
                    None => {
                        if self.check_next_is_equal('=') {
                            self.emit(Token::GreaterEqual);
                            self.next();
                        } else {
                            self.emit(Token::Greater);
                        }
                    }
                },
                '/' => {
                    if self.check_next_is_equal('/') {
                        let mut rest_of_line: String = String::new();
                        self.next();
                        if self.peek() == Some(&' ') {
                            self.next();
                        }
                        while let Some(c) = self.next() {
                            if c == '\n' {
                                break;
                            }
                            rest_of_line.push(c);
                        }
                        let comment_no_specials = self.remove_r_n(&rest_of_line);
                        self.emit(Token::Comment(comment_no_specials));
                    } else {
                        self.emit(Token::Slash);
                    }
                }
                '"' => match self.string() {
                    Ok(_) => {}
                    Err(e) => return Err(e),
                },
                c => {
                    if c.is_alphabetic() {
                        let mut keyword: String = String::new();
                        keyword.push(c);
                        while let Some(cc) = self.peek().filter(|x| x.is_alphabetic()) {
                            keyword.push(*cc);
                            self.next();
                        }

                        match self.identifier(&keyword) {
                            Ok(_) => {}
                            Err(e) => return Err(e),
                        }
                    } else if c.is_digit(10) {
                        match self.number(c) {
                            Ok(_) => {}
                            Err(e) => return Err(e),
                        }
                    } else {
                        self.emit(Token::Invalid(format!("Unrecognized char: {}", c)))
                    }
                }
            }
        }

        Ok(self.tokens.clone())
    }

    fn emit(&mut self, token: Token) {
        self.tokens.push(LocToken {
            token,
            start: self.start,
            end: self.cur,
        });
        self.start = self.cur;
    }

    fn try_emit_float(&mut self, number_string: String) {
        match number_string.parse::<f64>() {
            Ok(f) => self.emit(Token::Float(f)),
            Err(e) => self.emit(Token::Invalid(format!(
                "lexing: invalid token found: {}",
                e
            ))),
        }
    }

    fn try_emit_int(&mut self, number_string: String) {
        match number_string.parse::<i64>() {
            Ok(f) => self.emit(Token::Int(f)),
            Err(e) => self.emit(Token::Invalid(format!(
                "lexing: invalid token found: {}",
                e
            ))),
        }
    }

    fn check_next_is_equal(&mut self, expected: char) -> bool {
        match self.peek() {
            Some(&c) => {
                return c == expected;
            }
            None => return false,
        }
    }

    fn next(&mut self) -> Option<char> {
        let c = self.code.next();
        match c {
            Some('\n') => {
                self.cur.line += 1;
                self.cur.index = 0;
            }
            Some(_) => {
                self.cur.index += 1;
            }
            None => {}
        }
        c
    }

    fn peek(&mut self) -> Option<&char> {
        self.code.peek()
    }

    fn is_at_end(&mut self) -> bool {
        self.code.peek().is_none()
    }

    fn remove_r_n(&mut self, input: &str) -> String {
        input.chars().filter(|&c| c != '\r' && c != '\n').collect()
    }

    fn number(&mut self, c: char) -> Result<(), LErr> {
        if c.is_whitespace() {
            // skip
        } else if c.is_digit(10) {
            let mut number_string = c.to_string();

            while let Some(number) = self.peek().filter(|d| d.is_digit(10)) {
                number_string.push(*number);
                self.next();
            }
            if self.peek() == Some(&'.') {
                number_string.push('.');
                self.next();

                match self.peek() {
                    Some(&c) if c.is_digit(10) => {
                        while let Some(number) = self.peek().filter(|d| d.is_digit(10)) {
                            number_string.push(*number);
                            self.next();
                        }
                        self.try_emit_float(number_string);
                    }
                    None | _ => {
                        return Err(LErr::lexing_error(
                            String::from("A digit was expected."),
                            self.start,
                            self.cur,
                        ));
                    }
                }
            } else {
                self.try_emit_int(number_string);
            }
        }
        Ok(())
    }

    fn string(&mut self) -> Result<(), LErr> {
        let mut chars: String = String::new();
        while self.peek() != Some(&'"') && !self.is_at_end() {
            let next_char = self.peek().unwrap();
            chars.push(*next_char);
            self.next();
        }

        if self.is_at_end() {
            return Err(LErr::lexing_error(
                String::from("Unterminated string."),
                self.start,
                self.cur,
            ));
        }

        self.next();
        self.emit(Token::String(chars));
        Ok(())
    }

    fn identifier(&mut self, keyword: &str) -> Result<(), LErr> {
        let mut keywords = HashMap::new();
        keywords.insert("and", Token::And);
        keywords.insert("or", Token::Or);
        keywords.insert("if", Token::If);
        keywords.insert("else", Token::Else);
        keywords.insert("false", Token::False);
        keywords.insert("true", Token::True);
        keywords.insert("fn", Token::Fn);
        keywords.insert("nil", Token::Nil);
        keywords.insert("print", Token::Print);
        keywords.insert("return", Token::Return);
        keywords.insert("while", Token::While);
        keywords.insert("to", Token::To);
        keywords.insert("every", Token::Every);
        keywords.insert("step", Token::Step);
        keywords.insert("struct", Token::Struct);

        if keywords.contains_key(keyword) {
            self.emit(keywords[keyword].clone());
        } else if self.object_types().contains_key(keyword) {
            self.emit(self.object_types()[keyword].clone());
        } else {
            self.emit(Token::Identifier(keyword.to_string()));
        }

        Ok(())
    }

    fn object_types(&self) -> HashMap<&'a str, Token> {
        let mut object_types = HashMap::new();
        object_types.insert("int", Token::IdentifierType(ObjectType::Int));
        object_types.insert("float", Token::IdentifierType(ObjectType::Float));
        object_types.insert("str", Token::IdentifierType(ObjectType::String));
        object_types.insert("list", Token::IdentifierType(ObjectType::List));

        object_types
    }
}
