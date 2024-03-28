use std::collections::HashMap;

use crate::core::LErr;

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
    Semicolon,
    Plus,
    Minus,
    Star,
    Slash,
    Nil,
    And,
    Or,
    For,
    While,
    If,
    Else,
    To,
    Return,
    True,
    False,
    Fun,
    Print,
    Var,
    Comment(String),
    Invalid(String),
    Eof,
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
            if self.is_at_end() {
                self.emit(Token::Eof);
                break;
            }
            match c {
                '(' => self.emit(Token::LeftParen),
                ')' => self.emit(Token::RightParen),
                '{' => self.emit(Token::LeftBrace),
                '}' => self.emit(Token::RightBrace),
                ',' => self.emit(Token::Comma),
                '.' => self.emit(Token::Dot),
                ';' => self.emit(Token::Semicolon),
                '+' => self.emit(Token::Plus),
                '-' => self.emit(Token::Minus),
                '*' => self.emit(Token::Star),
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
                '>' => {
                    if self.check_next_is_equal('=') {
                        self.emit(Token::GreaterEqual);
                        self.next();
                    } else {
                        self.emit(Token::Greater);
                    }
                }
                '/' => {
                    if self.check_next_is_equal('/') {
                        let mut rest_of_line: String = String::new();
                        self.next();
                        if self.peek() == Some(&' ') {
                            self.next();
                        }
                        while let Some(c) = self.next() {
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
                    }
                    if c.is_digit(10) {
                        match self.number(c) {
                            Ok(_) => {}
                            Err(e) => return Err(e),
                        }
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
                self.cur.index += 1;
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
        keywords.insert("fun", Token::Fun);
        keywords.insert("nil", Token::Nil);
        keywords.insert("print", Token::Print);
        keywords.insert("return", Token::Return);
        keywords.insert("var", Token::Var);
        keywords.insert("while", Token::While);
        keywords.insert("to", Token::To);

        if keywords.contains_key(keyword) {
            self.emit(keywords[keyword].clone());
        } else {
            self.emit(Token::Identifier(keyword.to_string()));
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, LocToken, Token};

    #[test]
    fn test_int() {
        let input = String::from("45");
        let tokens = setup(input.clone());

        assert_eq!(
            tokens[0].token,
            Token::Int(input.parse::<i64>().expect("Expected valid int."))
        );
    }

    #[test]
    fn test_float() {
        let input = String::from("932874.2313");
        let tokens = setup(input.clone());

        assert_eq!(
            tokens[0].token,
            Token::Float(input.parse::<f64>().expect("Expected valid float.")),
        );
    }

    #[test]
    fn test_greater_equal() {
        let input: String = String::from("()-32* 4 >= 2");
        let tokens = setup(input.clone());
        let found_tokens = filter_tokens(tokens, &Token::GreaterEqual);

        assert_eq!(found_tokens[0].token, Token::GreaterEqual);
    }

    #[test]
    fn test_less_equal() {
        let input: String = String::from("()-32* 4 <= 2");
        let tokens = setup(input.clone());
        let found_tokens = filter_tokens(tokens, &Token::LessEqual);

        assert_eq!(found_tokens[0].token, Token::LessEqual);
    }

    #[test]
    fn test_bang_equal() {
        let input: String = String::from("()-32* 4 != 2");
        let tokens = setup(input.clone());
        let found_tokens = filter_tokens(tokens, &Token::BangEqual);

        assert_eq!(found_tokens[0].token, Token::BangEqual);
    }

    #[test]
    fn test_equal_equal() {
        let input: String = String::from("()-32* 4 == 2");
        let tokens = setup(input.clone());
        let found_tokens = filter_tokens(tokens, &Token::EqualEqual);

        assert_eq!(found_tokens[0].token, Token::EqualEqual);
    }

    #[test]
    fn test_string() {
        let test_string = "test";
        let input: String = String::from("\"") + test_string + "\"";
        let tokens = setup(input.clone());

        assert_eq!(tokens[0].token, Token::String(test_string.to_string()));
    }

    #[test]
    fn test_multiple_strings() {
        let test_string = "test1";
        let test_string2 = "test2";
        let input: String = String::from("\"") + test_string + "\"" + "\"" + test_string2 + "\"";
        let tokens = setup(input.clone());

        assert_eq!(tokens[0].token, Token::String(test_string.to_string()));
        assert_eq!(tokens[1].token, Token::String(test_string2.to_string()));
    }

    #[test]
    fn test_int_float_string() {
        let test_string = "test";
        let input: String = String::from("\"") + test_string + "\"" + "34 434.21";
        let tokens = setup(input.clone());

        assert_eq!(tokens[0].token, Token::String(test_string.to_string()));
        assert_eq!(tokens[1].token, Token::Int(34));
        assert_eq!(tokens[2].token, Token::Float(434.21));
    }

    #[test]
    fn test_multiple_tokens() {
        let input: String = String::from("(-*><=.,;\n");
        let tokens = setup(input.clone());

        let left_parent = filter_tokens(tokens.clone(), &Token::LeftParen);
        let minus = filter_tokens(tokens.clone(), &Token::Minus);
        let star = filter_tokens(tokens.clone(), &Token::Star);
        let greater = filter_tokens(tokens.clone(), &Token::Greater);
        let less_equal = filter_tokens(tokens.clone(), &Token::LessEqual);
        let dot = filter_tokens(tokens.clone(), &Token::Dot);
        let comma = filter_tokens(tokens.clone(), &Token::Comma);
        let semicolon = filter_tokens(tokens.clone(), &Token::Semicolon);

        assert_eq!(left_parent[0].token, Token::LeftParen);
        assert_eq!(minus[0].token, Token::Minus);
        assert_eq!(star[0].token, Token::Star);
        assert_eq!(greater[0].token, Token::Greater);
        assert_eq!(less_equal[0].token, Token::LessEqual);
        assert_eq!(dot[0].token, Token::Dot);
        assert_eq!(comma[0].token, Token::Comma);
        assert_eq!(semicolon[0].token, Token::Semicolon);
    }

    #[test]
    fn test_keywords() {
        let input: String = String::from("and while var asd if");
        let tokens = setup(input.clone());

        assert_eq!(tokens[0].token, Token::And);
        assert_eq!(tokens[1].token, Token::While);
        assert_eq!(tokens[2].token, Token::Var);
        assert_eq!(tokens[3].token, Token::Identifier(String::from("asd")));
        assert_eq!(tokens[4].token, Token::If);
    }

    fn setup(input: String) -> Vec<LocToken> {
        let mut lexer = Lexer::new(&input);
        return lexer.lex().unwrap(); // We can safely expect that unit test cases to always return tokens here.
    }

    fn filter_tokens(tokens: Vec<LocToken>, tokens_to_find: &Token) -> Vec<LocToken> {
        let found_tokens: Vec<LocToken> = tokens
            .into_iter()
            .filter(|token| token.token == *tokens_to_find)
            .collect();

        found_tokens
    }
}
