use crate::core::LErr;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Float(f64),
    Int(i32),
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
    Comment(String),
    Invalid(String),
    Eof,
}

pub struct Lexer<'a> {
    code: std::iter::Peekable<std::str::Chars<'a>>,
    line: i8,
    pub tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            code: code.chars().peekable(),
            line: 1,
            tokens: Vec::new(),
        }
    }

    // TODO: strings
    // TODO: look for keywords
    pub fn lex(&mut self) -> Result<Vec<Token>, LErr> {
        let mut error_occured: bool = false;
        let mut error_message: String = String::new();

        while let Some(c) = self.next() {
            if error_occured {
                return Err(LErr::lexing_error(error_message.to_string(), self.line));
            }
            if self.is_at_end() {
                self.emit(Token::Eof);
                break;
            }
            match c {
                '\n' => {
                    self.line += 1;
                    self.next();
                }
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
                    self.next();
                }
                '"' => {
                    // TODO: check until another " is found, that'll be a string value
                    // when no " is found throw error for unterminated string
                    // check error handling in jlox
                }
                c => {
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
                                    while let Some(number) = self.peek().filter(|d| d.is_digit(10))
                                    {
                                        number_string.push(*number);
                                        self.next();
                                    }
                                    self.try_emit_float(number_string);
                                }
                                None | _ => {
                                    error_message = String::from("A digit was expected");
                                    error_occured = true;
                                }
                            }
                        } else {
                            self.try_emit_int(number_string);
                        }
                    }
                }
            }
        }

        Ok(self.tokens.clone())
    }

    fn emit(&mut self, token: Token) {
        // TODO: DEBUG flag so we only log when in debug
        println!("Emitting token: {:?}", token);
        self.tokens.push(token);
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
        match number_string.parse::<i32>() {
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
        self.code.next()
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
}

#[cfg(test)]
mod tests {
    use super::{Lexer, Token};

    #[test]
    fn test_int() {
        let input = String::from("45");
        let tokens = setup(input.clone());

        assert_eq!(
            tokens[0],
            Token::Int(input.parse::<i32>().expect("Expected valid int."))
        );
    }

    #[test]
    fn test_float() {
        let input = String::from("932874.2313");
        let tokens = setup(input.clone());

        assert_eq!(
            tokens[0],
            Token::Float(input.parse::<f64>().expect("Expected valid float.")),
        );
    }

    #[test]
    fn test_comment() {
        let input: String = String::from("2.2 * ) _ // this is a comment\r\n 23");
        let tokens = setup(input.clone());
        let comment_tokens: Vec<Token> = tokens
            .into_iter()
            .filter(|token| {
                if let Token::Comment(_) = token {
                    true
                } else {
                    false
                }
            })
            .collect();

        assert_eq!(
            comment_tokens[0],
            Token::Comment(String::from("this is a comment 23"))
        );
    }

    #[test]
    fn test_comment2() {
        let input: String = String::from("2 + 2 * (8-2) //this is also a comment.");
        let tokens = setup(input.clone());
        let comment_tokens: Vec<Token> = tokens
            .into_iter()
            .filter(|token| {
                if let Token::Comment(_) = token {
                    true
                } else {
                    false
                }
            })
            .collect();

        assert_eq!(
            comment_tokens[0],
            Token::Comment(String::from("this is also a comment."))
        );
    }

    #[test]
    fn test_greater_equal() {
        let input: String = String::from("()-32* 4 >= 2");
        let tokens = setup(input.clone());
        let found_tokens = filter_tokens(tokens, &Token::GreaterEqual);

        assert_eq!(found_tokens[0], Token::GreaterEqual);
    }

    #[test]
    fn test_less_equal() {
        let input: String = String::from("()-32* 4 <= 2");
        let tokens = setup(input.clone());
        let found_tokens = filter_tokens(tokens, &Token::LessEqual);

        assert_eq!(found_tokens[0], Token::LessEqual);
    }

    #[test]
    fn test_bang_equal() {
        let input: String = String::from("()-32* 4 != 2");
        let tokens = setup(input.clone());
        let found_tokens = filter_tokens(tokens, &Token::BangEqual);

        assert_eq!(found_tokens[0], Token::BangEqual);
    }

    #[test]
    fn test_equal_equal() {
        let input: String = String::from("()-32* 4 == 2");
        let tokens = setup(input.clone());
        let found_tokens = filter_tokens(tokens, &Token::EqualEqual);

        assert_eq!(found_tokens[0], Token::EqualEqual);
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

        assert_eq!(left_parent[0], Token::LeftParen);
        assert_eq!(minus[0], Token::Minus);
        assert_eq!(star[0], Token::Star);
        assert_eq!(greater[0], Token::Greater);
        assert_eq!(less_equal[0], Token::LessEqual);
        assert_eq!(dot[0], Token::Dot);
        assert_eq!(comma[0], Token::Comma);
        assert_eq!(semicolon[0], Token::Semicolon);
    }

    fn setup(input: String) -> Vec<Token> {
        let mut lexer = Lexer::new(&input);
        return lexer.lex().unwrap(); // We can safely expect that unit test cases to always return tokens here.
    }

    fn filter_tokens(tokens: Vec<Token>, tokens_to_find: &Token) -> Vec<Token> {
        let found_tokens: Vec<Token> = tokens
            .into_iter()
            .filter(|token| token == tokens_to_find)
            .collect();

        found_tokens
    }
}
