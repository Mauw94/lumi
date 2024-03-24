#[derive(Debug, Clone)]
pub enum Token {
    Float(f64),
    Int(i32),
    String,
    LefTParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
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
    // TODO: when error occurs in Token we stop?
    // after a . a digit has to be present or else it's invalid
    pub fn lex(&mut self) {
        while let Some(c) = self.next() {
            match c {
                '\n' => {
                    self.line += 1;
                    self.next();
                }
                '(' => self.emit(Token::LeftBrace),
                ')' => self.emit(Token::RightBrace),
                '+' => self.emit(Token::Plus),
                '-' => self.emit(Token::Minus),
                '*' => self.emit(Token::Star),
                '/' => self.emit(Token::Slash),
                c => {
                    if c.is_whitespace() {
                        // skip
                    } else if c.is_digit(10) {
                        let mut number_string = c.to_string();

                        while let Some(number) = self.peek().filter(|d| d.is_digit(10)) {
                            number_string.push(*number);
                            self.next();
                        }
                        // emit a float
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
                                // TODO: better error handling here
                                Some(_) => panic!("Has to be a digit"),
                                None => panic!("Has to be a digit."),
                            }
                        } else {
                            // emit int
                            self.try_emit_int(number_string);
                        }
                    }
                }
            }
        }
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

    fn next(&mut self) -> Option<char> {
        // TODO: later needs check to see if this char is a newline
        self.code.next()
    }

    fn peek(&mut self) -> Option<&char> {
        self.code.peek()
    }
}
