#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Number(i32),
    Add,
    Sub,
    Mul,
    Div,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    EOF,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Ident(String),
    Semicolon,
    Assign,
    Return,
    If,
    Else,
    While,
    For,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: usize,
}

#[derive(Debug)]
pub struct TokenizeError {
    pub pos: usize,
    pub message: String,
}

#[derive(Debug)]
pub struct Tokenizer {
    pub tokens: Vec<Token>,
    pub errors: Vec<TokenizeError>,
    input: Vec<char>,
    pos: usize,
    ch: char,
}

impl Tokenizer {
    pub fn new(input: &str) -> Self {
        Tokenizer {
            tokens: Vec::new(),
            input: input.chars().collect(),
            pos: 0,
            ch: input.chars().next().unwrap_or('\0'),
            errors: Vec::new(),
        }
    }

    fn next(&mut self) {
        self.pos += 1;
        self.ch = match self.input.get(self.pos) {
            Some(ch) => *ch, 
            None => '\0',
        };
    }

    fn add_error(&mut self, msg: &str) {
        self.errors.push(TokenizeError {
            pos: self.pos,
            message: String::from(msg),
        });
    }

    fn add_token(&mut self, kind: TokenKind) {
        self.tokens.push(Token {
            kind,
            pos: self.pos,
        });
    }

    fn add_token_and_skip(&mut self, kind: TokenKind, skip: usize) {
        self.add_token(kind);
        for _ in 0..skip {
            self.next();
        }
    }

    fn next_is(&self, ch: char) -> bool {
        match self.input.get(self.pos + 1) {
            Some(next) => ch == *next,
            None => false,
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.ch {
                ' ' | '\t' | '\r' | '\n' => {},
                _ => break,
            }

            self.next();
        }
    }

    fn tokenize_number(&mut self) {
        let mut num: i32 = 0;
        while self.ch.is_ascii_digit() {
            num = (10 * num) + self.ch.to_digit(10).unwrap() as i32;
            self.next();
        }

        self.add_token(TokenKind::Number(num));
    }

    fn tokenize_ident(&mut self) {
        let start_pos = self.pos;

        loop {
            match self.ch {
                c if c.is_ascii_alphanumeric() || c == '_' => self.next(),
                _ => break,
            }
        }

        let s: String = self.input[start_pos..self.pos].iter().collect();
        match &*s {
            "return" => self.add_token(TokenKind::Return),
            "if" => self.add_token(TokenKind::If),
            "else" => self.add_token(TokenKind::Else),
            "while" => self.add_token(TokenKind::While),
            "for" => self.add_token(TokenKind::For),
            _ => self.add_token(TokenKind::Ident(s)),
        }
    }

    pub fn tokenize(&mut self) {
        loop {
            self.skip_whitespace();

            match self.ch {
                c if c.is_ascii_digit() => self.tokenize_number(),
                c if c.is_ascii_alphanumeric() || c == '_' => self.tokenize_ident(),
                '+' => self.add_token_and_skip(TokenKind::Add, 1),
                '-' => self.add_token_and_skip(TokenKind::Sub, 1),
                '*' => self.add_token_and_skip(TokenKind::Mul, 1),
                '/' => self.add_token_and_skip(TokenKind::Div, 1),
                '(' => self.add_token_and_skip(TokenKind::Lparen, 1),
                ')' => self.add_token_and_skip(TokenKind::Rparen, 1),
                '{' => self.add_token_and_skip(TokenKind::Lbrace, 1),
                '}' => self.add_token_and_skip(TokenKind::Rbrace, 1),
                '=' if self.next_is('=') => self.add_token_and_skip(TokenKind::Equal, 2),
                '=' => self.add_token_and_skip(TokenKind::Assign, 1),
                '!' if self.next_is('=') => self.add_token_and_skip(TokenKind::NotEqual, 2),
                '<' if self.next_is('=') => self.add_token_and_skip(TokenKind::LessThanOrEqual, 2),
                '<' => self.add_token_and_skip(TokenKind::LessThan, 1),
                '>' if self.next_is('=') => self.add_token_and_skip(TokenKind::GreaterThanOrEqual, 2),
                '>' => self.add_token_and_skip(TokenKind::GreaterThan, 1),
                ';' => self.add_token_and_skip(TokenKind::Semicolon, 1),
                '\0' => break,
                _ => { self.add_error("Unexpected token"); self.next() },
            }
        }

        self.add_token(TokenKind::EOF);
    }
}
