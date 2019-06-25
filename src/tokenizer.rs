#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Number(i32),
    Add,
    Sub,
    Asterisk,
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
    Comma,
    Int,
    Ampersand,
    SizeOf,
    Lbracket,
    Rbracket,
    Char,
    String(String),
}

impl ToString for TokenKind {
    fn to_string(&self) -> String {
        String::from(match self {
            TokenKind::Number(_) => "number",
            TokenKind::Add => "+",
            TokenKind::Sub => "-",
            TokenKind::Asterisk => "*",
            TokenKind::Div => "/",
            TokenKind::Lparen => "(",
            TokenKind::Rparen => ")",
            TokenKind::Lbrace => "{",
            TokenKind::Rbrace => "}",
            TokenKind::EOF => "EOF",
            TokenKind::Equal => "==",
            TokenKind::NotEqual => "!=",
            TokenKind::LessThan => "<",
            TokenKind::LessThanOrEqual => "<=",
            TokenKind::GreaterThan => ">",
            TokenKind::GreaterThanOrEqual => ">=",
            TokenKind::Ident(_) => "identifier",
            TokenKind::Semicolon => ";",
            TokenKind::Assign => "=",
            TokenKind::Return => "return",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::While => "while",
            TokenKind::For => "for",
            TokenKind::Comma => ",",
            TokenKind::Int => "int",
            TokenKind::Ampersand => "&",
            TokenKind::SizeOf => "sizeof",
            TokenKind::Lbracket => "[",
            TokenKind::Rbracket => "]",
            TokenKind::Char => "char",
            TokenKind::String(_) => "string",
        })
    }
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

#[derive(Debug)]
pub struct TokenizeError {
    pub line: usize,
    pub col: usize,
    pub message: String,
}

#[derive(Debug)]
pub struct Tokenizer {
    pub tokens: Vec<Token>,
    pub errors: Vec<TokenizeError>,
    input: Vec<char>,
    pos: usize,
    ch: char,
    line: usize,
    col: usize,
}

impl Tokenizer {
    pub fn new(input: &str) -> Self {
        Tokenizer {
            tokens: Vec::new(),
            input: input.chars().collect(),
            pos: 0,
            ch: input.chars().next().unwrap_or('\0'),
            errors: Vec::new(),
            line: 0,
            col: 0,
        }
    }

    fn next(&mut self) {
        self.pos += 1;
        self.col += 1;
        self.ch = match self.input.get(self.pos) {
            Some(ch) => *ch, 
            None => '\0',
        };
    }

    fn add_error(&mut self, msg: &str) {
        self.errors.push(TokenizeError {
            line: self.line,
            col: self.col,
            message: String::from(msg),
        });
    }

    fn add_token(&mut self, kind: TokenKind, start_col: usize, end_col: usize) {
        self.tokens.push(Token {
            kind,
            start_line: self.line,
            end_line: self.line,
            start_col,
            end_col,
        });
    }

    fn add_token_and_skip(&mut self, kind: TokenKind, skip: usize) {
        self.add_token(kind, self.col, self.col + skip);
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
                ' ' | '\t' | '\r' => self.next(),
                '\n' => {
                    self.next();
                    self.line += 1;
                    self.col = 0;
                },
                _ => break,
            }
        }
    }

    fn tokenize_number(&mut self) {
        let start_col = self.col;
        let mut num: i32 = 0;
        while self.ch.is_ascii_digit() {
            num = (10 * num) + self.ch.to_digit(10).unwrap() as i32;
            self.next();
        }

        self.add_token(TokenKind::Number(num), start_col, self.col);
    }

    fn tokenize_ident(&mut self) {
        let start_pos = self.pos;
        let start_col = self.col;

        loop {
            match self.ch {
                c if c.is_ascii_alphanumeric() || c == '_' => self.next(),
                _ => break,
            }
        }

        let s: String = self.input[start_pos..self.pos].iter().collect();
        self.add_token(match &*s {
            "return" => TokenKind::Return,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "int" => TokenKind::Int,
            "sizeof" => TokenKind::SizeOf,
            "char" => TokenKind::Char,
            _ => TokenKind::Ident(s),
        }, start_col, self.col);
    }

    pub fn tokenize_string(&mut self) {
        // " を消費
        self.next();

        let start_pos = self.pos;
        let start_col = self.col;
        loop {
            match self.ch {
                '"' => {
                    self.next();
                    break;
                },
                '\0' | '\n' => {
                    self.add_error("対応する \" がありません");
                    return;
                },
                _ => self.next(),
            };
        }

        self.add_token(TokenKind::String(self.input[start_pos..self.pos - 1].iter().collect()), start_col, self.col);
    }

    pub fn skip_single_line_comment(&mut self) {
        // 改行が来るまでスキップ
        loop {
            match self.ch {
                '\n' => break,
                _ => self.next(),
            };
        }
    }

    pub fn skip_block_comment(&mut self) {
        // "*/" が来るまでスキップ
        loop {
            match self.ch {
                '*' if self.next_is('/') => {
                    self.next();
                    self.next();
                    break;
                },
                '\n' => {
                    self.next();
                    self.line += 1;
                    self.col = 0;
                },
                _ => self.next(),
            };
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
                '*' => self.add_token_and_skip(TokenKind::Asterisk, 1),
                '/' if self.next_is('/') => self.skip_single_line_comment(),
                '/' if self.next_is('*') => self.skip_block_comment(),
                '/' => self.add_token_and_skip(TokenKind::Div, 1),
                '(' => self.add_token_and_skip(TokenKind::Lparen, 1),
                ')' => self.add_token_and_skip(TokenKind::Rparen, 1),
                '{' => self.add_token_and_skip(TokenKind::Lbrace, 1),
                '}' => self.add_token_and_skip(TokenKind::Rbrace, 1),
                '[' => self.add_token_and_skip(TokenKind::Lbracket, 1),
                ']' => self.add_token_and_skip(TokenKind::Rbracket, 1),
                '=' if self.next_is('=') => self.add_token_and_skip(TokenKind::Equal, 2),
                '=' => self.add_token_and_skip(TokenKind::Assign, 1),
                '!' if self.next_is('=') => self.add_token_and_skip(TokenKind::NotEqual, 2),
                '<' if self.next_is('=') => self.add_token_and_skip(TokenKind::LessThanOrEqual, 2),
                '<' => self.add_token_and_skip(TokenKind::LessThan, 1),
                '>' if self.next_is('=') => self.add_token_and_skip(TokenKind::GreaterThanOrEqual, 2),
                '>' => self.add_token_and_skip(TokenKind::GreaterThan, 1),
                ';' => self.add_token_and_skip(TokenKind::Semicolon, 1),
                ',' => self.add_token_and_skip(TokenKind::Comma, 1),
                '&' => self.add_token_and_skip(TokenKind::Ampersand, 1),
                '"' => self.tokenize_string(),
                '\0' => break,
                _ => { self.add_error("Unexpected token"); self.next() },
            }
        }

        self.add_token(TokenKind::EOF, self.col, self.col);
    }
}
