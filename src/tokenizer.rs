#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Number(i32),
    Add,
    Sub,
    Mul,
    Div,
    Lparen,
    Rparen,
    EOF,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: usize,
}

#[derive(Debug)]
pub struct Tokenizer {
    pub tokens: Vec<Token>,
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
        }
    }

    fn next(&mut self) {
        self.pos += 1;
        self.ch = match self.input.get(self.pos) {
            Some(ch) => *ch, 
            None => '\0',
        };
    }

    fn error(&self, msg: &str) {
        println!("{}", self.input.iter().collect::<String>());
        println!("{}^ {}", std::iter::repeat(" ").take(self.pos).collect::<String>(), msg);
    }

    fn add_token(&mut self, kind: TokenKind) {
        self.tokens.push(Token {
            kind,
            pos: self.pos,
        });
    }

    fn add_token_and_next(&mut self, kind: TokenKind) {
        self.add_token(kind);
        self.next();
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.ch {
                ' ' | '\t' | '\r' => {},
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

    pub fn tokenize(&mut self) {
        loop {
            self.skip_whitespace();

            match self.ch {
                c if c.is_ascii_digit() => self.tokenize_number(),
                '+' => self.add_token_and_next(TokenKind::Add),
                '-' => self.add_token_and_next(TokenKind::Sub),
                '*' => self.add_token_and_next(TokenKind::Mul),
                '/' => self.add_token_and_next(TokenKind::Div),
                '(' => self.add_token_and_next(TokenKind::Lparen),
                ')' => self.add_token_and_next(TokenKind::Rparen),
                '\0' => break,
                _ => { self.error("Unexpected token"); self.next() },
            }
        }

        self.add_token(TokenKind::EOF);
    }
}
