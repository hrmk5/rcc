#[derive(Debug)]
pub enum TokenKind {
    Number(i32),
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    pos: usize,
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

    fn tokenize_number(&mut self) {
        let mut result: i32 = 0;
        while self.ch.is_ascii_digit() {
            result = (10 * result) + self.ch.to_digit(10).unwrap() as i32;
            self.next();
        }

        self.add_token(TokenKind::Number(result));
    }

    pub fn tokenize(&mut self) {
        loop {
            match self.ch {
                c if c.is_ascii_digit() => self.tokenize_number(),
                '\0' => break,
                _ => { self.error("Unexpected token"); self.next() },
            }
        }
    }
}
