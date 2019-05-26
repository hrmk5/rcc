mod tokenizer;
mod parser;
mod gen;

use std::env;
use std::process;
use tokenizer::Tokenizer;
use parser::Parser;
use gen::gen;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("引数の個数が正しくありません");
        process::exit(1);
    }

    let mut tokenizer = Tokenizer::new(&args[1]);
    tokenizer.tokenize();

    println!("{:?}", tokenizer.tokens);

    let mut parser = Parser::new(tokenizer.tokens);
    let expr = parser.parse();

    println!("{:?}", expr);
}
