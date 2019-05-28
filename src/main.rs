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

    let input = &args[1];

    let mut tokenizer = Tokenizer::new(input);
    tokenizer.tokenize();
    if tokenizer.errors.len() > 0 {
        for error in tokenizer.errors {
            println!("{}", input);
            println!("{}^ {}", " ".repeat(error.pos), error.message);
        }
        process::exit(1);
    }

    //println!("{:?}", tokenizer.tokens);

    let mut parser = Parser::new(tokenizer.tokens);
    let expr = parser.parse();
    if parser.errors.len() > 0 {
        for error in parser.errors {
            println!("{}", input);
            println!("{}^ {}", " ".repeat(error.pos), error.message);
        }
        process::exit(1);
    }

    //println!("{:?}", expr);

    let mut asm = String::new();
    gen(&expr, &mut asm);

    println!("{}", asm);
}