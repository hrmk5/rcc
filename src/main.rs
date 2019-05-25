mod tokenizer;

use std::env;
use std::process;
use tokenizer::Tokenizer;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("引数の個数が正しくありません");
        process::exit(1);
    }

    let mut tokenizer = Tokenizer::new(&args[1]);
    tokenizer.tokenize();

    println!("{:?}", tokenizer.tokens);
}
