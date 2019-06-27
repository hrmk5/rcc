#![feature(box_patterns)]

mod tokenizer;
mod parser;
mod gen;
mod sema;
mod ast;

use std::env;
use std::process;
use std::fs::File;
use std::io::Read;
use tokenizer::Tokenizer;
use parser::Parser;
use gen::Generator;
use colored::*;

enum ShowType {
    Token,
    Program,
    Code,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("引数の個数が正しくありません");
        process::exit(1);
    }

    let filepath = &args[1];
    let mut file = File::open(filepath).expect("Unable to open file");
    let mut input = String::new();
    file.read_to_string(&mut input).expect("Unable to read file");

    let lines: Vec<&str> = input.split('\n').collect();

    let show_type = match args.get(2) {
        Some(s) => match &s[..] {
            "token" => ShowType::Token,
            "program" => ShowType::Program,
            _ => ShowType::Code,
        },
        _ => ShowType::Code,
    };

    let mut tokenizer = Tokenizer::new(&input);
    tokenizer.tokenize();
    if tokenizer.errors.len() > 0 {
        for error in tokenizer.errors {
            let line = (error.line + 1).to_string();
            println!("{} {}", format!("{} |", line).bright_cyan(), lines[error.line]);
            println!("{}^ {}", " ".repeat(error.col + line.chars().count() + 3).bright_red(), error.message.bright_red());
        }
        process::exit(1);
    }

    if let ShowType::Token = show_type {
        for token in &tokenizer.tokens {
            println!("{:?}", token);
        }
    }

    let mut parser = Parser::new(tokenizer.tokens);
    let mut program = parser.parse();
    if parser.errors.len() > 0 {
        for error in parser.errors {
            let line = (error.start_line + 1).to_string();
            println!("{} {}", format!("{} |", line).bright_cyan(), lines[error.start_line]);
            println!("{}{} {}", " ".repeat(error.start_col + line.chars().count() + 3).bright_red(), "^".repeat(error.end_col - error.start_col).bright_red(), error.message.bright_red());
        }
        process::exit(1);
    }

    if let ShowType::Program = show_type {
        println!("{:?}", program);
    }

    sema::walk(&mut program);

    let mut generator = Generator::new();
    generator.gen(program);

    if let ShowType::Code = show_type {
        println!("{}", generator.code);
    }
}
