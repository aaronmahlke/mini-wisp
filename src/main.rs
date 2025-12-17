mod ast;
mod error;
mod lexer;
mod span;
use std::{env, fs};

use crate::ast::Parser;
use crate::lexer::Cursor;

fn main() {
    let args: Vec<String> = env::args().collect();
    let src = &args[1];

    println!("Source: {}", src);
    let source = fs::read_to_string(src).expect("file not found");
    println!("Content: {}", source);

    let tokens = Cursor::new(&source).tokenize();
    // for token in &tokens {
    //     println!("{:?}", token)
    // }

    let mut parser = Parser::new(tokens);

    match parser.build() {
        Ok(ast) => {
            println!("{:#?}", ast);
        }
        Err(error) => {
            eprintln!("{}", error.format(&source))
        }
    };
}
