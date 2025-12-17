mod ast;
mod lexer;
use std::{env, fs};

use crate::ast::Parser;
use crate::lexer::Cursor;

fn main() {
    let args: Vec<String> = env::args().collect();
    let src = &args[1];

    println!("Source: {}", src);
    let file_content = fs::read_to_string(src).expect("file not found");
    println!("Content: {}", file_content);

    let tokens = Cursor::new(&file_content).tokenize();
    for token in &tokens {
        println!("{:?}", token)
    }
    let ast = Parser::new(tokens).build();
    println!("AST:");
    println!("{:#?}", ast);
}
