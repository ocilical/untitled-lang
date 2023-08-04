mod lexer;

use std::io::{self, Write};

use crate::lexer::lex;
use crate::lexer::Token;

fn main() {
    let stdin = io::stdin();
    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut source = String::new();
        stdin.read_line(&mut source).unwrap();

        println!("{:?}", lex(&source).collect::<Vec<Token>>());
    }
}
