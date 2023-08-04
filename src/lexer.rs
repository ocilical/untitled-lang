use phf::phf_map;
use std::collections::HashMap;

#[derive(Debug)]
struct Token<'a> {
    line: usize,
    kind: TokenKind<'a>,
}

#[derive(Debug)]
enum TokenKind<'a> {
    // non-keyword tokens
    Lparen,      // (
    Rparen,      // )
    Lbrace,      // {
    Rbrace,      // }
    Comma,       // ,
    Dot,         // .
    Plus,        // +
    Minus,       // -
    Mul,         // *
    Div,         // /
    Mod,         // %
    Semicolon,   // ;
    Not,         // !
    Neq,         // !=
    Eq,          // ==
    Assign,      // =
    Gt,          // >
    Geq,         // >=
    Lt,          // <
    Leq,         // <=
    Shl,         // <<
    Shr,         // >>
    BitAnd,      // &
    And,         // &&
    BitOr,       // |
    Or,          // ||
    BitNot,      // ~
    BitXor,      // ^
    Nand,        // !&
    PlusAssign,  // +=
    MinusAssign, // -=
    MulAssign,   // *=
    DivAssign,   // /=
    ModAssign,   // %=
    AndAssign,   // &=
    OrAssign,    // |=
    XorAssign,   // ^=
    ShlAssign,   // <<=
    ShrAssign,   // >>=

    // keyword tokens
    Else,   // else
    False,  // false
    Fn,     // fn
    Let,    // let
    Return, // return
    True,   // true
    Unless, // unless
    Until,  // until

    Ident { text: &'a str },

    Int { text: &'a str },

    Float { text: &'a str },

    String { text: &'a str },
}

static KEYWORDS: phf::Map<&'static str, TokenKind> = phf_map! {
    "else" => TokenKind::Else,
    "false" => TokenKind::False,
    "fn" => TokenKind::Fn,
    "let" => TokenKind::Let,
    "return" => TokenKind::Return,
    "true" => TokenKind::True,
    "unless" => TokenKind::Unless,
    "until" => TokenKind::Until,
};
