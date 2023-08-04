use std::str::CharIndices;

use itertools::Itertools;
use itertools::MultiPeek;
use phf::phf_map;

#[derive(Debug, PartialEq, Eq)]
pub struct Token<'a> {
    line: usize,
    kind: Kind<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Kind<'a> {
    // non-keyword tokens
    LParen,      // (
    RParen,      // )
    LBrace,      // {
    RBrace,      // }
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

    Error { error: String },
}

static KEYWORDS: phf::Map<&'static str, Kind> = phf_map! {
    "else" => Kind::Else,
    "false" => Kind::False,
    "fn" => Kind::Fn,
    "let" => Kind::Let,
    "return" => Kind::Return,
    "true" => Kind::True,
    "unless" => Kind::Unless,
    "until" => Kind::Until,
};

pub struct Lexer<'a> {
    text: &'a str,
    chars: MultiPeek<CharIndices<'a>>,
    line: usize,
}

pub fn lex(source: &str) -> Lexer<'_> {
    Lexer {
        text: source,
        chars: source.char_indices().multipeek(),
        line: 1,
    }
}

impl<'a> Lexer<'a> {
    fn next_char(&mut self) -> Option<(usize, char)> {
        let next = self.chars.next();

        if let Some((_, '\n')) = next {
            self.line += 1;
        }

        next
    }

    fn skip_char(&mut self) {
        self.next_char();
    }

    fn peek(&mut self) -> Option<(usize, char)> {
        let res = self.chars.peek().cloned();
        self.chars.reset_peek();
        res
    }

    fn peek_next(&mut self) -> Option<(usize, char)> {
        self.chars.peek();
        let res = self.chars.peek().cloned();
        self.chars.reset_peek();
        res
    }

    fn skip_line(&mut self) {
        loop {
            match self.next_char() {
                None => break,
                Some((_, '\n')) => break,
                _ => continue,
            }
        }
    }

    fn multi_comment(&mut self) {
        loop {
            match self.next_char() {
                None => break,
                Some((_, '*')) => match self.peek() {
                    None => break,
                    Some((_, '/')) => {
                        self.skip_char();
                        break;
                    }
                    _ => continue,
                },
                _ => continue,
            }
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some((_, c)) = self.peek() {
            match c {
                ' ' | '\r' | '\t' | '\n' => self.skip_char(),
                '/' => match self.peek_next() {
                    Some((_, '/')) => self.skip_line(),
                    Some((_, '*')) => {
                        self.skip_char();
                        self.skip_char();
                        self.multi_comment();
                    }
                    _ => break,
                },
                _ => break,
            }
        }
    }

    fn emit_token(&self, kind: Kind<'a>) -> Token<'a> {
        Token {
            line: self.line,
            kind,
        }
    }

    fn emit_eq(&mut self, no_eq: Kind<'a>, eq: Kind<'a>) -> Token<'a> {
        if let Some((_, '=')) = self.peek() {
            self.skip_char();
            self.emit_token(eq)
        } else {
            self.emit_token(no_eq)
        }
    }

    fn emit_string(&mut self, start: usize) -> Token<'a> {
        // saving this for hacky error reporting
        let l = self.line;
        loop {
            match self.next_char() {
                Some((_, '"')) => break,
                None | Some((_, '\n')) => {
                    return Token {
                        line: l,
                        kind: Kind::Error {
                            error: "unclosed string literal".to_owned(),
                        },
                    };
                }
                _ => continue,
            }
        }

        let (end, _) = self.peek().unwrap_or((self.text.len(), 'm'));

        self.emit_token(Kind::String {
            text: &self.text[start..end],
        })
    }

    fn emit_ident(&mut self, start: usize) -> Token<'a> {
        // : is definitely not a character that can be in an identifier
        while is_ident_cont(self.peek().unwrap_or((0, ':')).1) {
            self.skip_char()
        }

        let (end, _) = self.peek().unwrap_or((self.text.len(), 'm'));

        self.emit_token(
            KEYWORDS
                .get(&self.text[start..end])
                .cloned()
                .unwrap_or(Kind::Ident {
                    text: &self.text[start..end],
                }),
        )
    }

    fn emit_number(&mut self, start: usize) -> Token<'a> {
        // saving this for hacky error reporting
        let l = self.line;
        loop {
            match self.peek() {
                Some((_, '0'..='9')) => self.skip_char(),
                Some((_, 'e' | 'E' | '.')) => break,
                _ => {
                    let (end, _) = self.peek().unwrap_or((self.text.len(), 'm'));
                    return self.emit_token(Kind::Int {
                        text: &self.text[start..end],
                    });
                }
            }
        }

        let mut seen_dot = false;
        let mut seen_e = false;
        loop {
            match self.peek() {
                Some((_, '0'..='9')) => self.skip_char(),
                Some((_, '.')) if !seen_dot && !seen_e => {
                    seen_dot = true;
                    self.skip_char();
                }
                Some((_, 'e' | 'E')) if !seen_e => {
                    seen_dot = true;
                    seen_e = true;
                    self.skip_char();
                }
                Some((_, '.' | 'e' | 'E')) => {
                    while matches!(self.peek(), Some((_, '.' | 'e' | 'E' | '0'..='9'))) {
                        self.skip_char()
                    }
                    return Token {
                        line: l,
                        kind: Kind::Error {
                            error: "invalid float literal".to_owned(),
                        },
                    };
                }
                _ => break,
            }
        }

        let (end, _) = self.peek().unwrap_or((self.text.len(), 'm'));
        self.emit_token(Kind::Float {
            text: &self.text[start..end],
        })
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        if let Some((i, c)) = self.next_char() {
            match c {
                // single character tokens
                '(' => Some(self.emit_token(Kind::LParen)),
                ')' => Some(self.emit_token(Kind::RParen)),
                '{' => Some(self.emit_token(Kind::LBrace)),
                '}' => Some(self.emit_token(Kind::RBrace)),
                ',' => Some(self.emit_token(Kind::Comma)),
                '.' => Some(self.emit_token(Kind::Dot)),
                ';' => Some(self.emit_token(Kind::Semicolon)),
                '~' => Some(self.emit_token(Kind::BitNot)),

                // tokens that might have an equals sign
                '+' => Some(self.emit_eq(Kind::Plus, Kind::PlusAssign)),
                '-' => Some(self.emit_eq(Kind::Minus, Kind::MinusAssign)),
                '*' => Some(self.emit_eq(Kind::Mul, Kind::MulAssign)),
                '%' => Some(self.emit_eq(Kind::Mod, Kind::ModAssign)),
                '^' => Some(self.emit_eq(Kind::BitXor, Kind::XorAssign)),
                '/' => Some(self.emit_eq(Kind::Div, Kind::DivAssign)),
                '=' => Some(self.emit_eq(Kind::Assign, Kind::Eq)),

                // multicharacter tokens
                '!' => match self.peek() {
                    Some((_, '=')) => {
                        self.skip_char();
                        Some(self.emit_token(Kind::Neq))
                    }
                    Some((_, '&')) => {
                        self.skip_char();
                        Some(self.emit_token(Kind::Nand))
                    }
                    _ => Some(self.emit_token(Kind::Not)),
                },

                '&' => match self.peek() {
                    Some((_, '=')) => {
                        self.skip_char();
                        Some(self.emit_token(Kind::AndAssign))
                    }
                    Some((_, '&')) => {
                        self.skip_char();
                        Some(self.emit_token(Kind::And))
                    }
                    _ => Some(self.emit_token(Kind::BitAnd)),
                },

                '|' => match self.peek() {
                    Some((_, '=')) => {
                        self.skip_char();
                        Some(self.emit_token(Kind::OrAssign))
                    }
                    Some((_, '|')) => {
                        self.skip_char();
                        Some(self.emit_token(Kind::Or))
                    }
                    _ => Some(self.emit_token(Kind::BitOr)),
                },

                '>' => match self.peek() {
                    Some((_, '=')) => {
                        self.skip_char();
                        Some(self.emit_token(Kind::Geq))
                    }
                    Some((_, '>')) => match self.peek_next() {
                        Some((_, '=')) => {
                            self.skip_char();
                            self.skip_char();
                            Some(self.emit_token(Kind::ShrAssign))
                        }
                        _ => {
                            self.skip_char();
                            Some(self.emit_token(Kind::Shr))
                        }
                    },
                    _ => Some(self.emit_token(Kind::Gt)),
                },

                '<' => match self.peek() {
                    Some((_, '=')) => {
                        self.skip_char();
                        Some(self.emit_token(Kind::Leq))
                    }
                    Some((_, '<')) => match self.peek_next() {
                        Some((_, '=')) => {
                            self.skip_char();
                            self.skip_char();
                            Some(self.emit_token(Kind::ShlAssign))
                        }
                        _ => {
                            self.skip_char();
                            Some(self.emit_token(Kind::Shl))
                        }
                    },
                    _ => Some(self.emit_token(Kind::Lt)),
                },

                // the weird ones
                '"' => Some(self.emit_string(i)),

                '0'..='9' => Some(self.emit_number(i)),

                c if is_ident_start(c) => Some(self.emit_ident(i)),

                _ => Some(self.emit_token(Kind::Error {
                    error: format!("unrecognized character: {}", c),
                })),
            }
        } else {
            None
        }
    }
}

fn is_ident_start(c: char) -> bool {
    matches!(c, '_' | 'a'..='z' | 'A'..='Z')
}

fn is_ident_cont(c: char) -> bool {
    matches!(c, '_' | 'a'..='z' | 'A'..='Z'| '0'..='9')
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    fn t<'a>(line: usize, kind: Kind<'a>) -> Token<'a> {
        Token { line, kind }
    }

    #[rstest]
    #[case("(", vec![t(1, Kind::LParen)])]
    #[case(")", vec![t(1, Kind::RParen)])]
    #[case("{", vec![t(1, Kind::LBrace)])]
    #[case("}", vec![t(1, Kind::RBrace)])]
    #[case(",", vec![t(1, Kind::Comma)])]
    #[case(".", vec![t(1, Kind::Dot)])]
    #[case("+", vec![t(1, Kind::Plus)])]
    #[case("-", vec![t(1, Kind::Minus)])]
    #[case("*", vec![t(1, Kind::Mul)])]
    #[case("/", vec![t(1, Kind::Div)])]
    #[case("%", vec![t(1, Kind::Mod)])]
    #[case(";", vec![t(1, Kind::Semicolon)])]
    #[case("!", vec![t(1, Kind::Not)])]
    #[case("!=", vec![t(1, Kind::Neq)])]
    #[case("==", vec![t(1, Kind::Eq)])]
    #[case("=", vec![t(1, Kind::Assign)])]
    #[case(">", vec![t(1, Kind::Gt)])]
    #[case(">=", vec![t(1, Kind::Geq)])]
    #[case("<", vec![t(1, Kind::Lt)])]
    #[case("<=", vec![t(1, Kind::Leq)])]
    #[case("<<", vec![t(1, Kind::Shl)])]
    #[case(">>", vec![t(1, Kind::Shr)])]
    #[case("&", vec![t(1, Kind::BitAnd)])]
    #[case("&&", vec![t(1, Kind::And)])]
    #[case("|", vec![t(1, Kind::BitOr)])]
    #[case("||", vec![t(1, Kind::Or)])]
    #[case("~", vec![t(1, Kind::BitNot)])]
    #[case("^", vec![t(1, Kind::BitXor)])]
    #[case("!&", vec![t(1, Kind::Nand)])]
    #[case("+=", vec![t(1, Kind::PlusAssign)])]
    #[case("-=", vec![t(1, Kind::MinusAssign)])]
    #[case("*=", vec![t(1, Kind::MulAssign)])]
    #[case("/=", vec![t(1, Kind::DivAssign)])]
    #[case("%=", vec![t(1, Kind::ModAssign)])]
    #[case("&=", vec![t(1, Kind::AndAssign)])]
    #[case("|=", vec![t(1, Kind::OrAssign)])]
    #[case("^=", vec![t(1, Kind::XorAssign)])]
    #[case("<<=", vec![t(1, Kind::ShlAssign)])]
    #[case(">>=", vec![t(1, Kind::ShrAssign)])]
    #[case("else", vec![t(1, Kind::Else)])]
    #[case("false", vec![t(1, Kind::False)])]
    #[case("fn", vec![t(1, Kind::Fn)])]
    #[case("let", vec![t(1, Kind::Let)])]
    #[case("return", vec![t(1, Kind::Return)])]
    #[case("true", vec![t(1, Kind::True)])]
    #[case("unless", vec![t(1, Kind::Unless)])]
    #[case("until", vec![t(1, Kind::Until)])]
    #[case("hiiiiii", vec![t(1, Kind::Ident { text: "hiiiiii" })])]
    #[case("hiiiiii3", vec![t(1, Kind::Ident { text: "hiiiiii3" })])]
    #[case("_hiiiiii3", vec![t(1, Kind::Ident { text: "_hiiiiii3" })])]
    #[case("HIIIIII3", vec![t(1, Kind::Ident { text: "HIIIIII3" })])]
    #[case("0", vec![t(1, Kind::Int { text: "0" })])]
    #[case("1234567890", vec![t(1, Kind::Int { text: "1234567890" })])]
    #[case("1234567890", vec![t(1, Kind::Int { text: "1234567890" })])]
    #[case("1.", vec![t(1, Kind::Float { text: "1." })])]
    #[case("1.0", vec![t(1, Kind::Float { text: "1.0" })])]
    #[case("1.0e5", vec![t(1, Kind::Float { text: "1.0e5" })])]
    #[case("1.0E5", vec![t(1, Kind::Float { text: "1.0E5" })])]
    #[case("1E5", vec![t(1, Kind::Float { text: "1E5" })])]
    #[case("1e3.05", vec![t(1, Kind::Error { error: "invalid float literal".to_owned() })])]
    #[case("13..05", vec![t(1, Kind::Error { error: "invalid float literal".to_owned() })])]
    #[case("13ee05", vec![t(1, Kind::Error { error: "invalid float literal".to_owned() })])]
    #[case("1..3ee05", vec![t(1, Kind::Error { error: "invalid float literal".to_owned() })])]
    #[case("\"hiiiiii\"", vec![t(1, Kind::String { text: "\"hiiiiii\"" })])]
    #[case("\"hiiiiii", vec![t(1, Kind::Error { error: "unclosed string literal".to_owned() })])]
    #[case("\"hiiiiii\n", vec![t(1, Kind::Error { error: "unclosed string literal".to_owned() })])]
    #[case("ニャー", vec![t(1, Kind::Error { error: "unrecognized character: ニ".to_owned() }), t(1, Kind::Error { error: "unrecognized character: ャ".to_owned() }), t(1, Kind::Error { error: "unrecognized character: ー`".to_owned() })])]
    fn lex_test(#[case] text: &str, #[case] expected: Vec<Token>) {
        let res: Vec<Token> = lex(text).collect();
        assert_eq!(res, expected)
    }
}
