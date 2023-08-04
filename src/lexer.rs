use std::str::CharIndices;

use itertools::Itertools;
use itertools::MultiPeek;
use phf::phf_map;

#[derive(Debug)]
pub struct Token<'a> {
    line: usize,
    kind: Kind<'a>,
}

#[derive(Debug, Clone)]
enum Kind<'a> {
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
        loop {
            match self.next_char() {
                Some((_, '"')) => break,
                None | Some((_, '\n')) => {
                    return self.emit_token(Kind::Error {
                        error: "unclosed string literal".to_owned(),
                    })
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
                    return self.emit_token(Kind::Error {
                        error: "invalid float literal".to_owned(),
                    });
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
                '(' => Some(self.emit_token(Kind::Lparen)),
                ')' => Some(self.emit_token(Kind::Rparen)),
                '{' => Some(self.emit_token(Kind::Lbrace)),
                '}' => Some(self.emit_token(Kind::Rbrace)),
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
