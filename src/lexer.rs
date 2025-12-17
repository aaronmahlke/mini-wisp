use crate::span::Span;
use std::fmt;
use std::str::Chars;

const SPACE_CHAR: char = ' ';

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Ident { sym: String },
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Literal { kind: LiteralKind },
    Eq,
    Semi,
    Colon,
    Minus,
    Plus,
    Comma,
    Gt,
    Unknown,
    Eof,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Ident { sym } => write!(f, "identifier '{}'", sym),
            TokenKind::OpenParen => write!(f, "'('"),
            TokenKind::CloseParen => write!(f, "')'"),
            TokenKind::OpenBrace => write!(f, "'{{'"),
            TokenKind::CloseBrace => write!(f, "'}}'"),
            TokenKind::Literal { kind } => match kind {
                LiteralKind::Int(val) => write!(f, "integer literal '{}'", val),
            },
            TokenKind::Eq => write!(f, "'='"),
            TokenKind::Semi => write!(f, "';'"),
            TokenKind::Colon => write!(f, "':'"),
            TokenKind::Minus => write!(f, "'-'"),
            TokenKind::Plus => write!(f, "'+'"),
            TokenKind::Comma => write!(f, "','"),
            TokenKind::Gt => write!(f, "'>'"),
            TokenKind::Unknown => write!(f, "unknown token"),
            TokenKind::Eof => write!(f, "end of file"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    Int(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

pub struct Cursor<'a> {
    chars: Chars<'a>,
    pos: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(src: &'a str) -> Self {
        Cursor {
            chars: src.chars(),
            pos: 0,
        }
    }

    fn advance(&mut self) -> (TokenKind, usize) {
        let start = self.pos;

        let Some(c) = self.peek() else {
            return (TokenKind::Eof, 0);
        };

        let toke_kind = match c {
            c if is_ident_start(c) => self.ident(),
            '0'..='9' => self.number(),
            '(' => self.bump_and_return(TokenKind::OpenParen),
            ')' => self.bump_and_return(TokenKind::CloseParen),
            '{' => self.bump_and_return(TokenKind::OpenBrace),
            '}' => self.bump_and_return(TokenKind::CloseBrace),
            ';' => self.bump_and_return(TokenKind::Semi),
            ':' => self.bump_and_return(TokenKind::Colon),
            '=' => self.bump_and_return(TokenKind::Eq),
            '-' => self.bump_and_return(TokenKind::Minus),
            '+' => self.bump_and_return(TokenKind::Plus),
            '>' => self.bump_and_return(TokenKind::Gt),
            ',' => self.bump_and_return(TokenKind::Comma),
            _ => self.bump_and_return(TokenKind::Unknown),
        };

        let token_length = self.pos - start;
        (toke_kind, token_length)
    }

    fn bump_and_return(&mut self, kind: TokenKind) -> TokenKind {
        self.bump();
        kind
    }

    pub fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.pos += c.len_utf8();
        Some(c)
    }

    pub fn ident(&mut self) -> TokenKind {
        TokenKind::Ident {
            sym: self.eat_while(is_ident_next),
        }
    }

    pub fn number(&mut self) -> TokenKind {
        TokenKind::Literal {
            kind: LiteralKind::Int(self.eat_while(|c| c.is_ascii_digit())),
        }
    }

    pub fn eat_while(&mut self, predicate: impl Fn(char) -> bool) -> String {
        let mut result = String::new();
        while let Some(c) = self.peek() {
            if !predicate(c) {
                break;
            };
            result.push(c);
            self.bump();
        }
        result
    }

    pub fn peek(&mut self) -> Option<char> {
        self.chars.clone().next()
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while !self.is_eof() {
            self.skip_whitespace();
            let start = self.pos;
            let (token_kind, len) = self.advance();

            tokens.push(Token {
                kind: token_kind,
                span: Span {
                    start,
                    end: start + len,
                },
            });
        }
        tokens
    }

    fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    pub fn skip_whitespace(&mut self) {
        while self.peek().is_some() {
            let Some(c) = self.peek() else {
                break;
            };
            if is_whitespace(c) {
                self.bump();
            } else {
                break;
            }
        }
    }
}

pub fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic()
}

pub fn is_ident_next(c: char) -> bool {
    c.is_ascii_alphanumeric()
}

pub fn is_whitespace(c: char) -> bool {
    c.is_whitespace()
}
