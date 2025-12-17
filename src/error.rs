use crate::lexer::{Token, TokenKind};
use crate::span::Span;

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    UnexpectedToken { expected: TokenKind, found: Token },
    UnexpectedEof,
    ExpectedIdent(Token),
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind, span: Span) -> Self {
        ParseError { kind, span }
    }

    pub fn unexpected_token(expected: TokenKind, found: Token) -> Self {
        ParseError {
            kind: ParseErrorKind::UnexpectedToken {
                expected,
                found: found.clone(),
            },
            span: found.span,
        }
    }

    pub fn format(&self, source: &str) -> String {
        let Span { start, .. } = self.span;

        let adjusted_start = if matches!(self.kind, ParseErrorKind::UnexpectedToken { .. }) {
            let mut pos = start;
            while pos > 0 {
                pos -= 1;
                let ch = source.as_bytes()[pos];
                if !ch.is_ascii_whitespace() {
                    break;
                }
            }
            pos + 1
        } else if start > 0 && source.as_bytes().get(start) == Some(&b'\n') {
            start - 1
        } else {
            start
        };

        let before = &source[..adjusted_start];
        let line_num = before.matches('\n').count() + 1;
        let col = before
            .rfind('\n')
            .map(|pos| adjusted_start - pos - 1)
            .unwrap_or(adjusted_start);

        let line_start = before.rfind('\n').map(|p| p + 1).unwrap_or(0);
        let line_end = source[adjusted_start..]
            .find('\n')
            .map(|p| adjusted_start + p)
            .unwrap_or(source.len());
        let line = &source[line_start..line_end];

        let actual_col = if col >= line.len() { line.len() } else { col };

        let error_msg = match &self.kind {
            ParseErrorKind::ExpectedIdent(found) => {
                format!("expected identifier, found {}", found.kind)
            }
            ParseErrorKind::UnexpectedToken { expected, found } => {
                format!("expected {}, found {}", expected, found.kind)
            }
            ParseErrorKind::UnexpectedEof => "unexpected end of file".to_string(),
        };

        format!(
            "  |\n{} | {}\n  | {}\x1b[31m^ {}\x1b[0m\n",
            line_num,
            line,
            " ".repeat(actual_col),
            &error_msg
        )
    }
}
