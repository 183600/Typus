use std::path::PathBuf;
use std::fs::{self, File};
use std::io::{self, Read};
use crate::ast::{Token, TokenKind, Position};
use crate::error::Error;

/// 词法分析器
pub struct Lexer {
    /// 源代码
    pub source: String,
    /// 当前位置
    pub current: usize,
    /// 当前行
    pub line: usize,
    /// 当前列
    pub column: usize,
    /// 开始位置
    pub start: usize,
    /// 标记列表
    pub tokens: Vec<Token>,
    /// 当前文件
    pub current_file: PathBuf,
}

impl Lexer {
    /// 创建新的词法分析器
    pub fn new(source: &str, current_file: PathBuf) -> Self {
        Lexer {
            source: source.to_string(),
            current: 0,
            line: 1,
            column: 1,
            start: 0,
            tokens: Vec::new(),
            current_file,
        }
    }
    
    /// 执行词法分析
    pub fn tokenize(&mut self) -> Result<Vec<Token>, Error> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token()?;
        }
        
        // 添加 EOF 标记
        self.tokens.push(Token {
            kind: TokenKind::EOF,
            lexeme: "".to_string(),
            position: Position {
                line: self.line,
                column: self.column,
            },
        });
        
        Ok(self.tokens.clone())
    }
    
    /// 扫描标记
    fn scan_token(&mut self) -> Result<(), Error> {
        let c = self.advance();
        
        match c {
            // 单字符标记
            '(' => self.add_token(TokenKind::LeftParen),
            ')' => self.add_token(TokenKind::RightParen),
            '{' => self.add_token(TokenKind::LeftBrace),
            '}' => self.add_token(TokenKind::RightBrace),
            '[' => self.add_token(TokenKind::LeftBracket),
            ']' => self.add_token(TokenKind::RightBracket),
            ',' => self.add_token(TokenKind::Comma),
            '.' => self.add_token(TokenKind::Dot),
            ':' => self.add_token(TokenKind::Colon),
            ';' => self.add_token(TokenKind::Semicolon),
            
            // 可能是双字符标记
            '+' => {
                if self.match_char('=') {
                    self.add_token(TokenKind::PlusEqual)
                } else {
                    self.add_token(TokenKind::Plus)
                }
            },
            '-' => {
                if self.match_char('=') {
                    self.add_token(TokenKind::MinusEqual)
                } else if self.match_char('>') {
                    self.add_token(TokenKind::Arrow)
                } else {
                    self.add_token(TokenKind::Minus)
                }
            },
            '*' => {
                if self.match_char('=') {
                    self.add_token(TokenKind::StarEqual)
                } else {
                    self.add_token(TokenKind::Star)
                }
            },
            '/' => {
                if self.match_char('=') {
                    self.add_token(TokenKind::SlashEqual)
                } else if self.match_char('/') {
                    // 单行注释
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else if self.match_char('*') {
                    // 多行注释
                    while !(self.peek() == '*' && self.peek_next() == '/') && !self.is_at_end() {
                        if self.peek() == '\n' {
                            self.line += 1;
                            self.column = 0;
                        }
                        self.advance();
                    }
                    
                    if self.is_at_end() {
                        return Err(self.error("Unterminated comment"));
                    }
                    
                    // 消费 */
                    self.advance();
                    self.advance();
                } else {
                    self.add_token(TokenKind::Slash)
                }
            },
            '%' => {
                if self.match_char('=') {
                    self.add_token(TokenKind::PercentEqual)
                } else {
                    self.add_token(TokenKind::Percent)
                }
            },
            '=' => {
                if self.match_char('=') {
                    self.add_token(TokenKind::EqualEqual)
                } else {
                    self.add_token(TokenKind::Equal)
                }
            },
            '!' => {
                if self.match_char('=') {
                    self.add_token(TokenKind::BangEqual)
                } else {
                    self.add_token(TokenKind::Bang)
                }
            },
            '<' => {
                if self.match_char('=') {
                    self.add_token(TokenKind::LessEqual)
                } else {
                    self.add_token(TokenKind::Less)
                }
            },
            '>' => {
                if self.match_char('=') {
                    self.add_token(TokenKind::GreaterEqual)
                } else {
                    self.add_token(TokenKind::Greater)
                }
            },
            '&' => {
                if self.match_char('&') {
                    self.add_token(TokenKind::AndAnd)
                } else {
                    self.add_token(TokenKind::And)
                }
            },
            '|' => {
                if self.match_char('|') {
                    self.add_token(TokenKind::OrOr)
                } else {
                    self.add_token(TokenKind::Or)
                }
            },
            
            // 字符串字面量
            '"' => self.string()?,
            
            // 空白字符
            ' ' | '\r' | '\t' => {},
            '\n' => {
                self.line += 1;
                self.column = 0;
            },
            
            // 指令
            '#' => self.directive()?,
            
            // 其他字符
            _ => {
                if self.is_digit(c) {
                    self.number()?;
                } else if self.is_alpha(c) {
                    self.identifier()?;
                } else {
                    return Err(self.error(&format!("Unexpected character: {}", c)));
                }
            }
        }
        
        Ok(())
    }
    
    /// 处理指令
    fn directive(&mut self) -> Result<(), Error> {
        // 跳过 #
        while self.is_alpha(self.peek()) {
            self.advance();
        }
        
        // 获取指令名称
        let start = self.start + 1; // 跳过 #
        let directive_name = self.source[start..self.current].to_string();
        
        match directive_name.as_str() {
            "ownership" => {
                // 处理所有权指令
                if self.match_char(':') {
                    // 跳过 :
                    let mode_start = self.current;
                    
                    // 读取模式
                    while self.is_alpha(self.peek()) {
                        self.advance();
                    }
                    
                    let mode = self.source[mode_start..self.current].to_string();
                    
                    if !["on", "off", "auto"].contains(&mode.as_str()) {
                        return Err(self.error(&format!("Invalid ownership mode: {}", mode)));
                    }
                    
                    self.add_token_with_lexeme(TokenKind::OwnershipDirective, format!("{}:{}", directive_name, mode));
                } else {
                    return Err(self.error("Expected ':' after ownership directive"));
                }
            },
            "dependent_types" => {
                // 处理依赖类型指令
                if self.match_char(':') {
                    // 跳过 :
                    let mode_start = self.current;
                    
                    // 读取模式
                    while self.is_alpha(self.peek()) {
                        self.advance();
                    }
                    
                    let mode = self.source[mode_start..self.current].to_string();
                    
                    if !["on", "off"].contains(&mode.as_str()) {
                        return Err(self.error(&format!("Invalid dependent types mode: {}", mode)));
                    }
                    
                    self.add_token_with_lexeme(TokenKind::DependentTypesDirective, format!("{}:{}", directive_name, mode));
                } else {
                    return Err(self.error("Expected ':' after dependent_types directive"));
                }
            },
            "ownership_block" => {
                // 处理所有权块指令
                if self.match_char('(') {
                    // 跳过 (
                    let mode_start = self.current;
                    
                    // 读取模式
                    while self.is_alpha(self.peek()) {
                        self.advance();
                    }
                    
                    let mode = self.source[mode_start..self.current].to_string();
                    
                    if !["on", "off", "auto"].contains(&mode.as_str()) {
                        return Err(self.error(&format!("Invalid ownership mode: {}", mode)));
                    }
                    
                    if !self.match_char(')') {
                        return Err(self.error("Expected ')' after ownership mode"));
                    }
                    
                    self.add_token_with_lexeme(TokenKind::OwnershipBlockDirective, format!("{}({})", directive_name, mode));
                } else {
                    return Err(self.error("Expected '(' after ownership_block directive"));
                }
            },
            "dependent_types_block" => {
                // 处理依赖类型块指令
                if self.match_char('(') {
                    // 跳过 (
                    let mode_start = self.current;
                    
                    // 读取模式
                    while self.is_alpha(self.peek()) {
                        self.advance();
                    }
                    
                    let mode = self.source[mode_start..self.current].to_string();
                    
                    if !["on", "off"].contains(&mode.as_str()) {
                        return Err(self.error(&format!("Invalid dependent types mode: {}", mode)));
                    }
                    
                    if !self.match_char(')') {
                        return Err(self.error("Expected ')' after dependent types mode"));
                    }
                    
                    self.add_token_with_lexeme(TokenKind::DependentTypesBlockDirective, format!("{}({})", directive_name, mode));
                } else {
                    return Err(self.error("Expected '(' after dependent_types_block directive"));
                }
            },
            _ => {
                return Err(self.error(&format!("Unknown directive: {}", directive_name)));
            }
        }
        
        Ok(())
    }
    
    /// 处理标识符
    fn identifier(&mut self) -> Result<(), Error> {
        while self.is_alphanumeric(self.peek()) {
            self.advance();
        }
        
        let text = &self.source[self.start..self.current];
        
        // 检查是否是关键字
        let kind = match text {
            "package" => TokenKind::Package,
            "import" => TokenKind::Import,
            "func" => TokenKind::Func,
            "return" => TokenKind::Return,
            "var" => TokenKind::Var,
            "const" => TokenKind::Const,
            "let" => TokenKind::Let,
            "type" => TokenKind::Type,
            "struct" => TokenKind::Struct,
            "interface" => TokenKind::Interface,
            "map" => TokenKind::Map,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "for" => TokenKind::For,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "switch" => TokenKind::Switch,
            "case" => TokenKind::Case,
            "default" => TokenKind::Default,
            "true" => TokenKind::BoolLiteral,
            "false" => TokenKind::BoolLiteral,
            "nil" => TokenKind::NilLiteral,
            _ => TokenKind::Identifier,
        };
        
        self.add_token(kind);
        Ok(())
    }
    
    /// 处理数字
    fn number(&mut self) -> Result<(), Error> {
        // 整数部分
        while self.is_digit(self.peek()) {
            self.advance();
        }
        
        // 小数部分
        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            // 消费 .
            self.advance();
            
            while self.is_digit(self.peek()) {
                self.advance();
            }
            
            self.add_token(TokenKind::FloatLiteral);
        } else {
            self.add_token(TokenKind::IntLiteral);
        }
        
        Ok(())
    }
    
    /// 处理字符串
    fn string(&mut self) -> Result<(), Error> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
                self.column = 0;
            }
            self.advance();
        }
        
        if self.is_at_end() {
            return Err(self.error("Unterminated string"));
        }
        
        // 消费结束的 "
        self.advance();
        
        self.add_token(TokenKind::StringLiteral);
        Ok(())
    }
    
    /// 添加标记
    fn add_token(&mut self, kind: TokenKind) {
        let lexeme = self.source[self.start..self.current].to_string();
        self.add_token_with_lexeme(kind, lexeme);
    }
    
    /// 添加带有词素的标记
    fn add_token_with_lexeme(&mut self, kind: TokenKind, lexeme: String) {
        self.tokens.push(Token {
            kind,
            lexeme,
            position: Position {
                line: self.line,
                column: self.column - (self.current - self.start),
            },
        });
    }
    
    /// 前进到下一个字符
    fn advance(&mut self) -> char {
        let c = self.source.chars().nth(self.current).unwrap_or('\0');
        self.current += 1;
        self.column += 1;
        c
    }
    
    /// 匹配当前字符
    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.source.chars().nth(self.current).unwrap_or('\0') != expected {
            return false;
        }
        
        self.current += 1;
        self.column += 1;
        true
    }
    
    /// 查看当前字符
    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source.chars().nth(self.current).unwrap_or('\0')
        }
    }
    
    /// 查看下一个字符
    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source.chars().nth(self.current + 1).unwrap_or('\0')
        }
    }
    
    /// 检查是否是数字
    fn is_digit(&self, c: char) -> bool {
        c >= '0' && c <= '9'
    }
    
    /// 检查是否是字母
    fn is_alpha(&self, c: char) -> bool {
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
    }
    
    /// 检查是否是字母或数字
    fn is_alphanumeric(&self, c: char) -> bool {
        self.is_alpha(c) || self.is_digit(c)
    }
    
    /// 检查是否到达末尾
    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
    
    /// 创建错误
    fn error(&self, message: &str) -> Error {
        Error::lexical(message, self.current_file.clone(), self.line, self.column)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_lexer_basic() {
        let source = "package main;\nfunc main() {\n\treturn 42;\n}\n";
        let mut lexer = Lexer::new(source, PathBuf::from("test.gx"));
        let tokens = lexer.tokenize().unwrap();
        
        assert_eq!(tokens[0].kind, TokenKind::Package);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::Semicolon);
        assert_eq!(tokens[3].kind, TokenKind::Func);
        assert_eq!(tokens[4].kind, TokenKind::Identifier);
        assert_eq!(tokens[5].kind, TokenKind::LeftParen);
        assert_eq!(tokens[6].kind, TokenKind::RightParen);
        assert_eq!(tokens[7].kind, TokenKind::LeftBrace);
        assert_eq!(tokens[8].kind, TokenKind::Return);
        assert_eq!(tokens[9].kind, TokenKind::IntLiteral);
        assert_eq!(tokens[10].kind, TokenKind::Semicolon);
        assert_eq!(tokens[11].kind, TokenKind::RightBrace);
        assert_eq!(tokens[12].kind, TokenKind::EOF);
    }
    
    #[test]
    fn test_lexer_string() {
        let source = "\"Hello, World!\"";
        let mut lexer = Lexer::new(source, PathBuf::from("test.gx"));
        let tokens = lexer.tokenize().unwrap();
        
        assert_eq!(tokens[0].kind, TokenKind::StringLiteral);
        assert_eq!(tokens[0].lexeme, "\"Hello, World!\"");
    }
    
    #[test]
    fn test_lexer_number() {
        let source = "42 3.14";
        let mut lexer = Lexer::new(source, PathBuf::from("test.gx"));
        let tokens = lexer.tokenize().unwrap();
        
        assert_eq!(tokens[0].kind, TokenKind::IntLiteral);
        assert_eq!(tokens[0].lexeme, "42");
        assert_eq!(tokens[1].kind, TokenKind::FloatLiteral);
        assert_eq!(tokens[1].lexeme, "3.14");
    }
    
    #[test]
    fn test_lexer_directive() {
        let source = "#ownership:on;\n#dependent_types:off;\n#ownership_block(auto) {}\n#dependent_types_block(on) {}";
        let mut lexer = Lexer::new(source, PathBuf::from("test.gx"));
        let tokens = lexer.tokenize().unwrap();
        
        assert_eq!(tokens[0].kind, TokenKind::OwnershipDirective);
        assert_eq!(tokens[0].lexeme, "ownership:on");
        assert_eq!(tokens[1].kind, TokenKind::Semicolon);
        assert_eq!(tokens[2].kind, TokenKind::DependentTypesDirective);
        assert_eq!(tokens[2].lexeme, "dependent_types:off");
        assert_eq!(tokens[3].kind, TokenKind::Semicolon);
        assert_eq!(tokens[4].kind, TokenKind::OwnershipBlockDirective);
        assert_eq!(tokens[4].lexeme, "ownership_block(auto)");
        assert_eq!(tokens[5].kind, TokenKind::LeftBrace);
        assert_eq!(tokens[6].kind, TokenKind::RightBrace);
        assert_eq!(tokens[7].kind, TokenKind::DependentTypesBlockDirective);
        assert_eq!(tokens[7].lexeme, "dependent_types_block(on)");
        assert_eq!(tokens[8].kind, TokenKind::LeftBrace);
        assert_eq!(tokens[9].kind, TokenKind::RightBrace);
    }
}
