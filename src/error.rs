use std::path::PathBuf;
use std::fmt;

/// 错误类型
#[derive(Debug, Clone)]
pub enum ErrorKind {
    /// 词法错误
    Lexical,
    /// 语法错误
    Syntax,
    /// 语义错误
    Semantic,
    /// 类型错误
    Type,
    /// 所有权错误
    Ownership,
    /// IO 错误
    IO,
    /// 其他错误
    Other,
}

/// 错误
#[derive(Debug, Clone)]
pub struct Error {
    /// 错误类型
    pub kind: ErrorKind,
    /// 错误消息
    pub message: String,
    /// 文件
    pub file: PathBuf,
    /// 行号
    pub line: usize,
    /// 列号
    pub column: usize,
}

impl Error {
    /// 创建词法错误
    pub fn lexical(message: &str, file: PathBuf, line: usize, column: usize) -> Self {
        Error {
            kind: ErrorKind::Lexical,
            message: message.to_string(),
            file,
            line,
            column,
        }
    }
    
    /// 创建语法错误
    pub fn syntax(message: &str, file: PathBuf, line: usize, column: usize) -> Self {
        Error {
            kind: ErrorKind::Syntax,
            message: message.to_string(),
            file,
            line,
            column,
        }
    }
    
    /// 创建语义错误
    pub fn semantic(message: &str, file: PathBuf, line: usize, column: usize) -> Self {
        Error {
            kind: ErrorKind::Semantic,
            message: message.to_string(),
            file,
            line,
            column,
        }
    }
    
    /// 创建类型错误
    pub fn type_error(message: &str, file: PathBuf, line: usize, column: usize) -> Self {
        Error {
            kind: ErrorKind::Type,
            message: message.to_string(),
            file,
            line,
            column,
        }
    }
    
    /// 创建所有权错误
    pub fn ownership(message: &str, file: PathBuf, line: usize, column: usize) -> Self {
        Error {
            kind: ErrorKind::Ownership,
            message: message.to_string(),
            file,
            line,
            column,
        }
    }
    
    /// 创建 IO 错误
    pub fn io(message: &str, file: PathBuf) -> Self {
        Error {
            kind: ErrorKind::IO,
            message: message.to_string(),
            file,
            line: 0,
            column: 0,
        }
    }
    
    /// 创建其他错误
    pub fn other(message: &str) -> Self {
        Error {
            kind: ErrorKind::Other,
            message: message.to_string(),
            file: PathBuf::from("unknown"),
            line: 0,
            column: 0,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let kind = match self.kind {
            ErrorKind::Lexical => "Lexical",
            ErrorKind::Syntax => "Syntax",
            ErrorKind::Semantic => "Semantic",
            ErrorKind::Type => "Type",
            ErrorKind::Ownership => "Ownership",
            ErrorKind::IO => "IO",
            ErrorKind::Other => "Other",
        };
        
        if self.line > 0 && self.column > 0 {
            write!(
                f,
                "{} error at {}:{}:{}: {}",
                kind,
                self.file.display(),
                self.line,
                self.column,
                self.message
            )
        } else {
            write!(f, "{} error: {}", kind, self.message)
        }
    }
}

impl std::error::Error for Error {}
