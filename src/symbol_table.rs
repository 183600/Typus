use std::collections::HashMap;
use std::path::PathBuf;
use crate::ast::{Import, TypeParam};
use crate::error::Error;

/// 符号类型
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolType {
    /// 基本类型
    Basic(String),
    /// 数组类型
    Array(Box<SymbolType>, Option<usize>),
    /// 切片类型
    Slice(Box<SymbolType>),
    /// 映射类型
    Map(Box<SymbolType>, Box<SymbolType>),
    /// 结构体类型
    Struct(Vec<(String, Box<SymbolType>)>),
    /// 接口类型
    Interface(Vec<(String, Vec<Box<SymbolType>>, Option<Box<SymbolType>>)>),
    /// 函数类型
    Function(Vec<Box<SymbolType>>, Option<Box<SymbolType>>),
    /// 指针类型
    Pointer(Box<SymbolType>, bool),
    /// 依赖类型
    Dependent(String, Vec<TypeParam>, Box<SymbolType>, Option<String>),
}

/// 符号种类
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    /// 变量
    Variable,
    /// 函数
    Function,
    /// 类型
    Type,
    /// 包
    Package,
    /// 导入
    Import,
}

/// 符号
#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    /// 名称
    pub name: String,
    /// 类型
    pub symbol_type: Option<SymbolType>,
    /// 种类
    pub kind: SymbolKind,
    /// 是否可变
    pub is_mutable: bool,
    /// 是否使用所有权
    pub uses_ownership: bool,
    /// 是否已移动
    pub is_moved: bool,
    /// 是否已借用
    pub is_borrowed: bool,
    /// 是否已可变借用
    pub is_mut_borrowed: bool,
}

/// 作用域
#[derive(Debug, Clone)]
struct Scope {
    /// 符号表
    symbols: HashMap<String, Symbol>,
}

/// 符号表
#[derive(Debug, Clone)]
pub struct SymbolTable {
    /// 作用域栈
    scopes: Vec<Scope>,
    /// 当前文件
    current_file: Option<PathBuf>,
    /// 是否使用所有权
    uses_ownership_mode: bool,
    /// 是否使用依赖类型
    uses_dependent_types_mode: bool,
}

impl SymbolTable {
    /// 创建新的符号表
    pub fn new() -> Self {
        let global_scope = Scope {
            symbols: HashMap::new(),
        };
        
        SymbolTable {
            scopes: vec![global_scope],
            current_file: None,
            uses_ownership_mode: false,
            uses_dependent_types_mode: false,
        }
    }
    
    /// 设置当前文件
    pub fn set_current_file(&mut self, file: PathBuf) {
        self.current_file = Some(file);
    }
    
    /// 进入新的作用域
    pub fn enter_scope(&mut self) {
        let new_scope = Scope {
            symbols: HashMap::new(),
        };
        self.scopes.push(new_scope);
    }
    
    /// 退出当前作用域
    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }
    
    /// 定义符号
    pub fn define(&mut self, symbol: Symbol) -> Result<(), Error> {
        let scope = self.scopes.last_mut().unwrap();
        
        if scope.symbols.contains_key(&symbol.name) {
            return Err(Error::semantic(
                &format!("Symbol already defined: {}", symbol.name),
                self.current_file.clone().unwrap_or_else(|| PathBuf::from("unknown")),
                0, // 实际应该从 AST 节点获取
                0,
            ));
        }
        
        scope.symbols.insert(symbol.name.clone(), symbol);
        Ok(())
    }
    
    /// 解析符号
    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.symbols.get(name) {
                return Some(symbol);
            }
        }
        None
    }
    
    /// 解析符号（可变）
    pub fn resolve_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(symbol) = scope.symbols.get_mut(name) {
                return Some(symbol);
            }
        }
        None
    }
    
    /// 设置所有权模式
    pub fn set_ownership_mode(&mut self, enabled: bool) {
        self.uses_ownership_mode = enabled;
    }
    
    /// 是否使用所有权
    pub fn uses_ownership(&self) -> bool {
        self.uses_ownership_mode
    }
    
    /// 设置依赖类型模式
    pub fn set_dependent_types_mode(&mut self, enabled: bool) {
        self.uses_dependent_types_mode = enabled;
    }
    
    /// 是否使用依赖类型
    pub fn uses_dependent_types(&self) -> bool {
        self.uses_dependent_types_mode
    }
    
    /// 定义变量
    pub fn define_variable(&mut self, name: &str, symbol_type: Option<SymbolType>, is_mutable: bool) -> Result<(), Error> {
        let symbol = Symbol {
            name: name.to_string(),
            symbol_type,
            kind: SymbolKind::Variable,
            is_mutable,
            uses_ownership: self.uses_ownership_mode,
            is_moved: false,
            is_borrowed: false,
            is_mut_borrowed: false,
        };
        
        self.define(symbol)
    }
    
    /// 定义函数
    pub fn define_function(&mut self, name: &str, params: Vec<SymbolType>, return_type: Option<SymbolType>) -> Result<(), Error> {
        let symbol_type = SymbolType::Function(
            params.into_iter().map(Box::new).collect(),
            return_type.map(Box::new),
        );
        
        let symbol = Symbol {
            name: name.to_string(),
            symbol_type: Some(symbol_type),
            kind: SymbolKind::Function,
            is_mutable: false,
            uses_ownership: self.uses_ownership_mode,
            is_moved: false,
            is_borrowed: false,
            is_mut_borrowed: false,
        };
        
        self.define(symbol)
    }
    
    /// 定义类型
    pub fn define_type(&mut self, name: &str, symbol_type: SymbolType) -> Result<(), Error> {
        let symbol = Symbol {
            name: name.to_string(),
            symbol_type: Some(symbol_type),
            kind: SymbolKind::Type,
            is_mutable: false,
            uses_ownership: self.uses_ownership_mode,
            is_moved: false,
            is_borrowed: false,
            is_mut_borrowed: false,
        };
        
        self.define(symbol)
    }
    
    /// 定义包
    pub fn define_package(&mut self, name: &str) -> Result<(), Error> {
        let symbol = Symbol {
            name: name.to_string(),
            symbol_type: None,
            kind: SymbolKind::Package,
            is_mutable: false,
            uses_ownership: false,
            is_moved: false,
            is_borrowed: false,
            is_mut_borrowed: false,
        };
        
        self.define(symbol)
    }
    
    /// 定义导入
    pub fn define_import(&mut self, name: &str) -> Result<(), Error> {
        let symbol = Symbol {
            name: name.to_string(),
            symbol_type: None,
            kind: SymbolKind::Import,
            is_mutable: false,
            uses_ownership: false,
            is_moved: false,
            is_borrowed: false,
            is_mut_borrowed: false,
        };
        
        self.define(symbol)
    }
    
    /// 添加导入
    pub fn add_import(&mut self, import: &Import) -> Result<(), Error> {
        let name = if let Some(alias) = &import.alias {
            alias.clone()
        } else {
            // 从路径中提取包名
            let path = &import.path;
            path.split('/').last().unwrap_or(path).to_string()
        };
        
        self.define_import(&name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_symbol_table_basic() {
        let mut symbol_table = SymbolTable::new();
        
        // 定义变量
        symbol_table.define_variable("x", Some(SymbolType::Basic("int".to_string())), false).unwrap();
        
        // 解析变量
        let symbol = symbol_table.resolve("x").unwrap();
        assert_eq!(symbol.name, "x");
        assert_eq!(symbol.kind, SymbolKind::Variable);
        
        // 进入新的作用域
        symbol_table.enter_scope();
        
        // 定义同名变量
        symbol_table.define_variable("x", Some(SymbolType::Basic("string".to_string())), true).unwrap();
        
        // 解析变量
        let symbol = symbol_table.resolve("x").unwrap();
        assert_eq!(symbol.name, "x");
        assert_eq!(symbol.is_mutable, true);
        
        // 退出作用域
        symbol_table.exit_scope();
        
        // 解析变量
        let symbol = symbol_table.resolve("x").unwrap();
        assert_eq!(symbol.name, "x");
        assert_eq!(symbol.is_mutable, false);
    }
    
    #[test]
    fn test_symbol_table_ownership() {
        let mut symbol_table = SymbolTable::new();
        
        // 默认不使用所有权
        assert!(!symbol_table.uses_ownership());
        
        // 设置使用所有权
        symbol_table.set_ownership_mode(true);
        assert!(symbol_table.uses_ownership());
        
        // 定义变量
        symbol_table.define_variable("x", Some(SymbolType::Basic("int".to_string())), false).unwrap();
        
        // 解析变量
        let symbol = symbol_table.resolve("x").unwrap();
        assert!(symbol.uses_ownership);
        assert!(!symbol.is_moved);
        
        // 修改变量状态
        let symbol = symbol_table.resolve_mut("x").unwrap();
        symbol.is_moved = true;
        
        // 解析变量
        let symbol = symbol_table.resolve("x").unwrap();
        assert!(symbol.is_moved);
    }
}
