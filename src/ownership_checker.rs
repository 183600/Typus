use crate::ast::{AstNode, OwnershipMode};
use crate::symbol_table::{SymbolTable};
use crate::error::Error;
use std::path::PathBuf;

/// 所有权检查器
pub struct OwnershipChecker {
    /// 符号表
    pub symbol_table: SymbolTable,
    /// 当前文件
    pub current_file: Option<PathBuf>,
}

impl OwnershipChecker {
    /// 创建新的所有权检查器
    pub fn new(symbol_table: SymbolTable) -> Self {
        OwnershipChecker {
            symbol_table,
            current_file: None,
        }
    }
    
    /// 设置当前文件
    pub fn set_current_file(&mut self, file: PathBuf) {
        self.current_file = Some(file.clone());
        self.symbol_table.set_current_file(file);
    }
    
    /// 执行所有权检查
    pub fn check(&mut self, ast: &AstNode) -> Result<(), Error> {
        self.check_node(ast)
    }
    
    /// 检查节点
    fn check_node(&mut self, node: &AstNode) -> Result<(), Error> {
        match node {
            AstNode::Program { package: _, imports: _, declarations } => {
                // 检查程序
                for decl in declarations {
                    self.check_node(decl)?;
                }
            },
            AstNode::FunctionDecl { name: _, type_params: _, params, return_type: _, body } => {
                // 检查函数声明
                self.symbol_table.enter_scope();
                
                // 检查参数
                for param in params {
                    self.check_param(param)?;
                }
                
                // 检查函数体
                self.check_node(body)?;
                
                self.symbol_table.exit_scope();
            },
            AstNode::VarDecl { is_let: _, name: _, type_annotation: _, initializer } => {
                // 检查变量声明
                if let Some(init) = initializer {
                    self.check_node(init)?;
                    
                    // 如果使用所有权模式，检查初始化表达式是否会导致所有权转移
                    if self.symbol_table.uses_ownership() {
                        self.check_ownership_transfer(init)?;
                    }
                }
            },
            AstNode::BlockStmt { statements } => {
                // 检查块语句
                self.symbol_table.enter_scope();
                
                for stmt in statements {
                    self.check_node(stmt)?;
                }
                
                self.symbol_table.exit_scope();
            },
            AstNode::IfStmt { condition, then_branch, else_branch } => {
                // 检查条件
                self.check_node(condition)?;
                
                // 检查 then 分支
                self.check_node(then_branch)?;
                
                // 检查 else 分支
                if let Some(else_branch) = else_branch {
                    self.check_node(else_branch)?;
                }
            },
            AstNode::ForStmt { init, condition, update, body } => {
                // 检查 for 语句
                self.symbol_table.enter_scope();
                
                if let Some(init) = init {
                    self.check_node(init)?;
                }
                
                if let Some(condition) = condition {
                    self.check_node(condition)?;
                }
                
                if let Some(update) = update {
                    self.check_node(update)?;
                }
                
                self.check_node(body)?;
                
                self.symbol_table.exit_scope();
            },
            AstNode::ReturnStmt { value } => {
                // 检查 return 语句
                if let Some(value) = value {
                    self.check_node(value)?;
                    
                    // 如果使用所有权模式，检查返回值是否会导致所有权转移
                    if self.symbol_table.uses_ownership() {
                        self.check_ownership_transfer(value)?;
                    }
                }
            },
            AstNode::ExprStmt { expr } => {
                // 检查表达式语句
                self.check_node(expr)?;
            },
            AstNode::BinaryExpr { left, operator: _, right } => {
                // 检查二元表达式
                self.check_node(left)?;
                self.check_node(right)?;
                
                // 如果是赋值表达式，检查所有权转移
                if self.symbol_table.uses_ownership() {
                    match **right {
                        AstNode::Identifier { ref name } => {
                            self.check_variable_usage(name)?;
                        },
                        _ => {
                            self.check_node(right)?;
                        }
                    }
                }
            },
            AstNode::UnaryExpr { operator: _, operand } => {
                // 检查一元表达式
                self.check_node(operand)?;
            },
            AstNode::CallExpr { callee, args } => {
                // 检查调用表达式
                self.check_node(callee)?;
                
                // 检查参数
                for arg in args {
                    self.check_node(arg)?;
                    
                    // 如果使用所有权模式，检查参数是否会导致所有权转移
                    if self.symbol_table.uses_ownership() {
                        match **arg {
                            AstNode::Identifier { ref name } => {
                                self.check_variable_usage(name)?;
                            },
                            _ => {
                                self.check_node(arg)?;
                            }
                        }
                    }
                }
            },
            AstNode::IndexExpr { object, index } => {
                // 检查索引表达式
                self.check_node(object)?;
                self.check_node(index)?;
            },
            AstNode::FieldExpr { object, field: _ } => {
                // 检查字段访问表达式
                self.check_node(object)?;
            },
            AstNode::Identifier { name } => {
                // 检查标识符
                if self.symbol_table.uses_ownership() {
                    self.check_variable_usage(name)?;
                }
            },
            AstNode::OwnershipDirective { mode } => {
                // 处理所有权指令
                match mode {
                    OwnershipMode::On => self.symbol_table.set_ownership_mode(true),
                    OwnershipMode::Off => self.symbol_table.set_ownership_mode(false),
                    OwnershipMode::Auto => {}, // 自动模式不改变当前设置
                }
            },
            AstNode::OwnershipBlockDirective { mode, body } => {
                // 处理所有权块指令
                let old_mode = self.symbol_table.uses_ownership();
                
                match mode {
                    OwnershipMode::On => self.symbol_table.set_ownership_mode(true),
                    OwnershipMode::Off => self.symbol_table.set_ownership_mode(false),
                    OwnershipMode::Auto => {}, // 自动模式不改变当前设置
                }
                
                self.check_node(body)?;
                
                // 恢复原来的模式
                self.symbol_table.set_ownership_mode(old_mode);
            },
            // 其他 AST 节点类型...
            _ => {
                // 默认情况，不做特殊处理
            }
        }
        
        Ok(())
    }
    
    /// 检查参数
    fn check_param(&mut self, param: &crate::ast::Parameter) -> Result<(), Error> {
        // 如果使用所有权模式，检查参数是否是可变借用
        if self.symbol_table.uses_ownership() && param.is_mutable {
            // 可变借用检查
        }
        
        Ok(())
    }
    
    /// 检查变量使用
    fn check_variable_usage(&mut self, name: &str) -> Result<(), Error> {
        if let Some(symbol) = self.symbol_table.resolve_mut(name) {
            if symbol.uses_ownership {
                if symbol.is_moved {
                    return Err(Error::ownership(
                        &format!("Use of moved variable: {}", name),
                        self.current_file.clone().unwrap_or_else(|| PathBuf::from("unknown")),
                        0, // 实际应该从 AST 节点获取
                        0,
                    ));
                }
                
                if symbol.is_borrowed && symbol.is_mut_borrowed {
                    return Err(Error::ownership(
                        &format!("Cannot use variable while mutably borrowed: {}", name),
                        self.current_file.clone().unwrap_or_else(|| PathBuf::from("unknown")),
                        0, // 实际应该从 AST 节点获取
                        0,
                    ));
                }
            }
        }
        
        Ok(())
    }
    
    /// 检查所有权转移
    fn check_ownership_transfer(&mut self, expr: &AstNode) -> Result<(), Error> {
        match expr {
            AstNode::Identifier { name } => {
                if let Some(symbol) = self.symbol_table.resolve_mut(name) {
                    if symbol.uses_ownership {
                        if symbol.is_moved {
                            return Err(Error::ownership(
                                &format!("Use of moved variable: {}", name),
                                self.current_file.clone().unwrap_or_else(|| PathBuf::from("unknown")),
                                0, // 实际应该从 AST 节点获取
                                0,
                            ));
                        }
                        
                        if symbol.is_borrowed {
                            return Err(Error::ownership(
                                &format!("Cannot move borrowed variable: {}", name),
                                self.current_file.clone().unwrap_or_else(|| PathBuf::from("unknown")),
                                0, // 实际应该从 AST 节点获取
                                0,
                            ));
                        }
                        
                        // 标记变量已被移动
                        symbol.is_moved = true;
                    }
                }
            },
            // 其他表达式类型...
            _ => {
                // 递归检查子表达式
                match expr {
                    AstNode::BinaryExpr { left, right, .. } => {
                        self.check_ownership_transfer(left)?;
                        self.check_ownership_transfer(right)?;
                    },
                    AstNode::UnaryExpr { operand, .. } => {
                        self.check_ownership_transfer(operand)?;
                    },
                    AstNode::CallExpr { callee, args } => {
                        self.check_ownership_transfer(callee)?;
                        for arg in args {
                            self.check_ownership_transfer(arg)?;
                        }
                    },
                    AstNode::IndexExpr { object, index } => {
                        self.check_ownership_transfer(object)?;
                        self.check_ownership_transfer(index)?;
                    },
                    AstNode::FieldExpr { object, .. } => {
                        self.check_ownership_transfer(object)?;
                    },
                    _ => {},
                }
            }
        }
        
        Ok(())
    }
    
    /// 检查借用
    fn check_borrow(&mut self, expr: &AstNode, is_mutable: bool) -> Result<(), Error> {
        match expr {
            AstNode::Identifier { name } => {
                if let Some(symbol) = self.symbol_table.resolve_mut(name) {
                    if symbol.uses_ownership {
                        if symbol.is_moved {
                            return Err(Error::ownership(
                                &format!("Cannot borrow moved variable: {}", name),
                                self.current_file.clone().unwrap_or_else(|| PathBuf::from("unknown")),
                                0, // 实际应该从 AST 节点获取
                                0,
                            ));
                        }
                        
                        if is_mutable {
                            if symbol.is_borrowed {
                                return Err(Error::ownership(
                                    &format!("Cannot mutably borrow already borrowed variable: {}", name),
                                    self.current_file.clone().unwrap_or_else(|| PathBuf::from("unknown")),
                                    0, // 实际应该从 AST 节点获取
                                    0,
                                ));
                            }
                            
                            if !symbol.is_mutable {
                                return Err(Error::ownership(
                                    &format!("Cannot mutably borrow immutable variable: {}", name),
                                    self.current_file.clone().unwrap_or_else(|| PathBuf::from("unknown")),
                                    0, // 实际应该从 AST 节点获取
                                    0,
                                ));
                            }
                            
                            // 标记变量已被可变借用
                            symbol.is_borrowed = true;
                            symbol.is_mut_borrowed = true;
                        } else {
                            if symbol.is_mut_borrowed {
                                return Err(Error::ownership(
                                    &format!("Cannot immutably borrow mutably borrowed variable: {}", name),
                                    self.current_file.clone().unwrap_or_else(|| PathBuf::from("unknown")),
                                    0, // 实际应该从 AST 节点获取
                                    0,
                                ));
                            }
                            
                            // 标记变量已被不可变借用
                            symbol.is_borrowed = true;
                        }
                    }
                }
            },
            // 其他表达式类型...
            _ => {
                // 递归检查子表达式
            }
        }
        
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_ownership_checker_basic() {
        let mut symbol_table = SymbolTable::new();
        symbol_table.set_ownership_mode(true);
        
        let mut ownership_checker = OwnershipChecker::new(symbol_table);
        
        // 创建一个简单的 AST
        let ast = AstNode::Program {
            package: "main".to_string(),
            imports: Vec::new(),
            declarations: vec![
                Box::new(AstNode::FunctionDecl {
                    name: "main".to_string(),
                    type_params: Vec::new(),
                    params: Vec::new(),
                    return_type: None,
                    body: Box::new(AstNode::BlockStmt {
                        statements: Vec::new(),
                    }),
                }),
            ],
        };
        
        // 执行所有权检查
        let result = ownership_checker.check(&ast);
        assert!(result.is_ok());
    }
}
