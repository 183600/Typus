use crate::ast::{AstNode, TokenKind};
use crate::symbol_table::{SymbolTable};
use crate::error::Error;
use std::path::PathBuf;

/// 类型检查器
pub struct TypeChecker {
    /// 符号表
    pub symbol_table: SymbolTable,
    /// 当前文件
    pub current_file: Option<PathBuf>,
}

impl TypeChecker {
    /// 创建新的类型检查器
    pub fn new(symbol_table: SymbolTable) -> Self {
        TypeChecker {
            symbol_table,
            current_file: None,
        }
    }
    
    /// 设置当前文件
    pub fn set_current_file(&mut self, file: PathBuf) {
        self.current_file = Some(file.clone());
        self.symbol_table.set_current_file(file);
    }
    
    /// 执行类型检查
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
            AstNode::FunctionDecl { name: _, type_params, params, return_type, body } => {
                // 检查函数声明
                self.symbol_table.enter_scope();
                
                // 检查类型参数
                for type_param in type_params {
                    self.check_type_param(type_param)?;
                }
                
                // 检查参数
                for param in params {
                    self.check_param(param)?;
                }
                
                // 检查返回类型
                if let Some(ret_type) = return_type {
                    self.check_node(ret_type)?;
                }
                
                // 检查函数体
                self.check_node(body)?;
                
                self.symbol_table.exit_scope();
            },
            AstNode::TypeDecl { name: _, type_params, underlying_type, constraints } => {
                // 检查类型声明
                self.symbol_table.enter_scope();
                
                // 检查类型参数
                for type_param in type_params {
                    self.check_type_param(type_param)?;
                }
                
                // 检查底层类型
                self.check_node(underlying_type)?;
                
                // 检查约束
                if let Some(constraint) = constraints {
                    self.check_node(constraint)?;
                }
                
                self.symbol_table.exit_scope();
            },
            AstNode::VarDecl { is_let: _, name: _, type_annotation, initializer } => {
                // 检查变量声明
                if let Some(type_anno) = type_annotation {
                    self.check_node(type_anno)?;
                }
                
                if let Some(init) = initializer {
                    self.check_node(init)?;
                    
                    // 如果有类型注解和初始化表达式，检查类型兼容性
                    if let Some(type_anno) = type_annotation {
                        self.check_type_compatibility(type_anno, init)?;
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
                
                // 检查条件是否为布尔类型
                self.check_boolean_condition(condition)?;
                
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
                    
                    // 检查条件是否为布尔类型
                    self.check_boolean_condition(condition)?;
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
                    
                    // 检查返回值类型与函数返回类型是否兼容
                    // 这里简化实现，实际应该查找当前函数的返回类型并进行比较
                }
            },
            AstNode::ExprStmt { expr } => {
                // 检查表达式语句
                self.check_node(expr)?;
            },
            AstNode::BinaryExpr { left, operator, right } => {
                // 检查二元表达式
                self.check_node(left)?;
                self.check_node(right)?;
                
                // 检查操作符与操作数类型的兼容性
                self.check_binary_operator_compatibility(operator, left, right)?;
            },
            AstNode::UnaryExpr { operator, operand } => {
                // 检查一元表达式
                self.check_node(operand)?;
                
                // 检查操作符与操作数类型的兼容性
                self.check_unary_operator_compatibility(operator, operand)?;
            },
            AstNode::CallExpr { callee, args } => {
                // 检查调用表达式
                self.check_node(callee)?;
                
                // 检查参数
                for arg in args {
                    self.check_node(arg)?;
                }
                
                // 检查调用参数与函数参数的兼容性
                self.check_call_compatibility(callee, args)?;
            },
            AstNode::IndexExpr { object, index } => {
                // 检查索引表达式
                self.check_node(object)?;
                self.check_node(index)?;
                
                // 检查对象是否可索引
                self.check_indexable(object)?;
                
                // 检查索引类型是否正确
                self.check_index_type(index)?;
            },
            AstNode::FieldExpr { object, field } => {
                // 检查字段访问表达式
                self.check_node(object)?;
                
                // 检查对象是否有该字段
                self.check_field_access(object, field)?;
            },
            AstNode::Identifier { name } => {
                // 检查标识符
                if self.symbol_table.resolve(name).is_none() {
                    return Err(Error::semantic(
                        &format!("Undefined identifier: {}", name),
                        self.current_file.clone().unwrap_or_else(|| PathBuf::from("unknown")),
                        0, // 实际应该从 AST 节点获取
                        0,
                    ));
                }
            },
            AstNode::OwnershipDirective { mode } => {
                // 处理所有权指令
                match mode {
                    crate::ast::OwnershipMode::On => self.symbol_table.set_ownership_mode(true),
                    crate::ast::OwnershipMode::Off => self.symbol_table.set_ownership_mode(false),
                    crate::ast::OwnershipMode::Auto => {}, // 自动模式不改变当前设置
                }
            },
            AstNode::DependentTypesDirective { enabled } => {
                // 处理依赖类型指令
                self.symbol_table.set_dependent_types_mode(*enabled);
            },
            AstNode::OwnershipBlockDirective { mode, body } => {
                // 处理所有权块指令
                let old_mode = self.symbol_table.uses_ownership();
                
                match mode {
                    crate::ast::OwnershipMode::On => self.symbol_table.set_ownership_mode(true),
                    crate::ast::OwnershipMode::Off => self.symbol_table.set_ownership_mode(false),
                    crate::ast::OwnershipMode::Auto => {}, // 自动模式不改变当前设置
                }
                
                self.check_node(body)?;
                
                // 恢复原来的模式
                self.symbol_table.set_ownership_mode(old_mode);
            },
            AstNode::DependentTypesBlockDirective { enabled, body } => {
                // 处理依赖类型块指令
                let old_mode = self.symbol_table.uses_dependent_types();
                
                self.symbol_table.set_dependent_types_mode(*enabled);
                
                self.check_node(body)?;
                
                // 恢复原来的模式
                self.symbol_table.set_dependent_types_mode(old_mode);
            },
            // 其他 AST 节点类型...
            _ => {
                // 默认情况，不做特殊处理
            }
        }
        
        Ok(())
    }
    
    /// 检查类型参数
    fn check_type_param(&mut self, type_param: &crate::ast::TypeParam) -> Result<(), Error> {
        // 检查类型参数的约束
        if let Some(constraint) = &type_param.constraint {
            self.check_node(constraint)?;
        }
        
        Ok(())
    }
    
    /// 检查参数
    fn check_param(&mut self, param: &crate::ast::Parameter) -> Result<(), Error> {
        // 检查参数类型
        self.check_node(&param.type_node)?;
        
        // 检查参数约束
        if let Some(constraint) = &param.constraint {
            self.check_node(constraint)?;
        }
        
        Ok(())
    }
    
    /// 检查类型兼容性
    fn check_type_compatibility(&self, _expected: &AstNode, _actual: &AstNode) -> Result<(), Error> {
        // 简化实现，实际应该比较类型
        Ok(())
    }
    
    /// 检查条件是否为布尔类型
    fn check_boolean_condition(&self, _condition: &AstNode) -> Result<(), Error> {
        // 简化实现，实际应该检查条件表达式的类型
        Ok(())
    }
    
    /// 检查二元操作符兼容性
    fn check_binary_operator_compatibility(&self, _operator: &TokenKind, _left: &AstNode, _right: &AstNode) -> Result<(), Error> {
        // 简化实现，实际应该检查操作符与操作数类型的兼容性
        Ok(())
    }
    
    /// 检查一元操作符兼容性
    fn check_unary_operator_compatibility(&self, _operator: &TokenKind, _operand: &AstNode) -> Result<(), Error> {
        // 简化实现，实际应该检查操作符与操作数类型的兼容性
        Ok(())
    }
    
    /// 检查调用兼容性
    fn check_call_compatibility(&self, _callee: &AstNode, _args: &[Box<AstNode>]) -> Result<(), Error> {
        // 简化实现，实际应该检查调用参数与函数参数的兼容性
        Ok(())
    }
    
    /// 检查对象是否可索引
    fn check_indexable(&self, _object: &AstNode) -> Result<(), Error> {
        // 简化实现，实际应该检查对象是否是数组、切片或映射
        Ok(())
    }
    
    /// 检查索引类型
    fn check_index_type(&self, _index: &AstNode) -> Result<(), Error> {
        // 简化实现，实际应该检查索引类型是否正确
        Ok(())
    }
    
    /// 检查字段访问
    fn check_field_access(&self, _object: &AstNode, _field: &str) -> Result<(), Error> {
        // 简化实现，实际应该检查对象是否有该字段
        Ok(())
    }
    
    /// 检查依赖类型约束
    fn check_dependent_type_constraint(&mut self, constraint: &AstNode) -> Result<(), Error> {
        // 检查依赖类型约束
        if !self.symbol_table.uses_dependent_types() {
            return Err(Error::type_error(
                "Dependent type constraints are not allowed in this context",
                self.current_file.clone().unwrap_or_else(|| PathBuf::from("unknown")),
                0, // 实际应该从 AST 节点获取
                0,
            ));
        }
        
        // 检查约束表达式
        self.check_node(constraint)?;
        
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Position, Token};
    
    #[test]
    fn test_type_checker_basic() {
        let symbol_table = SymbolTable::new();
        let mut type_checker = TypeChecker::new(symbol_table);
        
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
        
        // 执行类型检查
        let result = type_checker.check(&ast);
        assert!(result.is_ok());
    }
}
