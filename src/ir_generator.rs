use crate::ast::{AstNode, OwnershipMode};
use crate::symbol_table::SymbolTable;
use crate::error::Error;
use std::path::PathBuf;

/// 中间表示节点类型
#[derive(Debug, Clone)]
pub enum IRNode {
    /// 程序
    Program {
        package: String,
        imports: Vec<String>,
        declarations: Vec<Box<IRNode>>,
    },
    
    /// 函数声明
    FunctionDecl {
        name: String,
        params: Vec<IRParam>,
        return_type: Option<Box<IRNode>>,
        body: Box<IRNode>,
        has_ownership: bool,
        has_dependent_types: bool,
    },
    
    /// 类型声明
    TypeDecl {
        name: String,
        underlying_type: Box<IRNode>,
        constraints: Option<String>,
        has_dependent_types: bool,
    },
    
    /// 变量声明
    VarDecl {
        name: String,
        type_node: Option<Box<IRNode>>,
        initializer: Option<Box<IRNode>>,
        is_mutable: bool,
        has_ownership: bool,
    },
    
    /// 表达式语句
    ExprStmt {
        expr: Box<IRNode>,
    },
    
    /// 二元表达式
    BinaryExpr {
        left: Box<IRNode>,
        operator: String,
        right: Box<IRNode>,
    },
    
    /// 一元表达式
    UnaryExpr {
        operator: String,
        operand: Box<IRNode>,
    },
    
    /// 调用表达式
    CallExpr {
        callee: Box<IRNode>,
        args: Vec<Box<IRNode>>,
    },
    
    /// 索引表达式
    IndexExpr {
        object: Box<IRNode>,
        index: Box<IRNode>,
    },
    
    /// 字段访问表达式
    FieldExpr {
        object: Box<IRNode>,
        field: String,
    },
    
    /// 标识符
    Identifier {
        name: String,
    },
    
    /// 字面量
    IntLiteral { value: i64 },
    FloatLiteral { value: f64 },
    StringLiteral { value: String },
    BoolLiteral { value: bool },
    NilLiteral,
    
    /// 块语句
    BlockStmt {
        statements: Vec<Box<IRNode>>,
    },
    
    /// 条件语句
    IfStmt {
        condition: Box<IRNode>,
        then_branch: Box<IRNode>,
        else_branch: Option<Box<IRNode>>,
    },
    
    /// 循环语句
    ForStmt {
        init: Option<Box<IRNode>>,
        condition: Option<Box<IRNode>>,
        update: Option<Box<IRNode>>,
        body: Box<IRNode>,
    },
    
    /// 返回语句
    ReturnStmt {
        value: Option<Box<IRNode>>,
    },
    
    /// 所有权块
    OwnershipBlock {
        mode: bool, // true for on, false for off
        body: Box<IRNode>,
    },
    
    /// 类型
    BasicType {
        name: String,
    },
    ArrayType {
        element_type: Box<IRNode>,
        size: Option<Box<IRNode>>,
    },
    SliceType {
        element_type: Box<IRNode>,
    },
    MapType {
        key_type: Box<IRNode>,
        value_type: Box<IRNode>,
    },
    StructType {
        fields: Vec<IRField>,
    },
    InterfaceType {
        methods: Vec<IRMethod>,
    },
    FunctionType {
        params: Vec<Box<IRNode>>,
        return_type: Option<Box<IRNode>>,
    },
    PointerType {
        base_type: Box<IRNode>,
        is_mutable: bool,
    },
    DependentType {
        name: String,
        params: Vec<IRTypeParam>,
        underlying_type: Box<IRNode>,
        constraints: Option<String>,
    },
    
    /// 借用表达式
    BorrowExpr {
        expr: Box<IRNode>,
        is_mutable: bool,
    },
    
    /// 解引用表达式
    DerefExpr {
        expr: Box<IRNode>,
    },
    
    /// 类型转换
    TypeCast {
        expr: Box<IRNode>,
        target_type: Box<IRNode>,
    },
    
    /// 运行时检查
    RuntimeCheck {
        condition: Box<IRNode>,
        message: String,
    },
}

/// 中间表示参数
#[derive(Debug, Clone)]
pub struct IRParam {
    /// 名称
    pub name: String,
    /// 类型
    pub type_node: Box<IRNode>,
    /// 是否可变
    pub is_mutable: bool,
}

/// 中间表示字段
#[derive(Debug, Clone)]
pub struct IRField {
    /// 名称
    pub name: String,
    /// 类型
    pub type_node: Box<IRNode>,
    /// 约束
    pub constraint: Option<String>,
}

/// 中间表示方法
#[derive(Debug, Clone)]
pub struct IRMethod {
    /// 名称
    pub name: String,
    /// 参数
    pub params: Vec<IRParam>,
    /// 返回类型
    pub return_type: Option<Box<IRNode>>,
}

/// 中间表示类型参数
#[derive(Debug, Clone)]
pub struct IRTypeParam {
    /// 名称
    pub name: String,
    /// 类型
    pub kind: Option<Box<IRNode>>,
    /// 约束
    pub constraint: Option<String>,
}

/// 中间表示生成器
pub struct IRGenerator {
    /// 符号表
    pub symbol_table: SymbolTable,
    /// 当前文件
    pub current_file: Option<PathBuf>,
}

impl IRGenerator {
    /// 创建新的中间表示生成器
    pub fn new(symbol_table: SymbolTable) -> Self {
        IRGenerator {
            symbol_table,
            current_file: None,
        }
    }
    
    /// 设置当前文件
    pub fn set_current_file(&mut self, file: PathBuf) {
        self.current_file = Some(file.clone());
        self.symbol_table.set_current_file(file);
    }
    
    /// 生成中间表示
    pub fn generate(&mut self, ast: &AstNode) -> Result<IRNode, Error> {
        self.generate_node(ast)
    }
    
    /// 生成节点
    fn generate_node(&mut self, node: &AstNode) -> Result<IRNode, Error> {
        match node {
            AstNode::Program { package, imports, declarations } => {
                // 生成程序
                let mut ir_imports = Vec::new();
                for import in imports {
                    ir_imports.push(import.path.clone());
                }
                
                let mut ir_declarations = Vec::new();
                for decl in declarations {
                    ir_declarations.push(Box::new(self.generate_node(decl)?));
                }
                
                Ok(IRNode::Program {
                    package: package.clone(),
                    imports: ir_imports,
                    declarations: ir_declarations,
                })
            },
            AstNode::FunctionDecl { name, type_params: _, params, return_type, body } => {
                // 生成函数声明
                let mut ir_params = Vec::new();
                for param in params {
                    ir_params.push(IRParam {
                        name: param.name.clone(),
                        type_node: Box::new(self.generate_node(&param.type_node)?),
                        is_mutable: param.is_mutable,
                    });
                }
                
                let ir_return_type = if let Some(ret_type) = return_type {
                    Some(Box::new(self.generate_node(ret_type)?))
                } else {
                    None
                };
                
                let ir_body = Box::new(self.generate_node(body)?);
                
                Ok(IRNode::FunctionDecl {
                    name: name.clone(),
                    params: ir_params,
                    return_type: ir_return_type,
                    body: ir_body,
                    has_ownership: self.symbol_table.uses_ownership(),
                    has_dependent_types: self.symbol_table.uses_dependent_types(),
                })
            },
            AstNode::TypeDecl { name, type_params: _, underlying_type, constraints } => {
                // 生成类型声明
                let ir_underlying_type = Box::new(self.generate_node(underlying_type)?);
                
                let ir_constraints = if let Some(constraint) = constraints {
                    Some(format!("{:?}", constraint)) // 简化实现
                } else {
                    None
                };
                
                Ok(IRNode::TypeDecl {
                    name: name.clone(),
                    underlying_type: ir_underlying_type,
                    constraints: ir_constraints,
                    has_dependent_types: self.symbol_table.uses_dependent_types(),
                })
            },
            AstNode::VarDecl { is_let, name, type_annotation, initializer } => {
                // 生成变量声明
                let ir_type = if let Some(type_anno) = type_annotation {
                    Some(Box::new(self.generate_node(type_anno)?))
                } else {
                    None
                };
                
                let ir_initializer = if let Some(init) = initializer {
                    Some(Box::new(self.generate_node(init)?))
                } else {
                    None
                };
                
                Ok(IRNode::VarDecl {
                    name: name.clone(),
                    type_node: ir_type,
                    initializer: ir_initializer,
                    is_mutable: !is_let,
                    has_ownership: self.symbol_table.uses_ownership(),
                })
            },
            AstNode::BlockStmt { statements } => {
                // 生成块语句
                let mut ir_statements = Vec::new();
                for stmt in statements {
                    ir_statements.push(Box::new(self.generate_node(stmt)?));
                }
                
                Ok(IRNode::BlockStmt {
                    statements: ir_statements,
                })
            },
            AstNode::IfStmt { condition, then_branch, else_branch } => {
                // 生成条件语句
                let ir_condition = Box::new(self.generate_node(condition)?);
                let ir_then_branch = Box::new(self.generate_node(then_branch)?);
                
                let ir_else_branch = if let Some(else_branch) = else_branch {
                    Some(Box::new(self.generate_node(else_branch)?))
                } else {
                    None
                };
                
                Ok(IRNode::IfStmt {
                    condition: ir_condition,
                    then_branch: ir_then_branch,
                    else_branch: ir_else_branch,
                })
            },
            AstNode::ForStmt { init, condition, update, body } => {
                // 生成循环语句
                let ir_init = if let Some(init) = init {
                    Some(Box::new(self.generate_node(init)?))
                } else {
                    None
                };
                
                let ir_condition = if let Some(condition) = condition {
                    Some(Box::new(self.generate_node(condition)?))
                } else {
                    None
                };
                
                let ir_update = if let Some(update) = update {
                    Some(Box::new(self.generate_node(update)?))
                } else {
                    None
                };
                
                let ir_body = Box::new(self.generate_node(body)?);
                
                Ok(IRNode::ForStmt {
                    init: ir_init,
                    condition: ir_condition,
                    update: ir_update,
                    body: ir_body,
                })
            },
            AstNode::ReturnStmt { value } => {
                // 生成返回语句
                let ir_value = if let Some(value) = value {
                    Some(Box::new(self.generate_node(value)?))
                } else {
                    None
                };
                
                Ok(IRNode::ReturnStmt {
                    value: ir_value,
                })
            },
            AstNode::ExprStmt { expr } => {
                // 生成表达式语句
                let ir_expr = Box::new(self.generate_node(expr)?);
                
                Ok(IRNode::ExprStmt {
                    expr: ir_expr,
                })
            },
            AstNode::BinaryExpr { left, operator, right } => {
                // 生成二元表达式
                let ir_left = Box::new(self.generate_node(left)?);
                let ir_right = Box::new(self.generate_node(right)?);
                
                let op_str = format!("{:?}", operator); // 简化实现
                
                Ok(IRNode::BinaryExpr {
                    left: ir_left,
                    operator: op_str,
                    right: ir_right,
                })
            },
            AstNode::UnaryExpr { operator, operand } => {
                // 生成一元表达式
                let ir_operand = Box::new(self.generate_node(operand)?);
                
                let op_str = format!("{:?}", operator); // 简化实现
                
                Ok(IRNode::UnaryExpr {
                    operator: op_str,
                    operand: ir_operand,
                })
            },
            AstNode::CallExpr { callee, args } => {
                // 生成调用表达式
                let ir_callee = Box::new(self.generate_node(callee)?);
                
                let mut ir_args = Vec::new();
                for arg in args {
                    ir_args.push(Box::new(self.generate_node(arg)?));
                }
                
                Ok(IRNode::CallExpr {
                    callee: ir_callee,
                    args: ir_args,
                })
            },
            AstNode::IndexExpr { object, index } => {
                // 生成索引表达式
                let ir_object = Box::new(self.generate_node(object)?);
                let ir_index = Box::new(self.generate_node(index)?);
                
                Ok(IRNode::IndexExpr {
                    object: ir_object,
                    index: ir_index,
                })
            },
            AstNode::FieldExpr { object, field } => {
                // 生成字段访问表达式
                let ir_object = Box::new(self.generate_node(object)?);
                
                Ok(IRNode::FieldExpr {
                    object: ir_object,
                    field: field.clone(),
                })
            },
            AstNode::Identifier { name } => {
                // 生成标识符
                Ok(IRNode::Identifier {
                    name: name.clone(),
                })
            },
            AstNode::IntLiteral { value } => {
                // 生成整数字面量
                Ok(IRNode::IntLiteral {
                    value: *value,
                })
            },
            AstNode::FloatLiteral { value } => {
                // 生成浮点数字面量
                Ok(IRNode::FloatLiteral {
                    value: *value,
                })
            },
            AstNode::StringLiteral { value } => {
                // 生成字符串字面量
                Ok(IRNode::StringLiteral {
                    value: value.clone(),
                })
            },
            AstNode::BoolLiteral { value } => {
                // 生成布尔字面量
                Ok(IRNode::BoolLiteral {
                    value: *value,
                })
            },
            AstNode::NilLiteral => {
                // 生成 nil 字面量
                Ok(IRNode::NilLiteral)
            },
            AstNode::OwnershipDirective { mode } => {
                // 处理所有权指令
                match mode {
                    OwnershipMode::On => self.symbol_table.set_ownership_mode(true),
                    OwnershipMode::Off => self.symbol_table.set_ownership_mode(false),
                    OwnershipMode::Auto => {}, // 自动模式不改变当前设置
                }
                
                // 所有权指令本身不生成 IR 节点
                Ok(IRNode::BlockStmt { statements: Vec::new() })
            },
            AstNode::DependentTypesDirective { enabled } => {
                // 处理依赖类型指令
                self.symbol_table.set_dependent_types_mode(*enabled);
                
                // 依赖类型指令本身不生成 IR 节点
                Ok(IRNode::BlockStmt { statements: Vec::new() })
            },
            AstNode::OwnershipBlockDirective { mode, body } => {
                // 处理所有权块指令
                let old_mode = self.symbol_table.uses_ownership();
                
                let new_mode = match mode {
                    OwnershipMode::On => true,
                    OwnershipMode::Off => false,
                    OwnershipMode::Auto => old_mode, // 自动模式不改变当前设置
                };
                
                self.symbol_table.set_ownership_mode(new_mode);
                
                let ir_body = Box::new(self.generate_node(body)?);
                
                // 恢复原来的模式
                self.symbol_table.set_ownership_mode(old_mode);
                
                Ok(IRNode::OwnershipBlock {
                    mode: new_mode,
                    body: ir_body,
                })
            },
            AstNode::DependentTypesBlockDirective { enabled, body } => {
                // 处理依赖类型块指令
                let old_mode = self.symbol_table.uses_dependent_types();
                
                self.symbol_table.set_dependent_types_mode(*enabled);
                
                let ir_body = Box::new(self.generate_node(body)?);
                
                // 恢复原来的模式
                self.symbol_table.set_dependent_types_mode(old_mode);
                
                // 依赖类型块转换为普通块
                Ok(*ir_body)
            },
            AstNode::BasicType { name } => {
                // 生成基本类型
                Ok(IRNode::BasicType {
                    name: name.clone(),
                })
            },
            AstNode::ArrayType { element_type, size } => {
                // 生成数组类型
                let ir_element_type = Box::new(self.generate_node(element_type)?);
                
                let ir_size = if let Some(size) = size {
                    Some(Box::new(self.generate_node(size)?))
                } else {
                    None
                };
                
                Ok(IRNode::ArrayType {
                    element_type: ir_element_type,
                    size: ir_size,
                })
            },
            AstNode::SliceType { element_type } => {
                // 生成切片类型
                let ir_element_type = Box::new(self.generate_node(element_type)?);
                
                Ok(IRNode::SliceType {
                    element_type: ir_element_type,
                })
            },
            AstNode::MapType { key_type, value_type } => {
                // 生成映射类型
                let ir_key_type = Box::new(self.generate_node(key_type)?);
                let ir_value_type = Box::new(self.generate_node(value_type)?);
                
                Ok(IRNode::MapType {
                    key_type: ir_key_type,
                    value_type: ir_value_type,
                })
            },
            AstNode::StructType { fields } => {
                // 生成结构体类型
                let mut ir_fields = Vec::new();
                for field in fields {
                    let ir_type = Box::new(self.generate_node(&field.type_node)?);
                    
                    let ir_constraint = if let Some(constraint) = &field.constraint {
                        Some(format!("{:?}", constraint)) // 简化实现
                    } else {
                        None
                    };
                    
                    ir_fields.push(IRField {
                        name: field.name.clone(),
                        type_node: ir_type,
                        constraint: ir_constraint,
                    });
                }
                
                Ok(IRNode::StructType {
                    fields: ir_fields,
                })
            },
            AstNode::InterfaceType { methods } => {
                // 生成接口类型
                let mut ir_methods = Vec::new();
                for method in methods {
                    let mut ir_params = Vec::new();
                    for param_type in &method.params {
                        let ir_type = Box::new(self.generate_node(param_type)?);
                        ir_params.push(IRParam {
                            name: "".to_string(), // 接口方法参数通常没有名称
                            type_node: ir_type,
                            is_mutable: false,
                        });
                    }
                    
                    let ir_return_type = if let Some(ret_type) = &method.return_type {
                        Some(Box::new(self.generate_node(ret_type)?))
                    } else {
                        None
                    };
                    
                    ir_methods.push(IRMethod {
                        name: method.name.clone(),
                        params: ir_params,
                        return_type: ir_return_type,
                    });
                }
                
                Ok(IRNode::InterfaceType {
                    methods: ir_methods,
                })
            },
            AstNode::FunctionType { params, return_type } => {
                // 生成函数类型
                let mut ir_params = Vec::new();
                for param in params {
                    ir_params.push(Box::new(self.generate_node(param)?));
                }
                
                let ir_return_type = if let Some(ret_type) = return_type {
                    Some(Box::new(self.generate_node(ret_type)?))
                } else {
                    None
                };
                
                Ok(IRNode::FunctionType {
                    params: ir_params,
                    return_type: ir_return_type,
                })
            },
            AstNode::PointerType { base_type, is_mutable } => {
                // 生成指针类型
                let ir_base_type = Box::new(self.generate_node(base_type)?);
                
                Ok(IRNode::PointerType {
                    base_type: ir_base_type,
                    is_mutable: *is_mutable,
                })
            },
            AstNode::DependentType { name, type_params, underlying_type, constraints } => {
                // 生成依赖类型
                let ir_underlying_type = Box::new(self.generate_node(underlying_type)?);
                
                let mut ir_params = Vec::new();
                for param in type_params {
                    let ir_kind = if let Some(kind) = &param.kind {
                        Some(Box::new(self.generate_node(kind)?))
                    } else {
                        None
                    };
                    
                    let ir_constraint = if let Some(constraint) = &param.constraint {
                        Some(format!("{:?}", constraint)) // 简化实现
                    } else {
                        None
                    };
                    
                    ir_params.push(IRTypeParam {
                        name: param.name.clone(),
                        kind: ir_kind,
                        constraint: ir_constraint,
                    });
                }
                
                let ir_constraints = if let Some(constraint) = constraints {
                    Some(format!("{:?}", constraint)) // 简化实现
                } else {
                    None
                };
                
                Ok(IRNode::DependentType {
                    name: name.clone(),
                    params: ir_params,
                    underlying_type: ir_underlying_type,
                    constraints: ir_constraints,
                })
            },
            // 其他 AST 节点类型...
            _ => {
                // 默认情况，生成空块
                Ok(IRNode::BlockStmt { statements: Vec::new() })
            }
        }
    }
    
    /// 生成运行时检查
    fn generate_runtime_check(&mut self, condition: &AstNode, message: &str) -> Result<IRNode, Error> {
        let ir_condition = Box::new(self.generate_node(condition)?);
        
        Ok(IRNode::RuntimeCheck {
            condition: ir_condition,
            message: message.to_string(),
        })
    }
    
    /// 生成借用表达式
    fn generate_borrow(&mut self, expr: &AstNode, is_mutable: bool) -> Result<IRNode, Error> {
        let ir_expr = Box::new(self.generate_node(expr)?);
        
        Ok(IRNode::BorrowExpr {
            expr: ir_expr,
            is_mutable,
        })
    }
    
    /// 生成解引用表达式
    fn generate_deref(&mut self, expr: &AstNode) -> Result<IRNode, Error> {
        let ir_expr = Box::new(self.generate_node(expr)?);
        
        Ok(IRNode::DerefExpr {
            expr: ir_expr,
        })
    }
    
    /// 生成类型转换
    fn generate_type_cast(&mut self, expr: &AstNode, target_type: &AstNode) -> Result<IRNode, Error> {
        let ir_expr = Box::new(self.generate_node(expr)?);
        let ir_target_type = Box::new(self.generate_node(target_type)?);
        
        Ok(IRNode::TypeCast {
            expr: ir_expr,
            target_type: ir_target_type,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Position, Token, TokenKind};
    
    #[test]
    fn test_ir_generator_basic() {
        let symbol_table = SymbolTable::new();
        let mut ir_generator = IRGenerator::new(symbol_table);
        
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
        
        // 生成中间表示
        let ir = ir_generator.generate(&ast).unwrap();
        
        match ir {
            IRNode::Program { package, declarations, .. } => {
                assert_eq!(package, "main");
                assert_eq!(declarations.len(), 1);
                
                match &*declarations[0] {
                    IRNode::FunctionDecl { name, .. } => {
                        assert_eq!(name, "main");
                    },
                    _ => panic!("Expected FunctionDecl"),
                }
            },
            _ => panic!("Expected Program"),
        }
    }
}
