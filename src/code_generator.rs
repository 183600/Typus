use crate::ir_generator::{IRNode, IRParam, IRField, IRMethod, IRTypeParam};
use crate::error::Error;
use std::path::PathBuf;
use std::fs::{self, File};
use std::io::{self, Write};

/// 代码生成器
pub struct CodeGenerator {
    /// 输出目录
    pub output_dir: PathBuf,
    /// 当前文件
    pub current_file: Option<PathBuf>,
    /// 缩进级别
    pub indent_level: usize,
}

impl CodeGenerator {
    /// 创建新的代码生成器
    pub fn new(output_dir: PathBuf) -> Self {
        CodeGenerator {
            output_dir,
            current_file: None,
            indent_level: 0,
        }
    }
    
    /// 设置当前文件
    pub fn set_current_file(&mut self, file: PathBuf) {
        self.current_file = Some(file);
    }
    
    /// 生成代码
    pub fn generate(&mut self, ir: &IRNode, output_file: &PathBuf) -> Result<(), Error> {
        // 创建输出目录
        if let Some(parent) = output_file.parent() {
            fs::create_dir_all(parent).map_err(|e| {
                Error::io(
                    &format!("Failed to create output directory: {}", e),
                    self.current_file.clone().unwrap_or_else(|| PathBuf::from("unknown")),
                )
            })?;
        }
        
        // 创建输出文件
        let mut file = File::create(output_file).map_err(|e| {
            Error::io(
                &format!("Failed to create output file: {}", e),
                self.current_file.clone().unwrap_or_else(|| PathBuf::from("unknown")),
            )
        })?;
        
        // 生成代码
        let code = self.generate_code(ir)?;
        
        // 写入文件
        file.write_all(code.as_bytes()).map_err(|e| {
            Error::io(
                &format!("Failed to write to output file: {}", e),
                self.current_file.clone().unwrap_or_else(|| PathBuf::from("unknown")),
            )
        })?;
        
        Ok(())
    }
    
    /// 生成代码
    fn generate_code(&mut self, ir: &IRNode) -> Result<String, Error> {
        match ir {
            IRNode::Program { package, imports, declarations } => {
                // 生成程序
                let mut code = format!("package {}\n\n", package);
                
                // 导入
                for import in imports {
                    code.push_str(&format!("import \"{}\"\n", import));
                }
                
                if !imports.is_empty() {
                    code.push('\n');
                }
                
                // 声明
                for decl in declarations {
                    code.push_str(&self.generate_code(decl)?);
                    code.push('\n');
                }
                
                Ok(code)
            },
            IRNode::FunctionDecl { name, params, return_type, body, has_ownership, has_dependent_types } => {
                // 生成函数声明
                let mut code = String::new();
                
                // 添加所有权和依赖类型注释
                if *has_ownership {
                    code.push_str("// Uses ownership\n");
                }
                
                if *has_dependent_types {
                    code.push_str("// Uses dependent types\n");
                }
                
                code.push_str("func ");
                code.push_str(name);
                code.push('(');
                
                // 参数
                let mut param_strs = Vec::new();
                for param in params {
                    let mut param_str = String::new();
                    
                    if param.is_mutable {
                        param_str.push('*');
                    }
                    
                    param_str.push_str(&param.name);
                    param_str.push(' ');
                    param_str.push_str(&self.generate_code(&param.type_node)?);
                    
                    param_strs.push(param_str);
                }
                
                code.push_str(&param_strs.join(", "));
                code.push(')');
                
                // 返回类型
                if let Some(ret_type) = return_type {
                    code.push(' ');
                    code.push_str(&self.generate_code(ret_type)?);
                }
                
                code.push(' ');
                code.push_str(&self.generate_code(body)?);
                
                Ok(code)
            },
            IRNode::TypeDecl { name, underlying_type, constraints, has_dependent_types } => {
                // 生成类型声明
                let mut code = String::new();
                
                // 添加依赖类型注释
                if *has_dependent_types {
                    code.push_str("// Uses dependent types\n");
                }
                
                code.push_str("type ");
                code.push_str(name);
                code.push(' ');
                code.push_str(&self.generate_code(underlying_type)?);
                
                // 约束
                if let Some(constraint) = constraints {
                    code.push_str(" // Constraint: ");
                    code.push_str(constraint);
                }
                
                Ok(code)
            },
            IRNode::VarDecl { name, type_node, initializer, is_mutable, has_ownership } => {
                // 生成变量声明
                let mut code = String::new();
                
                // 添加所有权注释
                if *has_ownership {
                    code.push_str("// Uses ownership\n");
                }
                
                if *is_mutable {
                    code.push_str("var ");
                } else {
                    code.push_str("const ");
                }
                
                code.push_str(name);
                
                // 类型
                if let Some(type_node) = type_node {
                    code.push(' ');
                    code.push_str(&self.generate_code(type_node)?);
                }
                
                // 初始化表达式
                if let Some(init) = initializer {
                    code.push_str(" = ");
                    code.push_str(&self.generate_code(init)?);
                }
                
                code.push_str(";\n");
                
                Ok(code)
            },
            IRNode::BlockStmt { statements } => {
                // 生成块语句
                let mut code = String::from("{\n");
                
                self.indent_level += 1;
                
                for stmt in statements {
                    code.push_str(&self.indent());
                    code.push_str(&self.generate_code(stmt)?);
                    
                    // 添加分号（如果需要）
                    match **stmt {
                        IRNode::BlockStmt { .. } | IRNode::IfStmt { .. } | IRNode::ForStmt { .. } => {},
                        _ => {
                            if !code.ends_with(";\n") && !code.ends_with("}\n") {
                                code.push_str(";\n");
                            }
                        }
                    }
                }
                
                self.indent_level -= 1;
                
                code.push_str(&self.indent());
                code.push('}');
                
                Ok(code)
            },
            IRNode::IfStmt { condition, then_branch, else_branch } => {
                // 生成条件语句
                let mut code = String::from("if ");
                code.push_str(&self.generate_code(condition)?);
                code.push(' ');
                code.push_str(&self.generate_code(then_branch)?);
                
                if let Some(else_branch) = else_branch {
                    code.push_str(" else ");
                    code.push_str(&self.generate_code(else_branch)?);
                }
                
                Ok(code)
            },
            IRNode::ForStmt { init, condition, update, body } => {
                // 生成循环语句
                let mut code = String::from("for ");
                
                if let Some(init) = init {
                    code.push_str(&self.generate_code(init)?);
                    code.push_str("; ");
                }
                
                if let Some(condition) = condition {
                    code.push_str(&self.generate_code(condition)?);
                }
                
                if let Some(update) = update {
                    code.push_str("; ");
                    code.push_str(&self.generate_code(update)?);
                }
                
                code.push(' ');
                code.push_str(&self.generate_code(body)?);
                
                Ok(code)
            },
            IRNode::ReturnStmt { value } => {
                // 生成返回语句
                let mut code = String::from("return");
                
                if let Some(value) = value {
                    code.push(' ');
                    code.push_str(&self.generate_code(value)?);
                }
                
                Ok(code)
            },
            IRNode::ExprStmt { expr } => {
                // 生成表达式语句
                self.generate_code(expr)
            },
            IRNode::BinaryExpr { left, operator, right } => {
                // 生成二元表达式
                let mut code = String::new();
                
                code.push('(');
                code.push_str(&self.generate_code(left)?);
                code.push(' ');
                code.push_str(operator);
                code.push(' ');
                code.push_str(&self.generate_code(right)?);
                code.push(')');
                
                Ok(code)
            },
            IRNode::UnaryExpr { operator, operand } => {
                // 生成一元表达式
                let mut code = String::new();
                
                code.push('(');
                code.push_str(operator);
                code.push_str(&self.generate_code(operand)?);
                code.push(')');
                
                Ok(code)
            },
            IRNode::CallExpr { callee, args } => {
                // 生成调用表达式
                let mut code = String::new();
                
                code.push_str(&self.generate_code(callee)?);
                code.push('(');
                
                let mut arg_strs = Vec::new();
                for arg in args {
                    arg_strs.push(self.generate_code(arg)?);
                }
                
                code.push_str(&arg_strs.join(", "));
                code.push(')');
                
                Ok(code)
            },
            IRNode::IndexExpr { object, index } => {
                // 生成索引表达式
                let mut code = String::new();
                
                code.push_str(&self.generate_code(object)?);
                code.push('[');
                code.push_str(&self.generate_code(index)?);
                code.push(']');
                
                Ok(code)
            },
            IRNode::FieldExpr { object, field } => {
                // 生成字段访问表达式
                let mut code = String::new();
                
                code.push_str(&self.generate_code(object)?);
                code.push('.');
                code.push_str(field);
                
                Ok(code)
            },
            IRNode::Identifier { name } => {
                // 生成标识符
                Ok(name.clone())
            },
            IRNode::IntLiteral { value } => {
                // 生成整数字面量
                Ok(value.to_string())
            },
            IRNode::FloatLiteral { value } => {
                // 生成浮点数字面量
                Ok(value.to_string())
            },
            IRNode::StringLiteral { value } => {
                // 生成字符串字面量
                Ok(format!("\"{}\"", value))
            },
            IRNode::BoolLiteral { value } => {
                // 生成布尔字面量
                Ok(if *value { "true".to_string() } else { "false".to_string() })
            },
            IRNode::NilLiteral => {
                // 生成 nil 字面量
                Ok("nil".to_string())
            },
            IRNode::OwnershipBlock { mode, body } => {
                // 生成所有权块
                let mut code = String::new();
                
                if *mode {
                    code.push_str("// Ownership: on\n");
                } else {
                    code.push_str("// Ownership: off\n");
                }
                
                code.push_str(&self.generate_code(body)?);
                
                Ok(code)
            },
            IRNode::BasicType { name } => {
                // 生成基本类型
                Ok(name.clone())
            },
            IRNode::ArrayType { element_type, size } => {
                // 生成数组类型
                let mut code = String::new();
                
                code.push('[');
                
                if let Some(size) = size {
                    code.push_str(&self.generate_code(size)?);
                }
                
                code.push(']');
                code.push_str(&self.generate_code(element_type)?);
                
                Ok(code)
            },
            IRNode::SliceType { element_type } => {
                // 生成切片类型
                let mut code = String::new();
                
                code.push_str("[]");
                code.push_str(&self.generate_code(element_type)?);
                
                Ok(code)
            },
            IRNode::MapType { key_type, value_type } => {
                // 生成映射类型
                let mut code = String::new();
                
                code.push_str("map[");
                code.push_str(&self.generate_code(key_type)?);
                code.push(']');
                code.push_str(&self.generate_code(value_type)?);
                
                Ok(code)
            },
            IRNode::StructType { fields } => {
                // 生成结构体类型
                let mut code = String::from("struct {\n");
                
                self.indent_level += 1;
                
                for field in fields {
                    code.push_str(&self.indent());
                    code.push_str(&field.name);
                    code.push(' ');
                    code.push_str(&self.generate_code(&field.type_node)?);
                    
                    // 约束
                    if let Some(constraint) = &field.constraint {
                        code.push_str(" // Constraint: ");
                        code.push_str(constraint);
                    }
                    
                    code.push('\n');
                }
                
                self.indent_level -= 1;
                
                code.push_str(&self.indent());
                code.push('}');
                
                Ok(code)
            },
            IRNode::InterfaceType { methods } => {
                // 生成接口类型
                let mut code = String::from("interface {\n");
                
                self.indent_level += 1;
                
                for method in methods {
                    code.push_str(&self.indent());
                    code.push_str(&method.name);
                    code.push('(');
                    
                    let mut param_strs = Vec::new();
                    for param in &method.params {
                        let mut param_str = String::new();
                        
                        if param.is_mutable {
                            param_str.push('*');
                        }
                        
                        if !param.name.is_empty() {
                            param_str.push_str(&param.name);
                            param_str.push(' ');
                        }
                        
                        param_str.push_str(&self.generate_code(&param.type_node)?);
                        
                        param_strs.push(param_str);
                    }
                    
                    code.push_str(&param_strs.join(", "));
                    code.push(')');
                    
                    // 返回类型
                    if let Some(ret_type) = &method.return_type {
                        code.push(' ');
                        code.push_str(&self.generate_code(ret_type)?);
                    }
                    
                    code.push('\n');
                }
                
                self.indent_level -= 1;
                
                code.push_str(&self.indent());
                code.push('}');
                
                Ok(code)
            },
            IRNode::FunctionType { params, return_type } => {
                // 生成函数类型
                let mut code = String::from("func(");
                
                let mut param_strs = Vec::new();
                for param in params {
                    param_strs.push(self.generate_code(param)?);
                }
                
                code.push_str(&param_strs.join(", "));
                code.push(')');
                
                // 返回类型
                if let Some(ret_type) = return_type {
                    code.push(' ');
                    code.push_str(&self.generate_code(ret_type)?);
                }
                
                Ok(code)
            },
            IRNode::PointerType { base_type, is_mutable } => {
                // 生成指针类型
                let mut code = String::new();
                
                if *is_mutable {
                    code.push_str("**");
                } else {
                    code.push('*');
                }
                
                code.push_str(&self.generate_code(base_type)?);
                
                Ok(code)
            },
            IRNode::DependentType { name, params, underlying_type, constraints } => {
                // 生成依赖类型
                let mut code = String::new();
                
                code.push_str(name);
                
                // 类型参数
                if !params.is_empty() {
                    code.push('[');
                    
                    let mut param_strs = Vec::new();
                    for param in params {
                        let mut param_str = String::new();
                        
                        param_str.push_str(&param.name);
                        
                        if let Some(kind) = &param.kind {
                            param_str.push_str(" ");
                            param_str.push_str(&self.generate_code(kind)?);
                        }
                        
                        if let Some(constraint) = &param.constraint {
                            param_str.push_str(" = ");
                            param_str.push_str(constraint);
                        }
                        
                        param_strs.push(param_str);
                    }
                    
                    code.push_str(&param_strs.join(", "));
                    code.push(']');
                }
                
                // 底层类型
                code.push(' ');
                code.push_str(&self.generate_code(underlying_type)?);
                
                // 约束
                if let Some(constraint) = constraints {
                    code.push_str(" // Constraint: ");
                    code.push_str(constraint);
                }
                
                Ok(code)
            },
            IRNode::BorrowExpr { expr, is_mutable } => {
                // 生成借用表达式
                let mut code = String::new();
                
                if *is_mutable {
                    code.push_str("&mut ");
                } else {
                    code.push('&');
                }
                
                code.push_str(&self.generate_code(expr)?);
                
                Ok(code)
            },
            IRNode::DerefExpr { expr } => {
                // 生成解引用表达式
                let mut code = String::new();
                
                code.push('*');
                code.push_str(&self.generate_code(expr)?);
                
                Ok(code)
            },
            IRNode::TypeCast { expr, target_type } => {
                // 生成类型转换
                let mut code = String::new();
                
                code.push_str(&self.generate_code(target_type)?);
                code.push('(');
                code.push_str(&self.generate_code(expr)?);
                code.push(')');
                
                Ok(code)
            },
            IRNode::RuntimeCheck { condition, message } => {
                // 生成运行时检查
                let mut code = String::new();
                
                code.push_str("if !(");
                code.push_str(&self.generate_code(condition)?);
                code.push_str(") {\n");
                
                self.indent_level += 1;
                
                code.push_str(&self.indent());
                code.push_str("panic(\"");
                code.push_str(message);
                code.push_str("\")\n");
                
                self.indent_level -= 1;
                
                code.push_str(&self.indent());
                code.push('}');
                
                Ok(code)
            },
        }
    }
    
    /// 获取当前缩进
    fn indent(&self) -> String {
        "\t".repeat(self.indent_level)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir_generator::IRNode;
    use tempfile::tempdir;
    
    #[test]
    fn test_code_generator_basic() {
        let temp_dir = tempdir().unwrap();
        let output_dir = temp_dir.path().to_path_buf();
        let mut code_generator = CodeGenerator::new(output_dir.clone());
        
        // 创建一个简单的 IR
        let ir = IRNode::Program {
            package: "main".to_string(),
            imports: vec!["fmt".to_string()],
            declarations: vec![
                Box::new(IRNode::FunctionDecl {
                    name: "main".to_string(),
                    params: Vec::new(),
                    return_type: None,
                    body: Box::new(IRNode::BlockStmt {
                        statements: vec![
                            Box::new(IRNode::ExprStmt {
                                expr: Box::new(IRNode::CallExpr {
                                    callee: Box::new(IRNode::FieldExpr {
                                        object: Box::new(IRNode::Identifier {
                                            name: "fmt".to_string(),
                                        }),
                                        field: "Println".to_string(),
                                    }),
                                    args: vec![
                                        Box::new(IRNode::StringLiteral {
                                            value: "Hello, World!".to_string(),
                                        }),
                                    ],
                                }),
                            }),
                        ],
                    }),
                    has_ownership: false,
                    has_dependent_types: false,
                }),
            ],
        };
        
        // 生成代码
        let output_file = output_dir.join("main.go");
        let result = code_generator.generate(&ir, &output_file);
        assert!(result.is_ok());
        
        // 检查生成的文件是否存在
        assert!(output_file.exists());
    }
}
