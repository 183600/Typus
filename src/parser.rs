use crate::ast::{AstNode, Token, TokenKind, Position, Import, TypeParam, Parameter, Field, Method, OwnershipMode};
use crate::error::Error;
use std::path::PathBuf;

/// 语法分析器
pub struct Parser {
    /// 标记列表
    tokens: Vec<Token>,
    /// 当前位置
    current: usize,
    /// 当前文件
    current_file: PathBuf,
}

impl Parser {
    /// 创建新的语法分析器
    pub fn new(tokens: Vec<Token>, current_file: PathBuf) -> Self {
        Parser {
            tokens,
            current: 0,
            current_file,
        }
    }
    
    /// 执行语法分析
    pub fn parse(&mut self) -> Result<AstNode, Error> {
        self.parse_program()
    }
    
    /// 解析程序
    fn parse_program(&mut self) -> Result<AstNode, Error> {
        // 解析包声明
        self.expect(TokenKind::Package)?;
        let package = self.expect_identifier()?;
        self.expect(TokenKind::Semicolon)?;
        
        // 解析导入
        let mut imports = Vec::new();
        while self.check(&TokenKind::Import) {
            imports.push(self.parse_import()?);
            self.expect(TokenKind::Semicolon)?;
        }
        
        // 解析声明
        let mut declarations = Vec::new();
        while !self.is_at_end() {
            declarations.push(Box::new(self.parse_declaration()?));
        }
        
        Ok(AstNode::Program {
            package,
            imports,
            declarations,
        })
    }
    
    /// 解析导入
    fn parse_import(&mut self) -> Result<Import, Error> {
        self.expect(TokenKind::Import)?;
        
        let alias = if self.check(&TokenKind::Identifier) {
            let name = self.expect_identifier()?;
            if self.check(&TokenKind::StringLiteral) {
                Some(name)
            } else {
                // 如果后面不是字符串字面量，则标识符是路径的一部分
                // 这里简化处理，假设标识符总是别名
                return Err(self.error("Expected import path after alias"));
            }
        } else {
            None
        };
        
        let path = self.expect_string_literal()?;
        
        Ok(Import {
            path,
            alias,
        })
    }
    
    /// 解析声明
    fn parse_declaration(&mut self) -> Result<AstNode, Error> {
        if self.check(&TokenKind::Func) {
            self.parse_function_decl()
        } else if self.check(&TokenKind::Type) {
            self.parse_type_decl()
        } else if self.check(&TokenKind::Var) || self.check(&TokenKind::Const) || self.check(&TokenKind::Let) {
            self.parse_var_decl()
        } else if self.check(&TokenKind::OwnershipDirective) {
            self.parse_ownership_directive()
        } else if self.check(&TokenKind::DependentTypesDirective) {
            self.parse_dependent_types_directive()
        } else if self.check(&TokenKind::OwnershipBlockDirective) {
            self.parse_ownership_block_directive()
        } else if self.check(&TokenKind::DependentTypesBlockDirective) {
            self.parse_dependent_types_block_directive()
        } else {
            Err(self.error("Expected declaration"))
        }
    }
    
    /// 解析函数声明
    fn parse_function_decl(&mut self) -> Result<AstNode, Error> {
        self.expect(TokenKind::Func)?;
        let name = self.expect_identifier()?;
        
        // 解析类型参数
        let type_params = if self.check(&TokenKind::LeftBracket) {
            self.parse_type_params()?
        } else {
            Vec::new()
        };
        
        // 解析参数
        self.expect(TokenKind::LeftParen)?;
        let params = self.parse_parameters()?;
        self.expect(TokenKind::RightParen)?;
        
        // 解析返回类型
        let return_type = if self.check(&TokenKind::LeftParen) || self.check(&TokenKind::Identifier) || self.check(&TokenKind::LeftBracket) || self.check(&TokenKind::Star) {
            Some(Box::new(self.parse_type()?))
        } else {
            None
        };
        
        // 解析函数体
        let body = Box::new(self.parse_block_stmt()?);
        
        Ok(AstNode::FunctionDecl {
            name,
            type_params,
            params,
            return_type,
            body,
        })
    }
    
    /// 解析类型参数列表
    fn parse_type_params(&mut self) -> Result<Vec<TypeParam>, Error> {
        self.expect(TokenKind::LeftBracket)?;
        let mut params = Vec::new();
        
        if !self.check(&TokenKind::RightBracket) {
            loop {
                params.push(self.parse_type_param()?);
                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
        }
        
        self.expect(TokenKind::RightBracket)?;
        Ok(params)
    }
    
    /// 解析类型参数
    fn parse_type_param(&mut self) -> Result<TypeParam, Error> {
        let name = self.expect_identifier()?;
        
        let kind = if self.match_token(&TokenKind::Colon) {
            Some(Box::new(self.parse_type()?))
        } else {
            None
        };
        
        let constraint = if self.match_token(&TokenKind::Equal) {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };
        
        Ok(TypeParam {
            name,
            kind,
            constraint,
        })
    }
    
    /// 解析参数列表
    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, Error> {
        let mut params = Vec::new();
        
        if !self.check(&TokenKind::RightParen) {
            loop {
                params.push(self.parse_parameter()?);
                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
        }
        
        Ok(params)
    }
    
    /// 解析参数
    fn parse_parameter(&mut self) -> Result<Parameter, Error> {
        let is_mutable = self.match_token(&TokenKind::Star); // 简化处理，用 * 表示可变借用
        let name = self.expect_identifier()?;
        let type_node = Box::new(self.parse_type()?);
        
        let constraint = if self.match_token(&TokenKind::Colon) {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };
        
        Ok(Parameter {
            name,
            type_node,
            constraint,
            is_mutable,
        })
    }
    
    /// 解析类型声明
    fn parse_type_decl(&mut self) -> Result<AstNode, Error> {
        self.expect(TokenKind::Type)?;
        let name = self.expect_identifier()?;
        
        // 解析类型参数
        let type_params = if self.check(&TokenKind::LeftBracket) {
            self.parse_type_params()?
        } else {
            Vec::new()
        };
        
        // 解析底层类型
        let underlying_type = Box::new(self.parse_type()?);
        
        // 解析约束
        let constraints = if self.match_token(&TokenKind::Colon) {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };
        
        Ok(AstNode::TypeDecl {
            name,
            type_params,
            underlying_type,
            constraints,
        })
    }
    
    /// 解析变量声明
    fn parse_var_decl(&mut self) -> Result<AstNode, Error> {
        let is_let = if self.match_token(&TokenKind::Let) {
            true
        } else {
            self.expect_one_of(&[TokenKind::Var, TokenKind::Const])?;
            false
        };
        
        let name = self.expect_identifier()?;
        
        // 解析类型注解
        let type_annotation = if self.check(&TokenKind::Identifier) || self.check(&TokenKind::LeftBracket) || self.check(&TokenKind::Star) || self.check(&TokenKind::Map) || self.check(&TokenKind::Struct) || self.check(&TokenKind::Interface) || self.check(&TokenKind::Func) {
            Some(Box::new(self.parse_type()?))
        } else {
            None
        };
        
        // 解析初始化表达式
        let initializer = if self.match_token(&TokenKind::Equal) {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };
        
        self.expect(TokenKind::Semicolon)?;
        
        Ok(AstNode::VarDecl {
            is_let,
            name,
            type_annotation,
            initializer,
        })
    }
    
    /// 解析语句
    fn parse_statement(&mut self) -> Result<AstNode, Error> {
        if self.check(&TokenKind::LeftBrace) {
            self.parse_block_stmt()
        } else if self.check(&TokenKind::If) {
            self.parse_if_stmt()
        } else if self.check(&TokenKind::For) {
            self.parse_for_stmt()
        } else if self.check(&TokenKind::Return) {
            self.parse_return_stmt()
        } else if self.check(&TokenKind::Var) || self.check(&TokenKind::Const) || self.check(&TokenKind::Let) {
            self.parse_var_decl()
        } else if self.check(&TokenKind::OwnershipDirective) {
            self.parse_ownership_directive()
        } else if self.check(&TokenKind::DependentTypesDirective) {
            self.parse_dependent_types_directive()
        } else if self.check(&TokenKind::OwnershipBlockDirective) {
            self.parse_ownership_block_directive()
        } else if self.check(&TokenKind::DependentTypesBlockDirective) {
            self.parse_dependent_types_block_directive()
        } else {
            self.parse_expression_stmt()
        }
    }
    
    /// 解析块语句
    fn parse_block_stmt(&mut self) -> Result<AstNode, Error> {
        self.expect(TokenKind::LeftBrace)?;
        let mut statements = Vec::new();
        
        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            statements.push(Box::new(self.parse_statement()?));
        }
        
        self.expect(TokenKind::RightBrace)?;
        Ok(AstNode::BlockStmt {
            statements,
        })
    }
    
    /// 解析条件语句
    fn parse_if_stmt(&mut self) -> Result<AstNode, Error> {
        self.expect(TokenKind::If)?;
        let condition = Box::new(self.parse_expression()?);
        let then_branch = Box::new(self.parse_block_stmt()?);
        
        let else_branch = if self.match_token(&TokenKind::Else) {
            if self.check(&TokenKind::If) {
                Some(Box::new(self.parse_if_stmt()?))
            } else {
                Some(Box::new(self.parse_block_stmt()?))
            }
        } else {
            None
        };
        
        Ok(AstNode::IfStmt {
            condition,
            then_branch,
            else_branch,
        })
    }
    
    /// 解析循环语句
    fn parse_for_stmt(&mut self) -> Result<AstNode, Error> {
        self.expect(TokenKind::For)?;
        
        // 简化处理，只支持 for { ... } 和 for condition { ... }
        if self.check(&TokenKind::LeftBrace) {
            // for { ... }
            let body = Box::new(self.parse_block_stmt()?);
            Ok(AstNode::ForStmt {
                init: None,
                condition: None,
                update: None,
                body,
            })
        } else {
            // for condition { ... }
            let condition = Box::new(self.parse_expression()?);
            let body = Box::new(self.parse_block_stmt()?);
            Ok(AstNode::ForStmt {
                init: None,
                condition: Some(condition),
                update: None,
                body,
            })
        }
    }
    
    /// 解析返回语句
    fn parse_return_stmt(&mut self) -> Result<AstNode, Error> {
        self.expect(TokenKind::Return)?;
        
        let value = if !self.check(&TokenKind::Semicolon) {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };
        
        self.expect(TokenKind::Semicolon)?;
        Ok(AstNode::ReturnStmt {
            value,
        })
    }
    
    /// 解析表达式语句
    fn parse_expression_stmt(&mut self) -> Result<AstNode, Error> {
        let expr = Box::new(self.parse_expression()?);
        self.expect(TokenKind::Semicolon)?;
        Ok(AstNode::ExprStmt {
            expr,
        })
    }
    
    /// 解析表达式
    fn parse_expression(&mut self) -> Result<AstNode, Error> {
        self.parse_assignment()
    }
    
    /// 解析赋值表达式
    fn parse_assignment(&mut self) -> Result<AstNode, Error> {
        let left = self.parse_logical_or()?;
        
        if self.match_token(&TokenKind::Equal) {
            let right = self.parse_assignment()?;
            Ok(AstNode::BinaryExpr {
                left: Box::new(left),
                operator: TokenKind::Equal,
                right: Box::new(right),
            })
        } else if self.match_token(&TokenKind::PlusEqual) {
            let right = self.parse_assignment()?;
            Ok(AstNode::BinaryExpr {
                left: Box::new(left),
                operator: TokenKind::PlusEqual,
                right: Box::new(right),
            })
        } else if self.match_token(&TokenKind::MinusEqual) {
            let right = self.parse_assignment()?;
            Ok(AstNode::BinaryExpr {
                left: Box::new(left),
                operator: TokenKind::MinusEqual,
                right: Box::new(right),
            })
        } else if self.match_token(&TokenKind::StarEqual) {
            let right = self.parse_assignment()?;
            Ok(AstNode::BinaryExpr {
                left: Box::new(left),
                operator: TokenKind::StarEqual,
                right: Box::new(right),
            })
        } else if self.match_token(&TokenKind::SlashEqual) {
            let right = self.parse_assignment()?;
            Ok(AstNode::BinaryExpr {
                left: Box::new(left),
                operator: TokenKind::SlashEqual,
                right: Box::new(right),
            })
        } else if self.match_token(&TokenKind::PercentEqual) {
            let right = self.parse_assignment()?;
            Ok(AstNode::BinaryExpr {
                left: Box::new(left),
                operator: TokenKind::PercentEqual,
                right: Box::new(right),
            })
        } else {
            Ok(left)
        }
    }
    
    /// 解析逻辑或表达式
    fn parse_logical_or(&mut self) -> Result<AstNode, Error> {
        let mut left = self.parse_logical_and()?;
        
        while self.match_token(&TokenKind::OrOr) {
            let right = self.parse_logical_and()?;
            left = AstNode::BinaryExpr {
                left: Box::new(left),
                operator: TokenKind::OrOr,
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }
    
    /// 解析逻辑与表达式
    fn parse_logical_and(&mut self) -> Result<AstNode, Error> {
        let mut left = self.parse_equality()?;
        
        while self.match_token(&TokenKind::AndAnd) {
            let right = self.parse_equality()?;
            left = AstNode::BinaryExpr {
                left: Box::new(left),
                operator: TokenKind::AndAnd,
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }
    
    /// 解析相等性表达式
    fn parse_equality(&mut self) -> Result<AstNode, Error> {
        let mut left = self.parse_comparison()?;
        
        while self.match_one_of(&[TokenKind::EqualEqual, TokenKind::BangEqual]) {
            let operator = self.previous().kind.clone();
            let right = self.parse_comparison()?;
            left = AstNode::BinaryExpr {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }
    
    /// 解析比较表达式
    fn parse_comparison(&mut self) -> Result<AstNode, Error> {
        let mut left = self.parse_term()?;
        
        while self.match_one_of(&[TokenKind::Less, TokenKind::LessEqual, TokenKind::Greater, TokenKind::GreaterEqual]) {
            let operator = self.previous().kind.clone();
            let right = self.parse_term()?;
            left = AstNode::BinaryExpr {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }
    
    /// 解析项表达式
    fn parse_term(&mut self) -> Result<AstNode, Error> {
        let mut left = self.parse_factor()?;
        
        while self.match_one_of(&[TokenKind::Plus, TokenKind::Minus]) {
            let operator = self.previous().kind.clone();
            let right = self.parse_factor()?;
            left = AstNode::BinaryExpr {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }
    
    /// 解析因子表达式
    fn parse_factor(&mut self) -> Result<AstNode, Error> {
        let mut left = self.parse_unary()?;
        
        while self.match_one_of(&[TokenKind::Star, TokenKind::Slash, TokenKind::Percent]) {
            let operator = self.previous().kind.clone();
            let right = self.parse_unary()?;
            left = AstNode::BinaryExpr {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }
    
    /// 解析一元表达式
    fn parse_unary(&mut self) -> Result<AstNode, Error> {
        if self.match_one_of(&[TokenKind::Bang, TokenKind::Minus]) {
            let operator = self.previous().kind.clone();
            let operand = self.parse_unary()?;
            Ok(AstNode::UnaryExpr {
                operator,
                operand: Box::new(operand),
            })
        } else {
            self.parse_call()
        }
    }
    
    /// 解析调用、索引和字段访问表达式
    fn parse_call(&mut self) -> Result<AstNode, Error> {
        let mut expr = self.parse_primary()?;
        
        loop {
            if self.match_token(&TokenKind::LeftParen) {
                // 调用表达式
                let args = self.parse_arguments()?;
                expr = AstNode::CallExpr {
                    callee: Box::new(expr),
                    args,
                };
            } else if self.match_token(&TokenKind::LeftBracket) {
                // 索引表达式
                let index = self.parse_expression()?;
                self.expect(TokenKind::RightBracket)?;
                expr = AstNode::IndexExpr {
                    object: Box::new(expr),
                    index: Box::new(index),
                };
            } else if self.match_token(&TokenKind::Dot) {
                // 字段访问表达式
                let field = self.expect_identifier()?;
                expr = AstNode::FieldExpr {
                    object: Box::new(expr),
                    field,
                };
            } else {
                break;
            }
        }
        
        Ok(expr)
    }
    
    /// 解析参数列表
    fn parse_arguments(&mut self) -> Result<Vec<Box<AstNode>>, Error> {
        let mut args = Vec::new();
        
        if !self.check(&TokenKind::RightParen) {
            loop {
                args.push(Box::new(self.parse_expression()?));
                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
        }
        
        self.expect(TokenKind::RightParen)?;
        Ok(args)
    }
    
    /// 解析基本表达式
    fn parse_primary(&mut self) -> Result<AstNode, Error> {
        if self.match_token(&TokenKind::IntLiteral) {
            let value = self.previous().lexeme.parse::<i64>().map_err(|_| self.error("Invalid integer literal"))?;
            Ok(AstNode::IntLiteral { value })
        } else if self.match_token(&TokenKind::FloatLiteral) {
            let value = self.previous().lexeme.parse::<f64>().map_err(|_| self.error("Invalid float literal"))?;
            Ok(AstNode::FloatLiteral { value })
        } else if self.match_token(&TokenKind::StringLiteral) {
            let value = self.previous().lexeme.clone();
            Ok(AstNode::StringLiteral { value })
        } else if self.match_token(&TokenKind::BoolLiteral) {
            let value = self.previous().lexeme == "true";
            Ok(AstNode::BoolLiteral { value })
        } else if self.match_token(&TokenKind::NilLiteral) {
            Ok(AstNode::NilLiteral)
        } else if self.match_token(&TokenKind::Identifier) {
            let name = self.previous().lexeme.clone();
            Ok(AstNode::Identifier { name })
        } else if self.match_token(&TokenKind::LeftParen) {
            let expr = self.parse_expression()?;
            self.expect(TokenKind::RightParen)?;
            Ok(expr)
        } else {
            Err(self.error("Expected primary expression"))
        }
    }
    
    /// 解析类型
    fn parse_type(&mut self) -> Result<AstNode, Error> {
        if self.match_token(&TokenKind::Identifier) {
            let name = self.previous().lexeme.clone();
            
            // 检查是否是依赖类型
            if self.check(&TokenKind::LeftBracket) {
                self.parse_dependent_type(name)
            } else {
                Ok(AstNode::BasicType { name })
            }
        } else if self.match_token(&TokenKind::LeftBracket) {
            // 数组或切片类型
            if self.match_token(&TokenKind::RightBracket) {
                // 切片类型
                let element_type = Box::new(self.parse_type()?);
                Ok(AstNode::SliceType { element_type })
            } else {
                // 数组类型
                let size = if self.check(&TokenKind::IntLiteral) { // Fix: Check for IntLiteral, not ]
                    Some(Box::new(self.parse_expression()?))
                } else {
                    None
                };
                self.expect(TokenKind::RightBracket)?;
                let element_type = Box::new(self.parse_type()?);
                Ok(AstNode::ArrayType {
                    element_type,
                    size,
                })
            }
        } else if self.match_token(&TokenKind::Map) {
            // 映射类型
            self.expect(TokenKind::LeftBracket)?;
            let key_type = Box::new(self.parse_type()?);
            self.expect(TokenKind::RightBracket)?;
            let value_type = Box::new(self.parse_type()?);
            Ok(AstNode::MapType {
                key_type,
                value_type,
            })
        } else if self.match_token(&TokenKind::Struct) {
            // 结构体类型
            self.expect(TokenKind::LeftBrace)?;
            let fields = self.parse_fields()?;
            self.expect(TokenKind::RightBrace)?;
            Ok(AstNode::StructType {
                fields,
            })
        } else if self.match_token(&TokenKind::Interface) {
            // 接口类型
            self.expect(TokenKind::LeftBrace)?;
            let methods = self.parse_methods()?;
            self.expect(TokenKind::RightBrace)?;
            Ok(AstNode::InterfaceType {
                methods,
            })
        } else if self.match_token(&TokenKind::Func) {
            // 函数类型
            self.expect(TokenKind::LeftParen)?;
            let params = self.parse_type_list()?;
            self.expect(TokenKind::RightParen)?;
            
            let return_type = if self.check(&TokenKind::LeftParen) || self.check(&TokenKind::Identifier) || self.check(&TokenKind::LeftBracket) || self.check(&TokenKind::Star) {
                Some(Box::new(self.parse_type()?))
            } else {
                None
            };
            
            Ok(AstNode::FunctionType {
                params,
                return_type,
            })
        } else if self.match_token(&TokenKind::Star) {
            // 指针类型
            let is_mutable = self.match_token(&TokenKind::Star); // 简化处理，** 表示可变指针
            let base_type = Box::new(self.parse_type()?);
            Ok(AstNode::PointerType {
                base_type,
                is_mutable,
            })
        } else {
            Err(self.error("Expected type"))
        }
    }
    
    /// 解析依赖类型
    fn parse_dependent_type(&mut self, name: String) -> Result<AstNode, Error> {
        let type_params = self.parse_type_params()?;
        let underlying_type = Box::new(self.parse_type()?);
        
        let constraints = if self.match_token(&TokenKind::Colon) {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };
        
        Ok(AstNode::DependentType {
            name,
            type_params,
            underlying_type,
            constraints,
        })
    }
    
    /// 解析字段列表
    fn parse_fields(&mut self) -> Result<Vec<Field>, Error> {
        let mut fields = Vec::new();
        
        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            fields.push(self.parse_field()?);
            if !self.match_token(&TokenKind::Semicolon) {
                break;
            }
        }
        
        Ok(fields)
    }
    
    /// 解析字段
    fn parse_field(&mut self) -> Result<Field, Error> {
        let name = self.expect_identifier()?;
        let type_node = Box::new(self.parse_type()?);
        
        let constraint = if self.match_token(&TokenKind::Colon) {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };
        
        Ok(Field {
            name,
            type_node,
            constraint,
        })
    }
    
    /// 解析方法列表
    fn parse_methods(&mut self) -> Result<Vec<Method>, Error> {
        let mut methods = Vec::new();
        
        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            methods.push(self.parse_method()?);
            if !self.match_token(&TokenKind::Semicolon) {
                break;
            }
        }
        
        Ok(methods)
    }
    
    /// 解析方法
    fn parse_method(&mut self) -> Result<Method, Error> {
        let name = self.expect_identifier()?;
        self.expect(TokenKind::LeftParen)?;
        let params = self.parse_type_list()?;
        self.expect(TokenKind::RightParen)?;
        
        let return_type = if self.check(&TokenKind::LeftParen) || self.check(&TokenKind::Identifier) || self.check(&TokenKind::LeftBracket) || self.check(&TokenKind::Star) {
            Some(Box::new(self.parse_type()?))
        } else {
            None
        };
        
        Ok(Method {
            name,
            params,
            return_type,
        })
    }
    
    /// 解析类型列表
    fn parse_type_list(&mut self) -> Result<Vec<Box<AstNode>>, Error> {
        let mut types = Vec::new();
        
        if !self.check(&TokenKind::RightParen) {
            loop {
                types.push(Box::new(self.parse_type()?));
                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
        }
        
        Ok(types)
    }
    
    /// 解析所有权指令
    fn parse_ownership_directive(&mut self) -> Result<AstNode, Error> {
        let token = self.expect(TokenKind::OwnershipDirective)?;
        let parts: Vec<&str> = token.lexeme.split(':').collect();
        
        if parts.len() != 2 {
            return Err(self.error("Invalid ownership directive format"));
        }
        
        let mode = match parts[1] {
            "on" => OwnershipMode::On,
            "off" => OwnershipMode::Off,
            "auto" => OwnershipMode::Auto,
            _ => return Err(self.error("Invalid ownership mode"))
        };
        
        self.expect(TokenKind::Semicolon)?;
        Ok(AstNode::OwnershipDirective { mode })
    }
    
    /// 解析依赖类型指令
    fn parse_dependent_types_directive(&mut self) -> Result<AstNode, Error> {
        let token = self.expect(TokenKind::DependentTypesDirective)?;
        let parts: Vec<&str> = token.lexeme.split(':').collect();
        
        if parts.len() != 2 {
            return Err(self.error("Invalid dependent types directive format"));
        }
        
        let enabled = match parts[1] {
            "on" => true,
            "off" => false,
            _ => return Err(self.error("Invalid dependent types mode"))
        };
        
        self.expect(TokenKind::Semicolon)?;
        Ok(AstNode::DependentTypesDirective { enabled })
    }
    
    /// 解析所有权块指令
    fn parse_ownership_block_directive(&mut self) -> Result<AstNode, Error> {
        let token = self.expect(TokenKind::OwnershipBlockDirective)?;
        let parts: Vec<&str> = token.lexeme.split(|c| c == '(' || c == ')').collect();
        
        if parts.len() < 2 {
            return Err(self.error("Invalid ownership block directive format"));
        }
        
        let mode = match parts[1] {
            "on" => OwnershipMode::On,
            "off" => OwnershipMode::Off,
            "auto" => OwnershipMode::Auto,
            _ => return Err(self.error("Invalid ownership mode"))
        };
        
        let body = Box::new(self.parse_block_stmt()?);
        Ok(AstNode::OwnershipBlockDirective { mode, body })
    }
    
    /// 解析依赖类型块指令
    fn parse_dependent_types_block_directive(&mut self) -> Result<AstNode, Error> {
        let token = self.expect(TokenKind::DependentTypesBlockDirective)?;
        let parts: Vec<&str> = token.lexeme.split(|c| c == '(' || c == ')').collect();
        
        if parts.len() < 2 {
            return Err(self.error("Invalid dependent types block directive format"));
        }
        
        let enabled = match parts[1] {
            "on" => true,
            "off" => false,
            _ => return Err(self.error("Invalid dependent types mode"))
        };
        
        let body = Box::new(self.parse_block_stmt()?);
        Ok(AstNode::DependentTypesBlockDirective { enabled, body })
    }
    
    // --- Helper Methods ---
    
    /// 检查当前标记类型
    fn check(&self, kind: &TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }
        &self.peek().kind == kind
    }
    
    /// 检查当前标记是否是给定类型之一
    fn check_one_of(&self, kinds: &[TokenKind]) -> bool {
        if self.is_at_end() {
            return false;
        }
        kinds.contains(&self.peek().kind)
    }
    
    /// 匹配当前标记类型，如果匹配则前进
    fn match_token(&mut self, kind: &TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }
    
    /// 匹配当前标记是否是给定类型之一，如果匹配则前进
    fn match_one_of(&mut self, kinds: &[TokenKind]) -> bool {
        if self.check_one_of(kinds) {
            self.advance();
            true
        } else {
            false
        }
    }
    
    /// 期望当前标记类型，如果匹配则前进，否则返回错误
    fn expect(&mut self, kind: TokenKind) -> Result<&Token, Error> {
        if self.check(&kind) {
            Ok(self.advance())
        } else {
            Err(self.error(&format!("Expected {:?}, found {:?}", kind, self.peek().kind)))
        }
    }
    
    /// 期望当前标记是给定类型之一，如果匹配则前进，否则返回错误
    fn expect_one_of(&mut self, kinds: &[TokenKind]) -> Result<&Token, Error> {
        if self.check_one_of(kinds) {
            Ok(self.advance())
        } else {
            Err(self.error(&format!("Expected one of {:?}, found {:?}", kinds, self.peek().kind)))
        }
    }
    
    /// 期望当前标记是标识符，如果匹配则返回名称并前进，否则返回错误
    fn expect_identifier(&mut self) -> Result<String, Error> {
        let token = self.expect(TokenKind::Identifier)?;
        Ok(token.lexeme.clone())
    }
    
    /// 期望当前标记是字符串字面量，如果匹配则返回内容并前进，否则返回错误
    fn expect_string_literal(&mut self) -> Result<String, Error> {
        let token = self.expect(TokenKind::StringLiteral)?;
        // 去除首尾引号
        Ok(token.lexeme[1..token.lexeme.len() - 1].to_string())
    }
    
    /// 前进到下一个标记
    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }
    
    /// 获取当前标记
    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }
    
    /// 获取前一个标记
    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
    
    /// 检查是否到达末尾
    fn is_at_end(&self) -> bool {
        self.peek().kind == TokenKind::EOF
    }
    
    /// 创建错误
    fn error(&self, message: &str) -> Error {
        let position = if self.is_at_end() {
            self.previous().position.clone()
        } else {
            self.peek().position.clone()
        };
        Error::syntax(message, self.current_file.clone(), position.line, position.column)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    
    fn parse_code(code: &str) -> Result<AstNode, Error> {
        let mut lexer = Lexer::new(code, PathBuf::from("test.gx"));
        let tokens = lexer.tokenize()?;
        let mut parser = Parser::new(tokens, PathBuf::from("test.gx"));
        parser.parse()
    }
    
    #[test]
    fn test_parse_basic_program() {
        let code = "package main;\nimport \"fmt\";\nfunc main() {\n\tfmt.Println(\"Hello\");\n}\n";
        let result = parse_code(code);
        assert!(result.is_ok());
        
        if let Ok(AstNode::Program { package, imports, declarations }) = result {
            assert_eq!(package, "main");
            assert_eq!(imports.len(), 1);
            assert_eq!(imports[0].path, "fmt");
            assert_eq!(declarations.len(), 1);
            
            if let AstNode::FunctionDecl { name, .. } = &*declarations[0] {
                assert_eq!(name, "main");
            } else {
                panic!("Expected FunctionDecl");
            }
        } else {
            panic!("Expected Program");
        }
    }
    
    #[test]
    fn test_parse_var_decl() {
        let code = "package main;\nvar x int = 10;\nlet y = \"hello\";\n";
        let result = parse_code(code);
        assert!(result.is_ok());
        
        if let Ok(AstNode::Program { declarations, .. }) = result {
            assert_eq!(declarations.len(), 2);
            
            if let AstNode::VarDecl { is_let, name, type_annotation, initializer } = &*declarations[0] {
                assert!(!is_let);
                assert_eq!(name, "x");
                assert!(type_annotation.is_some());
                assert!(initializer.is_some());
            } else {
                panic!("Expected VarDecl");
            }
            
            if let AstNode::VarDecl { is_let, name, type_annotation, initializer } = &*declarations[1] {
                assert!(is_let);
                assert_eq!(name, "y");
                assert!(type_annotation.is_none());
                assert!(initializer.is_some());
            } else {
                panic!("Expected VarDecl");
            }
        } else {
            panic!("Expected Program");
        }
    }
    
    #[test]
    fn test_parse_array_type() {
        let code = "package main;\nvar arr [10]int;\nvar slice []string;\n";
        let result = parse_code(code);
        assert!(result.is_ok());
        
        if let Ok(AstNode::Program { declarations, .. }) = result {
            assert_eq!(declarations.len(), 2);
            
            if let AstNode::VarDecl { type_annotation: Some(type_node), .. } = &*declarations[0] {
                if let AstNode::ArrayType { element_type, size } = &**type_node {
                    assert!(size.is_some());
                    if let AstNode::IntLiteral { value } = &**size.as_ref().unwrap() {
                        assert_eq!(*value, 10);
                    } else {
                        panic!("Expected IntLiteral for size");
                    }
                    if let AstNode::BasicType { name } = &**element_type {
                        assert_eq!(name, "int");
                    } else {
                        panic!("Expected BasicType for element");
                    }
                } else {
                    panic!("Expected ArrayType");
                }
            } else {
                panic!("Expected VarDecl with type annotation");
            }
            
            if let AstNode::VarDecl { type_annotation: Some(type_node), .. } = &*declarations[1] {
                if let AstNode::SliceType { element_type } = &**type_node {
                    if let AstNode::BasicType { name } = &**element_type {
                        assert_eq!(name, "string");
                    } else {
                        panic!("Expected BasicType for element");
                    }
                } else {
                    panic!("Expected SliceType");
                }
            } else {
                panic!("Expected VarDecl with type annotation");
            }
        } else {
            panic!("Expected Program");
        }
    }
}
