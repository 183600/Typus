use std::path::PathBuf;

/// 位置信息
#[derive(Debug, Clone, PartialEq)]
pub struct Position {
    /// 行号
    pub line: usize,
    /// 列号
    pub column: usize,
}

/// 标记
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// 标记类型
    pub kind: TokenKind,
    /// 词素
    pub lexeme: String,
    /// 位置
    pub position: Position,
}

/// 标记类型
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // 单字符标记
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Comma,        // ,
    Dot,          // .
    Colon,        // :
    Semicolon,    // ;
    
    // 一元运算符
    Bang,         // !
    
    // 二元运算符
    Plus,         // +
    Minus,        // -
    Star,         // *
    Slash,        // /
    Percent,      // %
    Equal,        // =
    Less,         // <
    Greater,      // >
    And,          // &
    Or,           // |
    
    // 复合运算符
    PlusEqual,    // +=
    MinusEqual,   // -=
    StarEqual,    // *=
    SlashEqual,   // /=
    PercentEqual, // %=
    EqualEqual,   // ==
    BangEqual,    // !=
    LessEqual,    // <=
    GreaterEqual, // >=
    AndAnd,       // &&
    OrOr,         // ||
    Arrow,        // ->
    
    // 字面量
    Identifier,    // 标识符
    IntLiteral,    // 整数字面量
    FloatLiteral,  // 浮点数字面量
    StringLiteral, // 字符串字面量
    BoolLiteral,   // 布尔字面量
    NilLiteral,    // nil 字面量
    
    // 关键字
    Package,    // package
    Import,     // import
    Func,       // func
    Return,     // return
    Var,        // var
    Const,      // const
    Let,        // let
    Type,       // type
    Struct,     // struct
    Interface,  // interface
    Map,        // map
    If,         // if
    Else,       // else
    For,        // for
    Break,      // break
    Continue,   // continue
    Switch,     // switch
    Case,       // case
    Default,    // default
    
    // 指令
    OwnershipDirective,          // #ownership:on/off/auto
    DependentTypesDirective,     // #dependent_types:on/off
    OwnershipBlockDirective,     // #ownership_block(on/off/auto)
    DependentTypesBlockDirective, // #dependent_types_block(on/off)
    
    // 其他
    EOF, // 文件结束
}

/// 导入
#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    /// 路径
    pub path: String,
    /// 别名
    pub alias: Option<String>,
}

/// 类型参数
#[derive(Debug, Clone, PartialEq)]
pub struct TypeParam {
    /// 名称
    pub name: String,
    /// 类型
    pub kind: Option<Box<AstNode>>,
    /// 约束
    pub constraint: Option<Box<AstNode>>,
}

/// 参数
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    /// 名称
    pub name: String,
    /// 类型
    pub type_node: Box<AstNode>,
    /// 约束
    pub constraint: Option<Box<AstNode>>,
    /// 是否可变
    pub is_mutable: bool,
}

/// 字段
#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    /// 名称
    pub name: String,
    /// 类型
    pub type_node: Box<AstNode>,
    /// 约束
    pub constraint: Option<Box<AstNode>>,
}

/// 方法
#[derive(Debug, Clone, PartialEq)]
pub struct Method {
    /// 名称
    pub name: String,
    /// 参数
    pub params: Vec<Box<AstNode>>,
    /// 返回类型
    pub return_type: Option<Box<AstNode>>,
}

/// 所有权模式
#[derive(Debug, Clone, PartialEq)]
pub enum OwnershipMode {
    /// 开启
    On,
    /// 关闭
    Off,
    /// 自动
    Auto,
}

/// 抽象语法树节点
#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    /// 程序
    Program {
        package: String,
        imports: Vec<Import>,
        declarations: Vec<Box<AstNode>>,
    },
    
    /// 函数声明
    FunctionDecl {
        name: String,
        type_params: Vec<TypeParam>,
        params: Vec<Parameter>,
        return_type: Option<Box<AstNode>>,
        body: Box<AstNode>,
    },
    
    /// 类型声明
    TypeDecl {
        name: String,
        type_params: Vec<TypeParam>,
        underlying_type: Box<AstNode>,
        constraints: Option<Box<AstNode>>,
    },
    
    /// 变量声明
    VarDecl {
        is_let: bool,
        name: String,
        type_annotation: Option<Box<AstNode>>,
        initializer: Option<Box<AstNode>>,
    },
    
    /// 表达式语句
    ExprStmt {
        expr: Box<AstNode>,
    },
    
    /// 二元表达式
    BinaryExpr {
        left: Box<AstNode>,
        operator: TokenKind,
        right: Box<AstNode>,
    },
    
    /// 一元表达式
    UnaryExpr {
        operator: TokenKind,
        operand: Box<AstNode>,
    },
    
    /// 调用表达式
    CallExpr {
        callee: Box<AstNode>,
        args: Vec<Box<AstNode>>,
    },
    
    /// 索引表达式
    IndexExpr {
        object: Box<AstNode>,
        index: Box<AstNode>,
    },
    
    /// 字段访问表达式
    FieldExpr {
        object: Box<AstNode>,
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
        statements: Vec<Box<AstNode>>,
    },
    
    /// 条件语句
    IfStmt {
        condition: Box<AstNode>,
        then_branch: Box<AstNode>,
        else_branch: Option<Box<AstNode>>,
    },
    
    /// 循环语句
    ForStmt {
        init: Option<Box<AstNode>>,
        condition: Option<Box<AstNode>>,
        update: Option<Box<AstNode>>,
        body: Box<AstNode>,
    },
    
    /// 返回语句
    ReturnStmt {
        value: Option<Box<AstNode>>,
    },
    
    /// 所有权指令
    OwnershipDirective {
        mode: OwnershipMode,
    },
    
    /// 依赖类型指令
    DependentTypesDirective {
        enabled: bool,
    },
    
    /// 所有权块指令
    OwnershipBlockDirective {
        mode: OwnershipMode,
        body: Box<AstNode>,
    },
    
    /// 依赖类型块指令
    DependentTypesBlockDirective {
        enabled: bool,
        body: Box<AstNode>,
    },
    
    /// 类型
    BasicType {
        name: String,
    },
    ArrayType {
        element_type: Box<AstNode>,
        size: Option<Box<AstNode>>,
    },
    SliceType {
        element_type: Box<AstNode>,
    },
    MapType {
        key_type: Box<AstNode>,
        value_type: Box<AstNode>,
    },
    StructType {
        fields: Vec<Field>,
    },
    InterfaceType {
        methods: Vec<Method>,
    },
    FunctionType {
        params: Vec<Box<AstNode>>,
        return_type: Option<Box<AstNode>>,
    },
    PointerType {
        base_type: Box<AstNode>,
        is_mutable: bool,
    },
    DependentType {
        name: String,
        type_params: Vec<TypeParam>,
        underlying_type: Box<AstNode>,
        constraints: Option<Box<AstNode>>,
    },
}
