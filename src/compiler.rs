use std::path::{Path, PathBuf};
use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::sync::{Arc, Mutex};
use rayon::prelude::*;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::type_checker::TypeChecker;
use crate::ownership_checker::OwnershipChecker;
use crate::ir_generator::IRGenerator;
use crate::code_generator::CodeGenerator;
use crate::symbol_table::SymbolTable;
use crate::error::Error;
use crate::source_manager::SourceManager;

/// 编译器
pub struct Compiler {
    /// 源代码管理器
    source_manager: SourceManager,
}

impl Compiler {
    /// 创建新的编译器
    pub fn new() -> Self {
        Compiler {
            source_manager: SourceManager::new(),
        }
    }
    
    /// 编译单个文件
    pub fn compile_file(&self, source_manager: &SourceManager, path: &Path) -> Result<(), Error> {
        // 读取源文件
        let source = source_manager.read_file(path)?;
        
        // 创建词法分析器
        let mut lexer = Lexer::new(&source, path.to_path_buf());
        
        // 进行词法分析
        let tokens = lexer.tokenize()?;
        
        // 创建语法分析器
        let mut parser = Parser::new(tokens, path.to_path_buf());
        
        // 进行语法分析
        let ast = parser.parse()?;
        
        // 创建符号表
        let symbol_table = SymbolTable::new();
        
        // 创建类型检查器
        let mut type_checker = TypeChecker::new(symbol_table.clone());
        type_checker.set_current_file(path.to_path_buf());
        
        // 进行类型检查
        type_checker.check(&ast)?;
        
        // 创建所有权检查器
        let mut ownership_checker = OwnershipChecker::new(symbol_table.clone());
        ownership_checker.set_current_file(path.to_path_buf());
        
        // 进行所有权检查
        ownership_checker.check(&ast)?;
        
        // 创建中间表示生成器
        let mut ir_generator = IRGenerator::new(symbol_table);
        ir_generator.set_current_file(path.to_path_buf());
        
        // 生成中间表示
        let ir = ir_generator.generate(&ast)?;
        
        // 创建代码生成器
        let mut code_generator = CodeGenerator::new();
        code_generator.set_current_file(path.to_path_buf());
        
        // 生成代码
        let code = code_generator.generate(&ir)?;
        
        // 获取输出路径
        let output_path = source_manager.get_output_path(path)?;
        
        // 创建输出目录
        if let Some(parent) = output_path.parent() {
            fs::create_dir_all(parent)?;
        }
        
        // 写入输出文件
        let mut file = File::create(output_path)?;
        file.write_all(code.as_bytes())?;
        
        Ok(())
    }
    
    /// 批量编译
    pub fn batch_compile(&self, input_path: &Path, output_path: &Path) -> Result<(), Error> {
        // 设置输出路径
        self.source_manager.set_output_dir(output_path.to_path_buf());
        
        // 批量转换
        let source_manager = self.source_manager.clone();
        let report = source_manager.batch_convert(move |sm, path| {
            self.compile_file(sm, path)
        })?;
        
        // 打印报告
        println!("Compilation report: {:?}", report);
        
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;
    
    #[test]
    fn test_compiler_basic() {
        // 创建临时目录
        let temp_dir = tempdir().unwrap();
        let input_path = temp_dir.path().join("input.gx");
        let output_path = temp_dir.path().join("output");
        
        // 创建测试文件
        let mut file = File::create(&input_path).unwrap();
        file.write_all(b"package main\n\nfunc main() {\n\tprintln(\"Hello, World!\")\n}\n").unwrap();
        
        // 创建编译器
        let compiler = Compiler::new();
        
        // 编译文件
        let result = compiler.batch_compile(&input_path, &output_path);
        
        // 检查结果
        assert!(result.is_ok());
        
        // 检查输出文件
        let output_file = output_path.join("input.go");
        assert!(output_file.exists());
        
        // 读取输出文件
        let mut output = String::new();
        File::open(output_file).unwrap().read_to_string(&mut output).unwrap();
        
        // 检查输出内容
        assert!(output.contains("package main"));
        assert!(output.contains("func main()"));
        assert!(output.contains("println(\"Hello, World!\")"));
    }
}
