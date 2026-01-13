#!/usr/bin/env python3
"""
修复所有测试文件中的编译错误
"""

import os
import re

def fix_compiler_advanced_spec():
    """修复CompilerAdvancedQuickCheckSpec.hs中的错误"""
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/CompilerAdvancedQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # 添加缺失的导入
    if 'import Compiler.GoAst' not in content:
        content = content.replace('import Parser (TypusFile(..), defaultFileDirectives)', 
                                'import Parser (TypusFile(..), defaultFileDirectives)\nimport Compiler.GoAst (GoModule(..))\nimport Compiler.TypeChecker (Type(..), BasicType(..))')
    
    # 修复GoModule构造
    content = content.replace('buildTypeEnv (Compiler.GoAst.GoModule [] [] [] [])', 
                             'buildTypeEnv (GoModule [] [] [] [])')
    
    # 修复BasicType构造
    content = content.replace('Compiler.TypeChecker.BasicType name', 
                             'BasicType name')
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"修复了 {file_path}")

def fix_dependencies_advanced_spec():
    """修复DependenciesAdvancedQuickCheckSpec.hs中的错误"""
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/DependenciesAdvancedQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # 修复反引号错误
    content = content.replace('\\`elem\\`', '`elem`')
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"修复了 {file_path}")

def fix_error_handler_advanced_spec():
    """修复ErrorHandlerAdvancedQuickCheckSpec.hs中的错误"""
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/ErrorHandlerAdvancedQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # 添加缺失的导入
    if 'import Ownership.Common.Types' not in content:
        content = content.replace('import Data.Char (isSpace, isAlphaNum)', 
                                'import Data.Char (isSpace, isAlphaNum)\nimport Ownership.Common.Types (OwnershipError(..))\nimport Dependencies.TypeSystem (DependentTypeError(..))')
    
    # 修复OwnershipError构造
    content = content.replace('OwnershipErrorCombined severity errMsg', 
                             'OwnershipErrorCombined severity (OwnershipError errMsg)')
    
    # 修复DependentTypeError构造
    content = content.replace('DependentTypeErrorCombined severity errMsg', 
                             'DependentTypeErrorCombined severity (DependentTypeError errMsg)')
    
    # 修复Property类型错误
    content = content.replace('isAtLeast sev sev', 
                             'property (isAtLeast sev sev)')
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"修复了 {file_path}")

if __name__ == "__main__":
    fix_compiler_advanced_spec()
    fix_dependencies_advanced_spec()
    fix_error_handler_advanced_spec()
    print("所有修复完成!")