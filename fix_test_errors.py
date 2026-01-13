#!/usr/bin/env python3
"""
修复测试文件中的常见错误
"""

import re
import os

def fix_compiler_advanced_spec():
    """修复 CompilerAdvancedQuickCheckSpec.hs 中的错误"""
    file_path = "test/Test/Unit/CompilerAdvancedQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # 修复 IR.GoModule 问题
    content = re.sub(
        r'IR\.GoModule \(IR\.ensurePackageDecl "main"\) \[\] \[\] \[\]',
        'IR.GoModule (IR.PackageDecl "main" Nothing) [] [] []',
        content
    )
    
    # 修复 ErrorLocation 构造函数问题
    content = re.sub(
        r'ErrorLocation _ line _ _ _ -> line >= 0',
        '''case loc of
        ErrorLocation _ line _ _ _ -> property $ line >= 0
        _ -> property True''',
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed {file_path}")

def fix_dependencies_advanced_spec():
    """修复 DependenciesAdvancedQuickCheckSpec.hs 中的错误"""
    file_path = "test/Test/Unit/DependenciesAdvancedQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # 修复 newTypeVariable 调用
    content = re.sub(
        r'newTypeVariable "test"',
        'runTypeInference newTypeVariable',
        content
    )
    
    # 修复 String 到 Text 的转换
    content = re.sub(r'SimpleT "([^"]+)"', r'SimpleT (T.pack "\1")', content)
    content = re.sub(r'SVarDecl "([^"]+)"', r'SVarDecl (T.pack "\1")', content)
    content = re.sub(r'SizeGT "([^"]+)"', r'SizeGT (T.pack "\1")', content)
    content = re.sub(r'SizeGE "([^"]+)"', r'SizeGE (T.pack "\1")', content)
    
    # 修复 generalize 函数调用
    content = re.sub(
        r'generalize env typeExpr',
        'runTypeInference (generalize typeExpr)',
        content
    )
    
    # 修复 instantiate 函数调用
    content = re.sub(
        r'instantiate scheme',
        'runTypeInference (instantiate scheme)',
        content
    )
    
    # 修复 sortBy 导入
    if "import Data.List (sortBy)" not in content:
        content = re.sub(
            r'(import Data\.List \(.*?\))',
            r'\1\nimport Data.List (sortBy)',
            content
        )
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed {file_path}")

def fix_error_handler_advanced_spec():
    """修复 ErrorHandlerAdvancedQuickCheckSpec.hs 中的错误"""
    file_path = "test/Test/Unit/ErrorHandlerAdvancedQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # 修复导入语法错误
    content = re.sub(
        r'import Dependencies\.TypeSystem \(DependentTypeError\(\.\.\)\)\s*\n\s*, emptyContext',
        '''import Dependencies.TypeSystem (DependentTypeError(..))
import Compiler.Errors.Core (emptyContext''',
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed {file_path}")

def fix_ownership_advanced_spec():
    """修复 OwnershipAdvancedQuickCheckSpec.hs 中的错误"""
    file_path = "test/Test/Unit/OwnershipAdvancedQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # 修复 Property 类型不匹配
    content = re.sub(
        r'in ordering === EQ \|\| ordering === LT \|\| ordering === GT',
        'in property $ ordering `elem` [EQ, LT, GT]',
        content
    )
    
    # 修复 analyzeOwnership 调用
    content = re.sub(
        r'analyzeOwnership analyzer code',
        'analyzeOwnership code',
        content
    )
    
    # 修复 analyzeOwnershipDebug 调用
    content = re.sub(
        r'analyzeOwnershipDebug analyzer code',
        'analyzeOwnershipDebug code',
        content
    )
    
    # 修复 analyzeOwnershipFile 调用
    content = re.sub(
        r'analyzeOwnershipFile analyzer filePath',
        'analyzeOwnershipFile filePath',
        content
    )
    
    # 修复 Property 类型不匹配
    content = re.sub(
        r'in length tokens >= 0',
        'in property $ length tokens >= 0',
        content
    )
    
    content = re.sub(
        r'in null tokens',
        'in property $ null tokens',
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed {file_path}")

if __name__ == "__main__":
    os.chdir("/home/runner/work/Typus/Typus")
    
    fix_compiler_advanced_spec()
    fix_dependencies_advanced_spec()
    fix_error_handler_advanced_spec()
    fix_ownership_advanced_spec()
    
    print("All fixes applied!")