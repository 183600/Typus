#!/usr/bin/env python3
"""
最终修复所有导入错误
"""

import re
import os

def fix_all_imports():
    """修复所有导入错误"""
    
    # 修复 ErrorHandlerAdvancedQuickCheckSpec.hs
    file_path = "test/Test/Unit/ErrorHandlerAdvancedQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # 移除所有错误导入
    content = re.sub(r'\s*, errorWithSuggestions', '', content)
    content = re.sub(r'\s*, infoWithCategory', '', content)
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed {file_path}")
    
    # 修复 ParserAdvancedQuickCheckSpec.hs
    file_path = "test/Test/Unit/ParserAdvancedQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # 移除错误导入
    content = re.sub(r'import Test\.Tasty\.QuickCheck.*?\n', 'import Test.Tasty.QuickCheck\n', content, flags=re.DOTALL)
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed {file_path}")
    
    # 修复 UtilsAdvancedQuickCheckSpec.hs
    file_path = "test/Test/Unit/UtilsAdvancedQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # 移除错误导入
    content = re.sub(r'import Test\.Tasty\.QuickCheck.*?\n', 'import Test.Tasty.QuickCheck\n', content, flags=re.DOTALL)
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed {file_path}")
    
    # 修复 SourceLocationAdvancedQuickCheckSpec.hs
    file_path = "test/Test/Unit/SourceLocationAdvancedQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # 移除未使用的导入
    content = re.sub(r'import SourceLocation.*?\n', '', content)
    content = re.sub(r'import Compiler\.Errors\.Core.*?\n', '', content)
    content = re.sub(r'import Control\.DeepSeq.*?\n', '', content)
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed {file_path}")

if __name__ == "__main__":
    os.chdir("/home/runner/work/Typus/Typus")
    
    fix_all_imports()
    
    print("All imports fixed!")