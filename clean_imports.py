#!/usr/bin/env python3
"""
清理未使用的导入
"""

import re
import os

def clean_imports():
    """清理未使用的导入"""
    
    # 要处理的文件列表
    files_to_clean = [
        "test/Test/Unit/UtilsAdvancedQuickCheckSpec.hs",
        "test/Test/Unit/ParserAdvancedQuickCheckSpec.hs",
        "test/Test/Unit/SourceLocationAdvancedQuickCheckSpec.hs"
    ]
    
    for file_path in files_to_clean:
        if not os.path.exists(file_path):
            continue
            
        with open(file_path, 'r') as f:
            content = f.read()
        
        # 移除未使用的导入
        content = re.sub(r'import Test\.Tasty\.HUnit.*?\n', '', content)
        content = re.sub(r'import Utils.*?\n', '', content)
        content = re.sub(r'import Data\.Char.*?\n', '', content)
        content = re.sub(r'import Data\.List.*?\n', '', content)
        content = re.sub(r'import qualified Data\.Text.*?\n', '', content)
        content = re.sub(r'import Parser.*?\n', '', content)
        content = re.sub(r'import qualified.*?\n', '', content)
        
        # 修复变量名冲突
        content = re.sub(r'lines <-', 'testLines <-', content)
        
        with open(file_path, 'w') as f:
            f.write(content)
        print(f"Cleaned {file_path}")

if __name__ == "__main__":
    os.chdir("/home/runner/work/Typus/Typus")
    
    clean_imports()
    
    print("All imports cleaned!")