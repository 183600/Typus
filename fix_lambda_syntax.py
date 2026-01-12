#!/usr/bin/env python3
"""
修复Lambda表达式语法错误
"""

import os
import re
import glob

def fix_lambda_syntax():
    """修复Lambda表达式语法错误"""
    
    # 查找所有测试文件
    test_files = []
    for root, dirs, files in os.walk('.'):
        for file in files:
            if file.endswith('.hs') and 'Test' in root:
                test_files.append(os.path.join(root, file))
    
    # 修复所有文件
    for file_path in test_files:
        print(f"处理文件: {file_path}")
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            old_content = content
            
            # 错误1: 修复Lambda表达式语法
            # 将 \(Compiler.Errors.Core.TypeError -> Compiler.TypeChecker.TypeError) 改为 (\err -> case err of Compiler.Errors.Core.TypeError -> Compiler.TypeChecker.TypeError)
            pattern1 = r'\\\(Compiler\.Errors\.Core\.TypeError -> Compiler\.TypeChecker\.TypeError\)'
            replacement1 = '(\\err -> case err of Compiler.Errors.Core.TypeError -> Compiler.TypeChecker.TypeError)'
            content = re.sub(pattern1, replacement1, content)
            
            # 如果内容有变化，写回文件
            if content != old_content:
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(content)
                
        except Exception as e:
            print(f"  错误: {e}")

if __name__ == "__main__":
    fix_lambda_syntax()
    print("修复完成!")