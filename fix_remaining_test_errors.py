#!/usr/bin/env python3
"""
修复剩余的测试文件错误
"""

import os
import re
import glob

def fix_remaining_test_errors():
    """修复剩余的测试文件错误"""
    
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
            
            # 错误1: 修复 parse error on input ')' 
            # 问题在于缺少一个右括号
            pattern1 = r'createTypusFileFromErrors \(map \(\\\(Compiler\.Errors\.Core\.TypeError -> Compiler\.TypeChecker\.TypeError\) errors\)'
            replacement1 = 'createTypusFileFromErrors (map (\\(Compiler.Errors.Core.TypeError -> Compiler.TypeChecker.TypeError)) errors)'
            content = re.sub(pattern1, replacement1, content)
            
            # 错误2: 修复 parse error on input ')' 在 calculateErrorStatistics 中
            pattern2 = r'calculateErrorStatistics \(map \(\\\(Compiler\.Errors\.Core\.TypeError -> Compiler\.TypeChecker\.TypeError\) errors\)'
            replacement2 = 'calculateErrorStatistics (map (\\(Compiler.Errors.Core.TypeError -> Compiler.TypeChecker.TypeError)) errors)'
            content = re.sub(pattern2, replacement2, content)
            
            # 错误3: 修复 parse error on input '='
            # 这可能是在 do 块中缺少 let 关键字
            pattern3 = r'(\s+)result = case parseTypus \(T\.pack code\) of'
            replacement3 = r'\1let result = case parseTypus (T.pack code) of'
            content = re.sub(pattern3, replacement3, content)
            
            # 错误4: 修复 Type 不在作用域
            pattern4 = r':: \[(String, Type)\]'
            replacement4 = ':: [(String, Compiler.TypeChecker.Type)]'
            content = re.sub(pattern4, replacement4, content)
            
            # 如果内容有变化，写回文件
            if content != old_content:
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(content)
                
        except Exception as e:
            print(f"  错误: {e}")

if __name__ == "__main__":
    fix_remaining_test_errors()
    print("修复完成!")