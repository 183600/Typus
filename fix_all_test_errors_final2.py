#!/usr/bin/env python3
"""
修复测试文件中的所有错误
"""

import os
import re
import glob

def fix_all_test_errors():
    """修复测试文件中的所有错误"""
    
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
            
            # 错误1: 修复 message 函数与 Text 类型不匹配
            # 将 message (ceError error) 替换为 T.unpack (message (ceError error))
            pattern1 = r'message \(ceError error\)'
            replacement1 = 'T.unpack (message (ceError error))'
            content = re.sub(pattern1, replacement1, content)
            
            # 错误2: 修复 Compiler.TypeChecker.Type 未导入
            # 将 Compiler.TypeChecker.Type 替换为 Type
            pattern2 = r'Compiler\.TypeChecker\.Type'
            replacement2 = 'Type'
            content = re.sub(pattern2, replacement2, content)
            
            # 错误3: 修复 parse error on input '='
            # 查找可能缺少的 do 关键字
            pattern3 = r'(\s+)result = case parseTypus \(T\.pack code\) of'
            replacement3 = r'\1result = case parseTypus (T.pack code) of'
            content = re.sub(pattern3, replacement3, content)
            
            # 错误4: 修复 calculateErrorStatistics 模式匹配错误
            # 将 calculateErrorStatistics (map TypeError errors) = 替换为正确的形式
            pattern4 = r'calculateErrorStatistics \(map TypeError errors\) ='
            replacement4 = 'calculateErrorStatistics errors ='
            content = re.sub(pattern4, replacement4, content)
            
            # 错误5: 修复 SyntaxError 和 Located 构造函数不在作用域
            # 将 SyntaxError (Located (SourcePos "" 0 0)) parseErr 替换为简单的字符串
            pattern5 = r'SyntaxError \(Located \(SourcePos "" 0 0\)\) parseErr'
            replacement5 = 'CompilerError (Located (SourcePos "" 0 0)) parseErr TypeErrorSeverity'
            content = re.sub(pattern5, replacement5, content)
            
            # 如果内容有变化，写回文件
            if content != old_content:
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(content)
                
        except Exception as e:
            print(f"  错误: {e}")

if __name__ == "__main__":
    fix_all_test_errors()
    print("修复完成!")