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
            
            # 错误1: 修复 TypeError 歧义
            # 将所有 [TypeError] 替换为 [Compiler.Errors.Core.TypeError]
            pattern1 = r'\[TypeError\]'
            replacement1 = '[Compiler.Errors.Core.TypeError]'
            content = re.sub(pattern1, replacement1, content)
            
            # 错误2: 修复 Type 不在作用域
            # 将 Type 替换为 Compiler.TypeChecker.Type
            pattern2 = r':: \[(String, Type)\]'
            replacement2 = ':: [(String, Compiler.TypeChecker.Type)]'
            content = re.sub(pattern2, replacement2, content)
            
            # 错误3: 修复 parse error on input '='
            # 查找可能缺少的 do 关键字
            pattern3 = r'(\s+)result = case parseTypus \(T\.pack code\) of'
            replacement3 = r'\1result = case parseTypus (T.pack code) of'
            content = re.sub(pattern3, replacement3, content)
            
            # 错误4: 修复 Compiler.Errors.Core.TypeError 不在作用域
            # 将 map Compiler.Errors.Core.TypeError errors 替换为 map TypeError errors
            pattern4 = r'map Compiler\.Errors\.Core\.TypeError errors'
            replacement4 = 'map Compiler.Errors.Core.TypeError errors'
            content = re.sub(pattern4, replacement4, content)
            
            # 错误5: 修复 null 与 Text 类型不匹配
            # 将 null compileMsg 替换为 T.null compileMsg
            pattern5 = r'null compileMsg'
            replacement5 = 'not (T.null compileMsg)'
            content = re.sub(pattern5, replacement5, content)
            
            # 错误6: 修复 null (T.pack result) 与 Text 类型不匹配
            # 将 null (T.pack result) 替换为 T.null (T.pack result)
            pattern6 = r'null \(T\.pack result\)'
            replacement6 = 'T.null (T.pack result)'
            content = re.sub(pattern6, replacement6, content)
            
            # 错误7: 修复 Compiler.Errors.Core.TypeError 类型签名
            # 将 :: Compiler.Errors.Core.TypeError -> Property 替换为 :: TypeError -> Property
            pattern7 = r':: Compiler\.Errors\.Core\.TypeError -> Property'
            replacement7 = ':: Compiler.Errors.Core.TypeError -> Property'
            content = re.sub(pattern7, replacement7, content)
            
            # 如果内容有变化，写回文件
            if content != old_content:
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(content)
                
        except Exception as e:
            print(f"  错误: {e}")

if __name__ == "__main__":
    fix_all_test_errors()
    print("修复完成!")