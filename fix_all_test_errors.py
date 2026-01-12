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
    
    # 错误1: TypusFile defaultFileDirectives [] [] [] 后面有多余的 ]
    pattern1 = r'TypusFile defaultFileDirectives \[\] \[\] \[\]\]'
    replacement1 = 'TypusFile defaultFileDirectives [] [] []'
    
    # 错误2: parseTypus 函数可能不存在或名称不正确
    # 需要先检查这个函数是否存在
    
    # 错误3: all hasLocation errors 中 hasLocation 函数可能不适用于 CompilerError
    # 需要包装成 property
    pattern3 = r'Left errors -> all hasLocation errors'
    replacement3 = 'Left errors -> property (all (hasLocation . unLoc) errors)'
    
    # 错误4: null formatted 中 formatted 可能是 T.Text 类型
    pattern4 = r'null formatted'
    replacement4 = 'T.null formatted'
    
    # 错误5: TypeError 与 CompilerError 类型不匹配
    # 需要将 TypeError 转换为 CompilerError
    pattern5 = r'calculateErrorStatistics errors'
    replacement5 = 'calculateErrorStatistics (map TypeError errors)'
    
    pattern6 = r'filterErrorsBySeverity errors (\d+)'
    replacement6 = r'filterErrorsBySeverity (map TypeError errors) \1'
    
    pattern7 = r'sortErrorsByLocation errors'
    replacement7 = r'sortErrorsByLocation (map TypeError errors)'
    
    pattern8 = r'highlightErrorInSource source error'
    replacement8 = r'highlightErrorInSource source (TypeError error)'
    
    # 修复所有文件
    for file_path in test_files:
        print(f"处理文件: {file_path}")
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # 修复错误1
            old_content = content
            content = re.sub(pattern1, replacement1, content)
            if content != old_content:
                print(f"  修复了 TypusFile 构造函数错误")
            
            # 修复错误3
            old_content = content
            content = re.sub(pattern3, replacement3, content)
            if content != old_content:
                print(f"  修复了 hasLocation 类型错误")
            
            # 修复错误4
            old_content = content
            content = re.sub(pattern4, replacement4, content)
            if content != old_content:
                print(f"  修复了 null 函数类型错误")
            
            # 修复错误5
            old_content = content
            content = re.sub(pattern5, replacement5, content)
            if content != old_content:
                print(f"  修复了 calculateErrorStatistics 类型错误")
            
            # 修复错误6
            old_content = content
            content = re.sub(pattern6, replacement6, content)
            if content != old_content:
                print(f"  修复了 filterErrorsBySeverity 类型错误")
            
            # 修复错误7
            old_content = content
            content = re.sub(pattern7, replacement7, content)
            if content != old_content:
                print(f"  修复了 sortErrorsByLocation 类型错误")
            
            # 修复错误8
            old_content = content
            content = re.sub(pattern8, replacement8, content)
            if content != old_content:
                print(f"  修复了 highlightErrorInSource 类型错误")
            
            # 写回文件
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(content)
                
        except Exception as e:
            print(f"  错误: {e}")

if __name__ == "__main__":
    fix_all_test_errors()
    print("修复完成!")