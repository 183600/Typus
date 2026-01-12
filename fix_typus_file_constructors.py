#!/usr/bin/env python3
"""
修复测试文件中的TypusFile构造函数调用
"""

import os
import re
import glob

def fix_typus_file_constructors():
    """修复测试文件中的TypusFile构造函数调用"""
    
    # 查找所有测试文件
    test_files = []
    for root, dirs, files in os.walk('.'):
        for file in files:
            if file.endswith('.hs') and 'Test' in root:
                test_files.append(os.path.join(root, file))
    
    # 需要修复的模式
    # TypusFile [] [] [] [] ... （大量空列表）
    # 替换为：
    # TypusFile defaultFileDirectives [] [] []
    
    pattern = r'TypusFile\s*(\[\]\s*){10,}'  # 匹配10个或更多空列表
    
    for file_path in test_files:
        print(f"处理文件: {file_path}")
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # 查找所有匹配的模式
            matches = re.finditer(pattern, content)
            for match in matches:
                old_text = match.group(0)
                new_text = "TypusFile defaultFileDirectives [] [] []"
                print(f"  替换: {old_text[:50]}... -> {new_text}")
                content = content.replace(old_text, new_text)
            
            # 写回文件
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(content)
                
        except Exception as e:
            print(f"  错误: {e}")

if __name__ == "__main__":
    fix_typus_file_constructors()
    print("修复完成!")