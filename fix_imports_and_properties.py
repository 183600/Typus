#!/usr/bin/env python3
"""
修复导入语句和属性定义的缩进问题
"""

import os
import re

# 需要修复的文件列表
files_to_fix = []
test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
for filename in os.listdir(test_dir):
    if filename.endswith(".hs"):
        files_to_fix.append(os.path.join(test_dir, filename))

def fix_import_and_property_indentation(content):
    """修复导入语句和属性定义的缩进问题"""
    lines = content.split('\n')
    fixed_lines = []
    
    for line in lines:
        # 修复导入语句的缩进
        if line.strip().startswith('import ') and not line.startswith('import '):
            # 移除前面的空格
            line = 'import ' + line.strip()[7:]
        
        # 修复属性定义的缩进
        if line.strip().startswith('prop_') and '::' in line and not line.startswith('  '):
            # 添加适当的缩进
            line = '  ' + line.strip()
        
        fixed_lines.append(line)
    
    return '\n'.join(fixed_lines)

def main():
    for file_path in files_to_fix:
        if os.path.exists(file_path):
            with open(file_path, 'r') as f:
                content = f.read()
            
            # 应用修复
            content = fix_import_and_property_indentation(content)
            
            with open(file_path, 'w') as f:
                f.write(content)
            
            print(f"Fixed {file_path}")
        else:
            print(f"File not found: {file_path}")

if __name__ == "__main__":
    main()