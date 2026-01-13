#!/usr/bin/env python3
"""
修复重复声明问题
"""

import re
import os

def fix_duplicate_declarations():
    """修复重复声明"""
    
    file_path = "test/Test/Unit/OwnershipAdvancedQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # 移除重复的函数声明
    lines = content.split('\n')
    seen_functions = set()
    new_lines = []
    
    i = 0
    while i < len(lines):
        line = lines[i]
        # 检查是否是函数声明行
        if re.match(r'^prop_\w+ ::', line):
            func_name = re.match(r'^prop_\w+', line).group()
            if func_name not in seen_functions:
                seen_functions.add(func_name)
                new_lines.append(line)
                i += 1
                # 添加函数体直到下一个函数或空行
                while i < len(lines) and not (re.match(r'^prop_\w+ ::', lines[i]) or lines[i].strip() == ''):
                    new_lines.append(lines[i])
                    i += 1
                continue
            else:
                # 跳过重复的函数
                i += 1
                while i < len(lines) and not (re.match(r'^prop_\w+ ::', lines[i]) or lines[i].strip() == ''):
                    i += 1
                continue
        else:
            new_lines.append(line)
            i += 1
    
    with open(file_path, 'w') as f:
        f.write('\n'.join(new_lines))
    print(f"Fixed {file_path}")

if __name__ == "__main__":
    os.chdir("/home/runner/work/Typus/Typus")
    
    fix_duplicate_declarations()
    
    print("Fixed duplicate declarations!")