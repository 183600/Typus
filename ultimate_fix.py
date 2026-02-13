#!/usr/bin/env python3
"""
全面修复所有函数中的语法错误和缩进问题
"""

import re

# 读取文件内容
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'r') as f:
    lines = f.readlines()

# 修复后的行
fixed_lines = []
i = 0
while i < len(lines):
    line = lines[i]
    
    # 检查是否是编译器测试函数中的 "in case compiled of" 行
    if 'in case compiled of' in line:
        # 添加这一行
        fixed_lines.append(line)
        i += 1
        
        # 添加 Right goCode -> 行
        while i < len(lines) and 'Right goCode ->' not in lines[i]:
            fixed_lines.append(lines[i])
            i += 1
        
        if i < len(lines):
            # 修复 Right goCode -> 行的缩进
            indent = len(line) - len(line.lstrip())
            fixed_lines.append(' ' * (indent + 5) + 'Right goCode -> property $ s `isInfixOf` goCode\n')
            i += 1
            
            # 添加 Left _ -> 行
            while i < len(lines) and 'Left _ -> property False' not in lines[i]:
                fixed_lines.append(lines[i])
                i += 1
            
            if i < len(lines):
                fixed_lines.append(' ' * (indent + 5) + 'Left _ -> property False\n')
                i += 1
                
                # 添加外层 Left _ -> 行
                fixed_lines.append(' ' * (indent - 2) + 'Left _ -> property True\n')
                
                # 添加 else property True 行（如果需要）
                # 检查是否在 if-then-else 结构中
                j = i - 10
                in_if = False
                while j >= 0 and j < i:
                    if 'if all isLetter' in lines[j]:
                        in_if = True
                        break
                    j += 1
                
                if in_if:
                    fixed_lines.append(' ' * (indent - 6) + 'else property True\n')
    else:
        fixed_lines.append(line)
    
    i += 1

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'w') as f:
    f.writelines(fixed_lines)

print("全面修复语法错误和缩进问题完成")