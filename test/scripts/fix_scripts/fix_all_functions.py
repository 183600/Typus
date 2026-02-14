#!/usr/bin/env python3
"""
全面修复所有函数中的语法错误
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
    fixed_lines.append(line)
    
    # 检查是否是函数定义
    if line.strip().startswith('prop_') and '::' in line:
        # 获取函数名
        func_name = line.strip().split(' :: ')[0]
        
        # 查找函数体
        i += 1
        func_lines = []
        in_func = True
        indent_level = 0
        
        while i < len(lines) and in_func:
            current_line = lines[i]
            
            # 检查是否是下一个函数定义或注释
            if current_line.strip().startswith('prop_') and '::' in current_line:
                in_func = False
                break
            elif current_line.strip().startswith('-- |'):
                in_func = False
                break
            
            func_lines.append(current_line)
            i += 1
        
        # 分析函数体
        # 检查是否有 if 条件
        has_if = any('if ' in l.strip() for l in func_lines)
        
        # 检查是否有 else property True
        has_else = any('else property True' in l for l in func_lines)
        
        # 如果有 if 条件但没有 else，则添加 else
        if has_if and not has_else:
            # 查找最后一个 Left _ -> property True 或 Right _ -> property True
            for j in range(len(func_lines) - 1, -1, -1):
                if 'Left _ -> property True' in func_lines[j] or 'Right _ -> property True' in func_lines[j]:
                    # 获取缩进
                    indent = len(func_lines[j]) - len(func_lines[j].lstrip())
                    # 在这里添加 else property True
                    func_lines.insert(j + 1, '  else property True\n')
                    break
        
        # 如果没有 if 条件但有 else，则删除 else
        if not has_if and has_else:
            # 删除所有 else property True
            func_lines = [l for l in func_lines if 'else property True' not in l]
        
        # 添加修复后的函数体
        fixed_lines.extend(func_lines)
    
    i += 1

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'w') as f:
    f.writelines(fixed_lines)

print("全面修复函数中的语法错误完成")