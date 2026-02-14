#!/usr/bin/env python3
"""
全面修复所有多余的 else property True
"""

import re

# 读取文件内容
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'r') as f:
    content = f.read()

# 修复所有没有 if 条件的函数中的 else property True
# 查找所有 "let parsed = P.parseTypus" 模式，如果前面没有 if，则删除后面的 else
lines = content.split('\n')
fixed_lines = []
i = 0
while i < len(lines):
    line = lines[i]
    fixed_lines.append(line)
    
    # 检查是否是 "let parsed = P.parseTypus" 行
    if 'let parsed = P.parseTypus' in line:
        # 检查前面是否有 if 条件
        has_if = False
        j = i - 1
        while j >= 0 and j >= i - 10:  # 只检查前10行
            if lines[j].strip().startswith('if '):
                has_if = True
                break
            j -= 1
        
        # 如果没有 if 条件，则需要删除后面的 else property True
        if not has_if:
            # 查找后面的 else property True
            j = i + 1
            while j < len(lines) and j < i + 20:  # 只检查后20行
                if 'else property True' in lines[j]:
                    # 删除这一行
                    break
                j += 1
    
    i += 1

# 重新组合内容
content = '\n'.join(fixed_lines)

# 使用正则表达式删除所有不在 if-then-else 结构中的 else property True
# 查找所有 "else property True" 并检查它们是否在正确的位置
lines = content.split('\n')
fixed_lines = []
i = 0
in_if = False
indent_level = 0

while i < len(lines):
    line = lines[i]
    
    # 检查是否是 if 行
    if line.strip().startswith('if '):
        in_if = True
        indent_level = len(line) - len(line.lstrip())
    # 检查是否是 then 行
    elif line.strip().startswith('then ') and in_if:
        pass  # 继续在 if-then-else 结构中
    # 检查是否是 else 行
    elif 'else property True' in line:
        if not in_if:
            # 不在 if-then-else 结构中，跳过这一行
            i += 1
            continue
        else:
            # 检查缩进是否正确
            current_indent = len(line) - len(line.lstrip())
            if current_indent != indent_level:
                # 缩进不正确，跳过这一行
                i += 1
                continue
            else:
                # 正确的 else，重置 in_if 标志
                in_if = False
    # 检查是否是其他函数定义
    elif line.strip().startswith('prop_') or line.strip().startswith('-- |'):
        in_if = False
    
    fixed_lines.append(line)
    i += 1

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'w') as f:
    f.write('\n'.join(fixed_lines))

print("全面修复多余的 else property True 完成")