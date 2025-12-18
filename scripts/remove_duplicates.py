#!/usr/bin/env python3
import re

with open('typus.cabal', 'r') as f:
    lines = f.readlines()

# 找到other-modules部分
in_other_modules = False
seen_modules = set()
output_lines = []
skip_next_comma = False

for i, line in enumerate(lines):
    # 检测other-modules部分的开始
    if re.match(r'\s*other-modules:', line):
        in_other_modules = True
        seen_modules.clear()
        output_lines.append(line)
        continue
    
    # 检测other-modules部分的结束（新的字段开始）
    if in_other_modules and re.match(r'\s*\w+:', line) and not line.strip().startswith('--'):
        in_other_modules = False
        seen_modules.clear()
    
    # 在other-modules部分中处理模块行
    if in_other_modules:
        # 提取模块名
        match = re.match(r'(\s*)(Test\.Unit\.\w+)(,?)\s*$', line)
        if match:
            indent, module, comma = match.groups()
            if module in seen_modules:
                # 跳过重复的模块
                print(f"Skipping duplicate: {module}")
                continue
            seen_modules.add(module)
    
    output_lines.append(line)

with open('typus.cabal', 'w') as f:
    f.writelines(output_lines)

print("Done!")
