#!/usr/bin/env python3
"""
查找所有需要修复的地方
"""

import re

# 读取文件内容
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'r') as f:
    content = f.read()

# 查找所有包含 "in property $ not (null $ show errors)" 的行
lines = content.split('\n')
for i, line in enumerate(lines, 1):
    if 'in property $ not (null $ show errors)' in line:
        print(f"Line {i}: {line}")
        # 打印前后几行作为上下文
        start = max(0, i-3)
        end = min(len(lines), i+2)
        for j in range(start, end):
            prefix = ">>> " if j == i-1 else "    "
            print(f"{prefix}{j+1}: {lines[j]}")
        print()