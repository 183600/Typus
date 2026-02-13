#!/usr/bin/env python3
"""
全面修复所有函数中的语法错误
"""

import re

# 读取文件内容
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'r') as f:
    content = f.read()

# 修复所有编译器测试函数的结构
# 查找所有 "in case compiled of" 模式，并确保它们有正确的结构
pattern1 = r'(\s+)in case compiled of\n(\s+)Right goCode -> property \$ s `isInfixOf` goCode\n(\s+)Left _ -> property False'
replacement1 = r'\1in case compiled of\n\2     Right goCode -> property $ s `isInfixOf` goCode\n\3     Left _ -> property False\n\1Left _ -> property True\n  else property True'

# 应用修复
content = re.sub(pattern1, replacement1, content, flags=re.MULTILINE)

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'w') as f:
    f.write(content)

print("全面修复语法错误完成")