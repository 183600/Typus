#!/usr/bin/env python3
"""
全面修复所有编译器测试函数中的缩进问题
"""

import re

# 读取文件内容
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'r') as f:
    content = f.read()

# 修复所有编译器测试函数的缩进问题
# 1. 修复 "in case parsed of" 后面的内容
content = re.sub(
    r'(\s+)in case parsed of\n(\s+)Right ast -> property \$ not \(null \$ show ast\)\n(\s+)Left _ -> property True\n(\s+)let compiled = C\.compile ast',
    r'\1in case parsed of\n\2     Right ast -> \n\4       let compiled = C.compile ast',
    content,
    flags=re.MULTILINE
)

# 2. 修复 "let compiled = C.compile ast" 后面的内容
content = re.sub(
    r'(\s+)let compiled = C\.compile ast\n(\s+)in case compiled of',
    r'\1let compiled = C.compile ast\n\2in case compiled of',
    content,
    flags=re.MULTILINE
)

# 3. 修复 "Right ast ->" 后面的内容
content = re.sub(
    r'(\s+)Right ast ->\n(\s+)let compiled = C\.compile ast',
    r'\1Right ast -> \n\2  let compiled = C.compile ast',
    content,
    flags=re.MULTILINE
)

# 4. 修复缩进级别
content = re.sub(
    r'(\s+)Right ast ->\n(\s+)property \$ s `isInfixOf` goCode',
    r'\1Right ast -> \n\2  property $ s `isInfixOf` goCode',
    content,
    flags=re.MULTILINE
)

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'w') as f:
    f.write(content)

print("全面修复缩进问题完成")