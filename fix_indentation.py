#!/usr/bin/env python3
"""
批量修复编译器测试函数中的缩进问题
"""

import re

# 读取文件内容
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'r') as f:
    content = f.read()

# 修复编译器测试函数的缩进问题
# 模式：修复 "in case parsed of" 和 "let compiled = C.compile ast" 的缩进
pattern = r'(\s+)in case parsed of\n(\s+)Right ast -> property \$ not \(null \$ show ast\)\n(\s+)Left _ -> property True\n(\s+)let compiled = C\.compile ast\n(\s+)in case compiled of'

replacement = r'\1in case parsed of\n\2     Right ast -> \n\4       let compiled = C.compile ast\n\5       in case compiled of'

# 应用修复
content = re.sub(pattern, replacement, content, flags=re.MULTILINE)

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'w') as f:
    f.write(content)

print("修复缩进问题完成")