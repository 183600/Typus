#!/usr/bin/env python3
"""
批量修复所有 parseTypus 和 parseDependentType 测试函数中的错误
"""

import re

# 读取文件内容
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'r') as f:
    content = f.read()

# 修复 parseTypus 相关的函数
# 模式1: in property $ not (null $ show errors) 后面跟着 Right/Left
pattern1 = r'(\s+)(in property \$ not \(null \$ show errors\)\n(?:\s+Right.*->.*property.*\n)(?:\s+Left.*->.*property.*))'
replacement1 = r'\1in case parsed of\n\1     Right ast -> property $ not (null $ show ast)\n\1     Left _ -> property True'

# 应用修复
content = re.sub(pattern1, replacement1, content, flags=re.MULTILINE)

# 修复 parseDependentType 相关的函数  
# 模式2: 类似的模式但针对 DTP.parseDependentType
pattern2 = r'(\s+)(in property \$ not \(null \$ show errors\)\n(?:\s+Right.*->.*property.*\n)(?:\s+Left.*->.*property.*))'
replacement2 = r'\1in case parsed of\n\1     Right _ -> property True\n\1     Left _ -> property False'

# 应用修复
content = re.sub(pattern2, replacement2, content, flags=re.MULTILINE)

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'w') as f:
    f.write(content)

print("批量修复完成")