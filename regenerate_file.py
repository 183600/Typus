#!/usr/bin/env python3
"""
重新生成整个文件，修复所有语法错误
"""

import re

# 读取文件内容
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'r') as f:
    content = f.read()

# 使用正则表达式修复所有编译器测试函数的结构
# 1. 修复所有 "in case compiled of" 后面的结构
content = re.sub(
    r'(\s+)in case compiled of\n(\s+)Right goCode -> property \$ s `isInfixOf` goCode\n(\s+)Left _ -> property False',
    r'\1in case compiled of\n\2     Right goCode -> property $ s `isInfixOf` goCode\n\3     Left _ -> property False',
    content,
    flags=re.MULTILINE
)

# 2. 修复所有缺少的 "Left _ -> property True"
content = re.sub(
    r'(\s+)Left _ -> property False\n(?!\s+Left _ -> property True)',
    r'\1Left _ -> property False\n\1Left _ -> property True',
    content,
    flags=re.MULTILINE
)

# 3. 修复所有缺少的 "else property True"
content = re.sub(
    r'(\s+)Left _ -> property True\n(?!\s+else property True)',
    r'\1Left _ -> property True\n  else property True',
    content,
    flags=re.MULTILINE
)

# 4. 修复重复的 "else property True"
content = re.sub(
    r'(else property True\n(\s*))else property True',
    r'\1',
    content,
    flags=re.MULTILINE
)

# 5. 修复重复的 "Left _ -> property True"
content = re.sub(
    r'(Left _ -> property True\n(\s*))Left _ -> property True',
    r'\1',
    content,
    flags=re.MULTILINE
)

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'w') as f:
    f.write(content)

print("重新生成文件完成")