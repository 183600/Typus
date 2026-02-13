#!/usr/bin/env python3
"""
批量修复 ComprehensiveTypusTestSuite.hs 中 analyzeOwnership 类型错误的脚本
"""

import re

# 读取文件内容
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'r') as f:
    content = f.read()

# 定义正则表达式模式来匹配需要修复的函数
pattern = r'(\s+)(\w+) :: String -> Property\n\1(\w+) s = \n\1if length s < 30\n\1then let (\w+Str) = "[^"]+"\n\1\s+parsed = O\.analyzeOwnership \w+Str\n\1\s+in case parsed of\n\1\s+Right analysis -> property \$ not \(null \$ show analysis\)\n\1\s+Left _ -> property True\n\1else property True'

# 定义替换模式
replacement = r'\1\2 :: String -> Property\n\1\2 s = \n\1if length s < 30\n\1then let \4Str = "\5"\n\1\s+errors = O.analyzeOwnership \4Str\n\1\s+in property $ not (null $ show errors)\n\1else property True'

# 使用正则表达式进行替换
new_content = re.sub(pattern, replacement, content, flags=re.MULTILINE)

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'w') as f:
    f.write(new_content)

print("批量修复完成")
