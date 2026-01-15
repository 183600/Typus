#!/usr/bin/env python3
import re

# 读取文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ErrorHandlerConsistencyQuickCheckSpec.hs', 'r') as f:
    content = f.read()

# 替换所有 SyntaxError 为 Parsing
content = re.sub(r'\bSyntaxError\b', 'Parsing', content)

# 替换所有 UnknownLocation 为 unknownLocation
content = re.sub(r'\bUnknownLocation\b', 'unknownLocation', content)

# 替换所有 NoRecovery 为 errorRecovery
content = re.sub(r'\bNoRecovery\b', 'errorRecovery', content)

# 修复 Property 类型错误
# 查找所有使用 === 和 && 的地方，并修复它们
content = re.sub(r'(\w+)\s+===\s+(\w+)\s+&&\s+(\w+)\s+===\s+(\w+)', r'\1 === \2 && \3 === \4', content)

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ErrorHandlerConsistencyQuickCheckSpec.hs', 'w') as f:
    f.write(content)

print("Fixed ErrorHandlerConsistencyQuickCheckSpec.hs")