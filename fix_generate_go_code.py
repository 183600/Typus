#!/usr/bin/env python3
import re

# 读取文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/CodeGenerationConsistencyQuickCheckSpec.hs', 'r') as f:
    content = f.read()

# 修复 generateGoCode 返回值的处理
# 模式: case generateGoCode typusFile of
#         Left _ -> assert False
#         Right goCode -> do
content = re.sub(
    r'case generateGoCode (\w+) of\s*\n\s*Left _ -> assert False\s*\n\s*Right (\w+) -> do',
    r'let \2 = generateGoCode \1',
    content
)

# 修复其他类似的模式
# 模式: let goResult = generateGoCode typusFile
#       case goResult of
#         Left _ -> assert False
#         Right goCode -> do
content = re.sub(
    r'let goResult = generateGoCode (\w+)\s*\n\s*case goResult of\s*\n\s*Left _ -> assert False\s*\n\s*Right (\w+) -> do',
    r'let \2 = generateGoCode \1',
    content
)

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/CodeGenerationConsistencyQuickCheckSpec.hs', 'w') as f:
    f.write(content)

print("Fixed remaining generateGoCode return value issues")