#!/usr/bin/env python3
import re

# 读取文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/CodeGenerationConsistencyQuickCheckSpec.hs', 'r') as f:
    content = f.read()

# 修复 let 绑定中的 assert 语句
# 模式: let goCode = generateGoCode typusFile
#       assert $ not $ null goCode
content = re.sub(
    r'let goCode = generateGoCode (\w+)\s*\n\s*assert \$ not \$ null goCode',
    r'let goCode = generateGoCode \1\n      assert $ not $ null goCode',
    content
)

# 修复其他类似的模式
# 模式: let goCode = generateGoCode typusFile
#       assert $ "func" `isInfixOf` goCode
content = re.sub(
    r'let goCode = generateGoCode (\w+)\s*\n\s*assert \$ "func" `isInfixOf` goCode',
    r'let goCode = generateGoCode \1\n      assert $ "func" `isInfixOf` goCode',
    content
)

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/CodeGenerationConsistencyQuickCheckSpec.hs', 'w') as f:
    f.write(content)

print("Fixed let binding indentation issues")