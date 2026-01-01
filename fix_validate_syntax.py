#!/usr/bin/env python3
import re

# 读取文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/AdditionalSyntaxValidatorSpec.hs', 'r') as f:
    content = f.read()

# 替换模式1: 移除 validator 变量声明
# 模式: let validator = newSyntaxValidator\n                code = "..."
pattern1 = r'(\s+)let validator = newSyntaxValidator\n(\s+)code = (.*)'
content = re.sub(pattern1, r'\1let code = \3', content)

# 替换模式2: 修复 validateSyntax 调用
# 模式: result = validateSyntax validator code\n                errors = getSyntaxErrors result
pattern2 = r'(\s+)result = validateSyntax validator code\n(\s+)errors = getSyntaxErrors result'
content = re.sub(pattern2, r'\1errors = validateSyntax code', content)

# 替换模式3: 修复其他 validateSyntax 调用
# 模式: result = validateSyntax validator someCode
pattern3 = r'(\s+)result = validateSyntax validator (\w+)'
content = re.sub(pattern3, r'\1errors = validateSyntax \2', content)

# 替换模式4: 修复 getSyntaxErrors 调用在其他变量上
# 模式: errors = getSyntaxErrors result
pattern4 = r'(\s+)errors = getSyntaxErrors result'
content = re.sub(pattern4, r'\1-- errors already obtained from validateSyntax', content)

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/AdditionalSyntaxValidatorSpec.hs', 'w') as f:
    f.write(content)

print("Fixed validateSyntax calls in AdditionalSyntaxValidatorSpec.hs")