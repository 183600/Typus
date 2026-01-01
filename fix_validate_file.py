#!/usr/bin/env python3
import re

# 读取文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/AdditionalSyntaxValidatorSpec.hs', 'r') as f:
    content = f.read()

# 1. 修复 newSyntaxValidator 测试用例
# 将测试用例改为检查 validateSyntax 对空字符串的处理
pattern1 = r'testCase "newSyntaxValidator creates empty validator" \$ do\n\s*let validator = newSyntaxValidator\n\s*errors = getSyntaxErrors validator\n\s*length errors \@\?= 0'
replacement1 = 'testCase "validateSyntax handles empty string" $ do\n            let emptyCode = ""\n                errors = validateSyntax emptyCode\n            length errors @?= 0'
content = re.sub(pattern1, replacement1, content)

# 2. 修复 validateFile 调用
# 模式: result = validateFile validator content
pattern2 = r'(\s+)result = validateFile validator (\w+)'
content = re.sub(pattern2, r'\1errors = validateFile \2', content)

# 3. 修复 validateFile 的其他调用
# 模式: let validator = newSyntaxValidator\n                content = ...
pattern3 = r'(\s+)let validator = newSyntaxValidator\n(\s+)content = (.*)'
content = re.sub(pattern3, r'\1let content = \3', content)

# 4. 移除多余的注释
pattern4 = r'\s*-- errors already obtained from validateSyntax'
content = re.sub(pattern4, '', content)

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/AdditionalSyntaxValidatorSpec.hs', 'w') as f:
    f.write(content)

print("Fixed remaining validateFile and newSyntaxValidator calls")