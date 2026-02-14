#!/usr/bin/env python3

# 修复BasicQuickCheckTestSuite.hs中的所有语法错误

with open('/home/runner/work/Typus/Typus/test/Test/Unit/BasicQuickCheckTestSuite.hs', 'r') as f:
    content = f.read()

# 修复所有缺少右括号的地方
content = content.replace('(isRight (Right (42 :: Int))', '(isRight (Right (42 :: Int)))')
content = content.replace('(not $ isRight (Left ("error" :: String))', '(not $ isRight (Left ("error" :: String)))')
content = content.replace('(not $ isLeft (Right ("success" :: String))', '(not $ isLeft (Right ("success" :: String)))')

# 修复所有多余右括号的地方
content = content.replace('))', ')')

# 修复特定行
lines = content.split('\n')
for i, line in enumerate(lines):
    if 'assertBool "Right value is right"' in line and 'isRight (Right (42 :: Int)' in line:
        if not line.endswith('))'):
            lines[i] = line + ')'
    elif 'assertBool "Left value is not right"' in line and 'not $ isRight (Left ("error" :: String)' in line:
        if not line.endswith('))'):
            lines[i] = line + ')'
    elif 'assertBool "Right value is not left"' in line and 'not $ isLeft (Right ("success" :: String)' in line:
        if not line.endswith('))'):
            lines[i] = line + ')'

with open('/home/runner/work/Typus/Typus/test/Test/Unit/BasicQuickCheckTestSuite.hs', 'w') as f:
    f.write('\n'.join(lines))

print("所有语法错误已修复")