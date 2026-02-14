#!/usr/bin/env python3

# 修复BasicQuickCheckTestSuite.hs中的所有语法错误

with open('/home/runner/work/Typus/Typus/test/Test/Unit/BasicQuickCheckTestSuite.hs', 'r') as f:
    content = f.read()

# 修复特定行的语法错误
lines = content.split('\n')
for i, line in enumerate(lines):
    # 修复assertBool语句中多余的右括号
    if 'assertBool' in line and line.endswith('))'):
        lines[i] = line[:-2] + ')'
    
    # 修复property语句中缺少的右括号
    if 'property (not (isInfixOf' in line and line.endswith('withoutComments)'):
        lines[i] = line + '))'

with open('/home/runner/work/Typus/Typus/test/Test/Unit/BasicQuickCheckTestSuite.hs', 'w') as f:
    f.write('\n'.join(lines))

print("所有语法错误已修复")