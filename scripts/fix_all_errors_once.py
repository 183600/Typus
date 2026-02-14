#!/usr/bin/env python3

import re

# 一次性修复BasicQuickCheckTestSuite.hs中的所有语法错误

with open('/home/runner/work/Typus/Typus/test/Test/Unit/BasicQuickCheckTestSuite.hs', 'r') as f:
    content = f.read()

# 使用正则表达式找到所有需要修复的行并修复
lines = content.split('\n')
for i, line in enumerate(lines):
    # 修复assertBool语句中多余的右括号
    if 'assertBool' in line and line.strip().endswith('))'):
        lines[i] = line.rstrip()[:-1] + '\n'

with open('/home/runner/work/Typus/Typus/test/Test/Unit/BasicQuickCheckTestSuite.hs', 'w') as f:
    f.write('\n'.join(lines))

print("所有语法错误已修复")