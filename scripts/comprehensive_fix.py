#!/usr/bin/env python3

import re

# 全面分析并修复BasicQuickCheckTestSuite.hs中的所有语法错误

def fix_syntax_errors(content):
    lines = content.split('\n')
    
    # 修复每一行的语法错误
    for i, line in enumerate(lines):
        # 修复assertBool语句中多余的右括号
        if 'assertBool' in line and line.strip().endswith('))'):
            lines[i] = line.rstrip()[:-1]
        
        # 修复property语句中缺少的右括号
        if 'property (not (isInfixOf' in line and line.rstrip().endswith('withoutComments)'):
            lines[i] = line + '))'
        
        # 修复特定函数调用中缺少的右括号
        if 'isRight (Right (42 :: Int))' in line and not line.rstrip().endswith('))'):
            lines[i] = line + ')'
        if 'isRight (Left ("error" :: String))' in line and not line.rstrip().endswith('))'):
            lines[i] = line + ')'
        if 'isLeft (Left ("error" :: String))' in line and not line.rstrip().endswith('))'):
            lines[i] = line + ')'
        if 'isLeft (Right ("success" :: String))' in line and not line.rstrip().endswith('))'):
            lines[i] = line + ')'
    
    return '\n'.join(lines)

# 读取文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/BasicQuickCheckTestSuite.hs', 'r') as f:
    content = f.read()

# 修复语法错误
fixed_content = fix_syntax_errors(content)

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/BasicQuickCheckTestSuite.hs', 'w') as f:
    f.write(fixed_content)

print("所有语法错误已修复")