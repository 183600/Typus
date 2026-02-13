#!/usr/bin/env python3

# 精确修复 Ownership 类型错误，不影响其他部分

import re

def precise_ownership_fix():
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'r') as f:
        content = f.read()
    
    # 只修复 O.analyzeOwnership 后面的特定模式
    # 使用更精确的正则表达式
    
    # 模式1: errors = O.analyzeOwnership \w+ 后跟换行和 "in case errors of:"
    pattern1 = r'(\s+errors = O\.analyzeOwnership \w+)\n(\s+)in case errors of:\n(\s+)Right _ -> property True\n(\s+)Left _ -> property False'
    
    def replacement1(match):
        errors_line = match.group(1)
        indent = match.group(2)
        return f"{errors_line}\n{indent}in property $ null errors"
    
    content = re.sub(pattern1, replacement1, content, flags=re.MULTILINE)
    
    # 模式2: 处理多行字符串中的 O.analyzeOwnership
    pattern2 = r'(\s+errors = O\.analyzeOwnership \w+)\n(\s+)in case errors of:\n(\s+)Right _ -> property True\n(\s+)Left _ -> property False'
    
    content = re.sub(pattern2, replacement1, content, flags=re.MULTILINE)
    
    # 写回文件
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'w') as f:
        f.write(content)
    
    print("Precise ownership fix completed")

if __name__ == "__main__":
    precise_ownership_fix()