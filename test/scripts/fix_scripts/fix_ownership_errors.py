#!/usr/bin/env python3

# 系统地修复 ComprehensiveTypusTestSuite.hs 中所有的 Ownership 类型不匹配错误

import re

def fix_ownership_errors():
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'r') as f:
        content = f.read()
    
    # 模式：匹配 O.analyzeOwnership 后面的 case 语句
    pattern = r'(errors = O\.analyzeOwnership.*?\n\s+)in case errors of:\s*\n\s*Right _ -> property True\s*\n\s*Left _ -> property False'
    
    # 替换为正确的形式
    replacement = r'\1in property $ null errors'
    
    # 进行替换
    new_content = re.sub(pattern, replacement, content, flags=re.MULTILINE | re.DOTALL)
    
    # 统计替换次数
    if new_content != content:
        print("Successfully fixed ownership type errors")
        # 写回文件
        with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'w') as f:
            f.write(new_content)
        return True
    else:
        print("No matches found")
        return False

if __name__ == "__main__":
    fix_ownership_errors()