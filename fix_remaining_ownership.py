#!/usr/bin/env python3

# 批量修复剩余的 Ownership 类型错误

import re

def fix_remaining_ownership_errors():
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'r') as f:
        content = f.read()
    
    # 使用更简单的模式：找到所有 "errors = O.analyzeOwnership" 后面的错误模式
    # 并替换为正确的形式
    
    # 模式匹配以 errors = O.analyzeOwnership 开头，后跟换行和缩进，然后是 case 语句
    pattern = r'(\s+errors = O\.analyzeOwnership \w+)\n(\s+)in case errors of:\n(\s+)Right _ -> property True\n(\s+)Left _ -> property False'
    
    def replacement_func(match):
        var_line = match.group(1)  # errors = O.analyzeOwnership ...
        indent = match.group(2)     # 缩进
        return f"{var_line}\n{indent}in property $ null errors"
    
    new_content = re.sub(pattern, replacement_func, content, flags=re.MULTILINE)
    
    # 统计修改
    changes = len(re.findall(pattern, content, flags=re.MULTILINE))
    
    if changes > 0:
        print(f"Found and fixed {changes} more ownership type errors")
        with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'w') as f:
            f.write(new_content)
        return True
    else:
        print("No more patterns found")
        return False

if __name__ == "__main__":
    fix_remaining_ownership_errors()