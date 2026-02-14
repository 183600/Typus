#!/usr/bin/env python3

# 修复缺失的函数签名和结构

import re

def fix_missing_signatures():
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'r') as f:
        content = f.read()
    
    # 查找所有缺失函数签名的模式
    # 模式：函数调用直接开始，没有签名行
    lines = content.split('\n')
    fixed_lines = []
    
    i = 0
    while i < len(lines):
        line = lines[i]
        
        # 检查是否是一个没有签名的函数实现
        if (re.match(r'^prop_\w+\s+\w+\s*=', line.strip()) and 
            i > 0 and 
            not lines[i-1].strip().endswith(':: Property')):
            
            # 提取函数名
            func_name_match = re.match(r'^(prop_\w+)\s+', line.strip())
            if func_name_match:
                func_name = func_name_match.group(1)
                # 添加缺失的签名
                fixed_lines.append(f"{func_name} :: String -> Property")
        
        fixed_lines.append(line)
        i += 1
    
    # 写回文件
    new_content = '\n'.join(fixed_lines)
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'w') as f:
        f.write(new_content)
    
    print("Fixed missing function signatures")

if __name__ == "__main__":
    fix_missing_signatures()