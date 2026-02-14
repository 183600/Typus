#!/usr/bin/env python3

# 系统地修复 ComprehensiveTypusTestSuite.hs 中所有的 Ownership 类型不匹配错误

import re

def fix_ownership_errors():
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'r') as f:
        content = f.read()
    
    # 分解处理：先找到所有 O.analyzeOwnership 的位置
    lines = content.split('\n')
    fixed_lines = []
    i = 0
    
    while i < len(lines):
        line = lines[i]
        
        # 检查是否包含 O.analyzeOwnership
        if 'errors = O.analyzeOwnership' in line:
            # 添加这一行
            fixed_lines.append(line)
            i += 1
            
            # 检查接下来的几行是否有 case errors of 模式
            if i < len(lines) and 'in case errors of:' in lines[i]:
                # 跳过 case 语句
                i += 1
                if i < len(lines) and 'Right _ -> property True' in lines[i]:
                    i += 1  # 跳过 Right 行
                if i < len(lines) and 'Left _ -> property False' in lines[i]:
                    i += 1  # 跳过 Left 行
                
                # 添加正确的替换
                fixed_lines.append('       in property $ null errors')
            else:
                # 如果没有找到预期的模式，保持原样
                fixed_lines.append(lines[i] if i < len(lines) else '')
                i += 1
        else:
            fixed_lines.append(line)
            i += 1
    
    # 写回文件
    new_content = '\n'.join(fixed_lines)
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'w') as f:
        f.write(new_content)
    
    print("Fixed ownership type errors")

if __name__ == "__main__":
    fix_ownership_errors()