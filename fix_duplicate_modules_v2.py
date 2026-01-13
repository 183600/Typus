#!/usr/bin/env python3
"""
修复cabal文件中的重复模块问题
"""

def fix_duplicate_modules():
    cabal_file = "/home/runner/work/Typus/Typus/typus.cabal"
    
    with open(cabal_file, 'r') as f:
        lines = f.readlines()
    
    # 找出要删除的行号
    lines_to_remove = []
    for i, line in enumerate(lines):
        if 'Test.Unit.CompilerAdvancedQuickCheckSpec' in line and i > 2000:
            lines_to_remove.append(i)
        elif 'Test.Unit.DependenciesAdvancedQuickCheckSpec' in line and i > 2000:
            lines_to_remove.append(i)
        elif 'Test.Unit.OwnershipAdvancedQuickCheckSpec' in line and i > 2000:
            lines_to_remove.append(i)
    
    # 删除重复的行
    new_lines = [line for i, line in enumerate(lines) if i not in lines_to_remove]
    
    # 写回文件
    with open(cabal_file, 'w') as f:
        f.writelines(new_lines)
    
    print(f"已删除 {len(lines_to_remove)} 个重复模块行")

if __name__ == "__main__":
    fix_duplicate_modules()