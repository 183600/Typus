#!/usr/bin/env python3
"""
使用更简单的方法修复所有缩进问题
"""

# 读取文件内容
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'r') as f:
    lines = f.readlines()

# 修复后的行
fixed_lines = []
i = 0
while i < len(lines):
    line = lines[i]
    
    # 检查是否是编译器测试函数中的 "in case parsed of" 行
    if 'in case parsed of' in line and i > 0:
        # 检查前一行是否包含 P.parseTypus
        if i > 0 and 'P.parseTypus' in lines[i-1]:
            # 修复这个函数
            fixed_lines.append(line)
            i += 1
            
            # 添加 Right ast -> 行
            while i < len(lines) and 'Right ast ->' not in lines[i]:
                fixed_lines.append(lines[i])
                i += 1
            
            if i < len(lines):
                # 修复 Right ast -> 行
                indent = len(line) - len(line.lstrip())
                fixed_lines.append(' ' * (indent + 5) + 'Right ast -> \n')
                i += 1
                
                # 添加 let compiled = 行
                while i < len(lines) and 'let compiled = C.compile ast' not in lines[i]:
                    fixed_lines.append(lines[i])
                    i += 1
                
                if i < len(lines):
                    fixed_lines.append(' ' * (indent + 7) + 'let compiled = C.compile ast\n')
                    i += 1
                    
                    # 添加 in case compiled of 行
                    while i < len(lines) and 'in case compiled of' not in lines[i]:
                        fixed_lines.append(lines[i])
                        i += 1
                    
                    if i < len(lines):
                        fixed_lines.append(' ' * (indent + 7) + 'in case compiled of\n')
                        i += 1
                        
                        # 添加 Right goCode -> 和 Left _ -> 行
                        while i < len(lines) and ('Right goCode ->' not in lines[i] and 'Left _ ->' not in lines[i]):
                            fixed_lines.append(lines[i])
                            i += 1
                        
                        while i < len(lines) and ('Right goCode ->' in lines[i] or 'Left _ ->' in lines[i]):
                            if 'Right goCode ->' in lines[i]:
                                fixed_lines.append(' ' * (indent + 12) + lines[i].strip() + '\n')
                            elif 'Left _ ->' in lines[i]:
                                fixed_lines.append(' ' * (indent + 12) + lines[i].strip() + '\n')
                            i += 1
        else:
            fixed_lines.append(line)
    else:
        fixed_lines.append(line)
    
    i += 1

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'w') as f:
    f.writelines(fixed_lines)

print("简单修复缩进问题完成")