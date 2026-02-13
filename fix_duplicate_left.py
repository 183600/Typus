#!/usr/bin/env python3
"""
修复所有重复的 Left _ -> property False
"""

# 读取文件内容
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'r') as f:
    lines = f.readlines()

# 修复后的行
fixed_lines = []
i = 0
while i < len(lines):
    line = lines[i]
    fixed_lines.append(line)
    
    # 检查是否是 Left _ -> property False 行
    if 'Left _ -> property False' in line:
        i += 1
        # 检查下一行是否也是 Left _ -> property False
        if i < len(lines) and 'Left _ -> property False' in lines[i]:
            # 跳过重复的行
            while i < len(lines) and 'Left _ -> property False' in lines[i]:
                i += 1
            i -= 1  # 调整因为循环会增加
    
    i += 1

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'w') as f:
    f.writelines(fixed_lines)

print("修复重复的 Left _ -> property False 完成")