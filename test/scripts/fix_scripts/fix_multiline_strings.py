#!/usr/bin/env python3

# 修复 ComprehensiveTypusTestSuite.hs 中的换行符问题

with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'r') as f:
    content = f.read()

# 修复所有包含实际换行符的字符串
replacements = [
    # 修复 prop_ownership_basic_semantics
    ('"{//! ownership: on\ns := NewMyString(\"' + '\" ++ s ++ \"' + '\")\nt := s}"', 
     '"{//! ownership: on\\ns := NewMyString(\"' + '\" ++ s ++ \"' + '\")\\nt := s}"'),
    
    # 修复 prop_move_semantics  
    ('"{//! ownership: on\ns := NewMyString(\"' + '\" ++ s ++ \"' + '\")\nt := s\n// s 已被移动}"',
     '"{//! ownership: on\\ns := NewMyString(\"' + '\" ++ s ++ \"' + '\")\\nt := s\\n// s 已被移动}"'),
]

for old, new in replacements:
    if old in content:
        content = content.replace(old, new)
        print(f"Fixed: {old[:50]}...")
    else:
        print(f"Not found: {old[:50]}...")

# 逐行修复更复杂的模式
lines = content.split('\n')
fixed_lines = []
i = 0
while i < len(lines):
    line = lines[i]
    
    # 检查是否是多行字符串的开始
    if 'ownershipStr = "{//! ownership: on' in line and line.strip().endswith('"'):
        # 这是一个多行字符串的开始
        next_line = lines[i + 1] if i + 1 < len(lines) else ''
        if 's := NewMyString(' in next_line:
            # 重新构建为单行
            combined = line.rstrip() + '\\n' + next_line.strip()
            if i + 2 < len(lines):
                third_line = lines[i + 2]
                if third_line.strip() == 't := s}"':
                    combined += '\\n' + third_line.strip()
                    fixed_lines.append(combined)
                    i += 3
                    continue
        
        # 如果不匹配我们的模式，保持原样
        fixed_lines.append(line)
    else:
        fixed_lines.append(line)
    i += 1

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'w') as f:
    f.write('\n'.join(fixed_lines))

print("Fixed multiline string issues")