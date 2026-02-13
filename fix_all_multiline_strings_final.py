#!/usr/bin/env python3
"""
彻底修复ComprehensiveTypusTestSuite.hs中的所有多行字符串问题
"""

import re

def fix_all_multiline_strings():
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs"
    
    # 读取文件
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # 定义需要修复的多行字符串模式
    patterns_to_fix = [
        # moveStr
        (r'let moveStr = "\{\/\//! ownership: on\ns := NewMyString\(\"" \+\+ s \+\+ \"\)\nt := s\n\/\/ s 已被移动\}', 
         r'let moveStr = "{//! ownership: on\\ns := NewMyString(\"" ++ s ++ "\")\\nt := s\\n// s 已被移动}"'),
        
        # 其他类似模式
        (r'let \w+Str = "\{\/\//! ownership: on[^}]*\}', 
         lambda m: m.group(0).replace('\n', '\\n')),
    ]
    
    # 逐个模式修复
    for pattern, replacement in patterns_to_fix:
        if callable(replacement):
            content = re.sub(pattern, replacement, content, flags=re.MULTILINE | re.DOTALL)
        else:
            content = re.sub(pattern, replacement, content, flags=re.MULTILINE | re.DOTALL)
    
    # 通用方法：找到所有包含多行字符串的let绑定并修复
    lines = content.split('\n')
    result_lines = []
    i = 0
    
    while i < len(lines):
        line = lines[i]
        
        # 检查是否是let绑定开始的多行字符串
        if re.match(r'^\s*then let \w+Str = "\{\/\//! ownership: on$', line):
            # 这是一个多行字符串的开始
            indent = re.match(r'^(\s*)', line).group(1)
            var_name = re.search(r'let (\w+)Str =', line).group(1)
            
            # 收集整个多行字符串
            string_lines = []
            j = i
            while j < len(lines) and not lines[j].strip().endswith('}"'):
                if j > i:  # 跳过第一行，我们已经处理了
                    string_lines.append(lines[j])
                j += 1
            
            # 添加最后一行
            if j < len(lines):
                string_lines.append(lines[j])
            
            # 重新构建为单行
            if string_lines:
                # 提取字符串内容（去掉引号和缩进）
                first_line = string_lines[0] if string_lines else ""
                if '"{//! ownership: on' in first_line:
                    # 重新组合
                    combined_content = '\\n'.join([l.strip() for l in string_lines])
                    fixed_line = f"{indent}then let {var_name}Str = \"{combined_content}"
                    result_lines.append(fixed_line)
                else:
                    # 保持原样
                    result_lines.extend(lines[i:j+1])
            else:
                result_lines.append(line)
            
            i = j + 1
        else:
            result_lines.append(line)
            i += 1
    
    # 写回文件
    fixed_content = '\n'.join(result_lines)
    with open(file_path, 'w', encoding='utf-8') as f:
        f.write(fixed_content)
    
    print("所有多行字符串修复完成!")

if __name__ == "__main__":
    fix_all_multiline_strings()