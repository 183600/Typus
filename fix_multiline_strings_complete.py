#!/usr/bin/env python3
"""
修复ComprehensiveTypusTestSuite.hs中的多行字符串问题
"""

import re

def fix_multiline_strings():
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs"
    
    # 读取文件
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # 修复多行字符串问题 - 将真正的多行字符串转换为用 \n 连接的单行
    lines = content.split('\n')
    fixed_lines = []
    i = 0
    
    while i < len(lines):
        line = lines[i]
        
        # 检查是否是包含 "{//! ownership: on" 的行但不是完整字符串
        if '{//! ownership: on' in line and not line.strip().endswith('}'):
            # 找到这个多行字符串的结束
            multiline_start = i
            indent = ''
            # 提取缩进
            match = re.match(r'^(\s*)', line)
            if match:
                indent = match.group(1)
            
            # 收集多行字符串内容
            string_parts = []
            current_line = line
            
            # 提取第一行的开始部分
            start_match = re.match(r'(\s*then let \w+Str = ")(.*)', current_line)
            if start_match:
                prefix = start_match.group(1)
                first_part = start_match.group(2)
                string_parts.append(first_part)
                
                # 继续收集后续行直到找到结束的 }
                i += 1
                while i < len(lines):
                    next_line = lines[i]
                    if next_line.strip().endswith('"}'):
                        # 最后一行
                        string_parts.append(next_line.strip())
                        break
                    else:
                        # 中间行
                        string_parts.append(next_line.strip())
                    i += 1
                
                # 重新组合为单行字符串
                combined_string = '\\n'.join(string_parts)
                fixed_line = f"{indent}then let moveStr = \"{combined_string}"
                # 需要修复变量名
                var_match = re.search(r'let (\w+)Str =', prefix)
                if var_match:
                    var_name = var_match.group(1)
                    fixed_line = f"{indent}then let {var_name}Str = \"{combined_string}"
                
                fixed_lines.append(fixed_line)
            else:
                fixed_lines.append(line)
        else:
            fixed_lines.append(line)
        
        i += 1
    
    # 重新组合内容
    fixed_content = '\n'.join(fixed_lines)
    
    # 写回文件
    with open(file_path, 'w', encoding='utf-8') as f:
        f.write(fixed_content)
    
    print("多行字符串修复完成!")

if __name__ == "__main__":
    fix_multiline_strings()