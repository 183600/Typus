#!/usr/bin/env python3
import os
import re

def check_brackets(file_path):
    """检查文件中的括号是否匹配"""
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # 移除注释和字符串中的括号
    lines = content.split('\n')
    cleaned_lines = []
    in_string = False
    in_comment = False
    string_char = None
    
    for line in lines:
        cleaned_line = ""
        i = 0
        while i < len(line):
            char = line[i]
            
            # 处理字符串
            if not in_comment:
                if not in_string and char in ['"', "'"]:
                    in_string = True
                    string_char = char
                    cleaned_line += char
                elif in_string and char == string_char:
                    # 检查是否是转义的引号
                    if i > 0 and line[i-1] == '\\':
                        cleaned_line += char
                    else:
                        in_string = False
                        string_char = None
                        cleaned_line += char
                elif in_string:
                    cleaned_line += char
                # 处理注释
                elif char == '-' and i + 1 < len(line) and line[i+1] == '-':
                    in_comment = True
                    break
                else:
                    cleaned_line += char
            i += 1
        
        if not in_comment:
            cleaned_lines.append(cleaned_line)
    
    cleaned_content = '\n'.join(cleaned_lines)
    
    # 计算括号
    open_brackets = cleaned_content.count('[')
    close_brackets = cleaned_content.count(']')
    open_parens = cleaned_content.count('(')
    close_parens = cleaned_content.count(')')
    open_braces = cleaned_content.count('{')
    close_braces = cleaned_content.count('}')
    
    return {
        'file': file_path,
        'open_brackets': open_brackets,
        'close_brackets': close_brackets,
        'open_parens': open_parens,
        'close_parens': close_parens,
        'open_braces': open_braces,
        'close_braces': close_braces,
        'bracket_diff': open_brackets - close_brackets,
        'paren_diff': open_parens - close_parens,
        'brace_diff': open_braces - close_braces
    }

def fix_bracket_mismatch(file_path):
    """修复括号不匹配的问题"""
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    new_content = content
    
    # 修复多余的右括号
    # 例如: ]))
    # 修复为: ])
    new_content = re.sub(r'\]\)\)', '])', new_content)
    
    # 修复多余的右括号
    # 例如: ]))
    # 修复为: ])
    
    if new_content != content:
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(new_content)
        print(f"Fixed bracket mismatch in {file_path}")
        return True
    
    return False

def main():
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/AdditionalCabalTestSpec.hs"
    
    # 检查括号
    bracket_info = check_brackets(file_path)
    print(f"Bracket info for {bracket_info['file']}:")
    print(f"  Open brackets: {bracket_info['open_brackets']}, Close brackets: {bracket_info['close_brackets']}, Diff: {bracket_info['bracket_diff']}")
    print(f"  Open parens: {bracket_info['open_parens']}, Close parens: {bracket_info['close_parens']}, Diff: {bracket_info['paren_diff']}")
    print(f"  Open braces: {bracket_info['open_braces']}, Close braces: {bracket_info['close_braces']}, Diff: {bracket_info['brace_diff']}")
    
    # 修复括号不匹配
    if bracket_info['bracket_diff'] < 0:
        print(f"Too many closing brackets: {abs(bracket_info['bracket_diff'])}")
        fix_bracket_mismatch(file_path)
    elif bracket_info['bracket_diff'] > 0:
        print(f"Too many opening brackets: {bracket_info['bracket_diff']}")
    
    if bracket_info['paren_diff'] < 0:
        print(f"Too many closing parens: {abs(bracket_info['paren_diff'])}")
    elif bracket_info['paren_diff'] > 0:
        print(f"Too many opening parens: {bracket_info['paren_diff']}")

if __name__ == "__main__":
    main()