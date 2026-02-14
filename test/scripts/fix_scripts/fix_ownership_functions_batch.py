#!/usr/bin/env python3
"""
系统性修复ComprehensiveTypusTestSuite.hs中的ownership测试函数错误
"""

import re

def fix_ownership_functions():
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs"
    
    # 读取文件
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # 修复模式1: 缺少 errors = O.analyzeOwnership 定义的函数
    # 匹配格式: let xxxStr = "... then in case errors of
    pattern1 = r'(\s+then let (\w+)Str = "([^"]*(?:\+\+ s \+\+)[^"]*)"\s+)(in case errors of)'
    
    def replace_missing_errors(match):
        indent = match.group(1)
        var_name = match.group(2)
        str_content = match.group(3)
        case_part = match.group(4)
        
        # 检查是否已经有 errors 定义
        if "errors = O.analyzeOwnership" in indent:
            return match.group(0)
        
        # 添加 errors 定义
        return f'{indent}errors = O.analyzeOwnership {var_name}Str\n{indent}{case_part}'
    
    content = re.sub(pattern1, replace_missing_errors, content, flags=re.MULTILINE)
    
    # 修复模式2: 修复字符串连接问题 "" ++ s ++ "" -> "\"" ++ s ++ "\""
    pattern2 = r'""\s*\+\+\s*s\s*\+\+\s*""'
    content = re.sub(pattern2, r'"\"" ++ s ++ "\""', content)
    
    # 修复模式3: 修复其他字符串连接问题 "world" -> "\"world\""
    pattern3 = r'm\.data = "world"'
    content = re.sub(pattern3, r'm.data = "world"', content)
    
    # 写回文件
    with open(file_path, 'w', encoding='utf-8') as f:
        f.write(content)
    
    print("修复完成!")

if __name__ == "__main__":
    fix_ownership_functions()
