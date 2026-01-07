#!/usr/bin/env python3
"""
全面修复测试文件中的语法错误
"""

import os
import re

# 需要修复的文件列表
files_to_fix = []
test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
for filename in os.listdir(test_dir):
    if filename.endswith(".hs"):
        files_to_fix.append(os.path.join(test_dir, filename))

def fix_comprehensive_syntax_errors(content):
    """全面修复语法错误"""
    lines = content.split('\n')
    fixed_lines = []
    
    for line in lines:
        # 修复属性定义的缩进和语法
        if line.strip().startswith('prop_') and '::' in line:
            # 确保属性定义有正确的缩进
            line = '  ' + line.strip()
        
        # 修复重复的 let 关键字
        if 'let let' in line:
            line = line.replace('let let', 'let')
        
        # 修复 tests 定义的语法错误
        if line.strip().startswith('let tests =') and not line.startswith('tests ='):
            line = line.replace('let tests =', 'tests =')
        
        # 修复导入语句的缩进
        if line.strip().startswith('import ') and not line.startswith('import '):
            line = 'import ' + line.strip()[7:]
        
        # 修复 do 块中的变量绑定
        if re.match(r'^\s+\w+\s*<-', line) and not line.strip().startswith('let'):
            # 检查是否在 do 块中
            prev_lines = fixed_lines[-5:] if len(fixed_lines) >= 5 else fixed_lines
            in_do_block = any('do' in pl for pl in prev_lines)
            if not in_do_block:
                # 如果不在 do 块中，可能需要添加 do
                pass
        
        fixed_lines.append(line)
    
    return '\n'.join(fixed_lines)

def fix_specific_patterns(content):
    """修复特定的语法模式"""
    # 重复的 let 关键字
    content = re.sub(r'let let', 'let', content)
    
    # 修复属性定义
    content = re.sub(r'^(\s+)prop_(\w+)\s*::', r'  prop_\2 ::', content, flags=re.MULTILINE)
    
    # 修复 tests 定义
    content = re.sub(r'(\s+)let tests =', r'tests =', content)
    
    # 修复导入语句
    content = re.sub(r'^(\s+)import ', r'import ', content, flags=re.MULTILINE)
    
    return content

def main():
    for file_path in files_to_fix:
        if os.path.exists(file_path):
            with open(file_path, 'r') as f:
                content = f.read()
            
            # 应用修复
            content = fix_comprehensive_syntax_errors(content)
            content = fix_specific_patterns(content)
            
            with open(file_path, 'w') as f:
                f.write(content)
            
            print(f"Fixed {file_path}")
        else:
            print(f"File not found: {file_path}")

if __name__ == "__main__":
    main()