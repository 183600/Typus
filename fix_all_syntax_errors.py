#!/usr/bin/env python3
"""
更全面地修复测试文件中的语法错误
"""

import os
import re

# 需要修复的文件列表
files_to_fix = []
test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
for filename in os.listdir(test_dir):
    if filename.endswith(".hs"):
        files_to_fix.append(os.path.join(test_dir, filename))

def fix_haskell_syntax(content):
    """修复Haskell语法错误"""
    lines = content.split('\n')
    fixed_lines = []
    
    for line in lines:
        # 修复导入语句中的重复错误
        if "import Test.Tasty.HUnit" in line and line.count(")") > 1:
            line = re.sub(r'\),\s*\w+\)', ')', line)
        
        # 修复do块中的变量绑定，添加let
        if re.match(r'^\s+\w+\s*=', line) and not line.strip().startswith('let'):
            # 检查是否在do块中
            prev_lines = fixed_lines[-5:] if len(fixed_lines) >= 5 else fixed_lines
            in_do_block = any('do' in pl for pl in prev_lines)
            if in_do_block:
                line = re.sub(r'^(\s+)(\w+\s*=)', r'\1let \2', line)
        
        # 修复lambda表达式
        if '\\(' in line and '->' in line and not line.strip().startswith('\\'):
            line = re.sub(r'^(\s+)', r'\1', line)
        
        # 修复QuickCheck属性中的缩进
        if 'property' in line or 'forAll' in line or '==>' in line:
            if not line.startswith('  '):
                line = '  ' + line
        
        # 修复case表达式
        if 'case' in line and 'of' in line and not line.startswith('    '):
            line = '    ' + line
        
        # 修复let绑定
        if line.strip().startswith('let ') and not line.startswith('    '):
            line = '    ' + line
        
        # 修复测试中的缩进
        if 'testCase' in line and not line.startswith('  '):
            line = '  ' + line
        
        # 修复testGroup中的缩进
        if 'testGroup' in line and not line.startswith('  '):
            line = '  ' + line
        
        fixed_lines.append(line)
    
    return '\n'.join(fixed_lines)

def fix_specific_patterns(content):
    """修复特定的语法模式"""
    # 修复lambda表达式
    content = re.sub(r'\\\(([^)]+)\)\s*->', r'\\\1 ->', content)
    
    # 修复属性测试
    content = re.sub(r'(\s+)(property|forAll|==>)', r'\1\2', content)
    
    # 修复let绑定
    content = re.sub(r'(\s+)(\w+\s*=\s*)', r'\1let \2', content)
    
    return content

def main():
    for file_path in files_to_fix:
        if os.path.exists(file_path):
            with open(file_path, 'r') as f:
                content = f.read()
            
            # 应用修复
            content = fix_haskell_syntax(content)
            content = fix_specific_patterns(content)
            
            with open(file_path, 'w') as f:
                f.write(content)
            
            print(f"Fixed {file_path}")
        else:
            print(f"File not found: {file_path}")

if __name__ == "__main__":
    main()