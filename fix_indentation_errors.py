#!/usr/bin/env python3
import os
import re

def fix_indentation_errors(file_path):
    """修复常见的缩进错误"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        original_content = content
        
        # 修复 testGroup 缩进错误
        content = re.sub(r'(tests :: TestTree\s+tests =\s+)(testGroup)', r'\1  \2', content)
        
        # 修复 prop_ 函数定义中的缩进错误
        content = re.sub(r'(prop_\w+ .+ =\n)([^ \t\n])', r'\1  \2', content)
        
        # 修复 let/in 表达式中的缩进错误
        content = re.sub(r'(let .+\n\s+.+\n)(in [^ \t\n])', r'\1      \2', content)
        
        # 修复 import 语句中的语法错误
        content = re.sub(r'(import .+ \(\s+[^)]+)(\s+\))', r'\1\2', content, flags=re.MULTILINE)
        
        # 修复模块导出列表中的语法错误
        content = re.sub(r'(\w+\.\w+\([^)]+)\s+\)\s*,\s*(\w+\.\w+)', r'\1),\n    \2', content)
        
        if content != original_content:
            with open(file_path, 'w') as f:
                f.write(content)
            print(f"Fixed indentation errors in {file_path}")
            return True
        return False
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return False

def main():
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    fixed_count = 0
    
    for root, dirs, files in os.walk(test_dir):
        for file in files:
            if file.endswith('.hs'):
                file_path = os.path.join(root, file)
                if fix_indentation_errors(file_path):
                    fixed_count += 1
    
    print(f"Fixed indentation errors in {fixed_count} files")

if __name__ == "__main__":
    main()