#!/usr/bin/env python3
"""
修复使用L.函数但没有正确导入qualified Data.List as L的文件
"""

import os
import re
import sys

def fix_qualified_usage_in_file(file_path):
    """修复单个文件中的L.函数使用问题"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        original_content = content
        
        # 检查是否使用了L.函数
        l_function_pattern = r'L\.(null|length|head|foldl|all|map|maximum|isPrefixOf|isSuffixOf|isInfixOf|any|sum|take|drop|elem)'
        uses_l_functions = re.search(l_function_pattern, content)
        
        if not uses_l_functions:
            return False
        
        # 检查是否已经导入了qualified Data.List as L
        has_qualified_import = re.search(r'import qualified Data\.List as L', content)
        
        if not has_qualified_import:
            # 找到第一个import语句，在其后添加qualified import
            import_pattern = r'(import [^\n]+)'
            matches = list(re.finditer(import_pattern, content))
            
            if matches:
                # 在第一个import语句后添加qualified import
                first_import = matches[0]
                insert_pos = first_import.end()
                
                # 插入qualified import
                qualified_import = '\nimport qualified Data.List as L'
                content = content[:insert_pos] + qualified_import + content[insert_pos:]
                
                # 写入文件
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(content)
                
                print(f"Added qualified import to: {file_path}")
                return True
        
        return False
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return False

def find_files_to_fix(directory):
    """查找需要修复的文件"""
    files_to_fix = []
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file.endswith('.hs'):
                file_path = os.path.join(root, file)
                try:
                    with open(file_path, 'r', encoding='utf-8') as f:
                        content = f.read()
                        # 检查是否使用了L.函数但没有导入qualified Data.List as L
                        if (re.search(r'L\.(null|length|head|foldl|all|map|maximum|isPrefixOf|isSuffixOf|isInfixOf|any|sum|take|drop|elem)', content) and
                            not re.search(r'import qualified Data\.List as L', content)):
                            files_to_fix.append(file_path)
                except Exception as e:
                    print(f"Error reading {file_path}: {e}")
    return files_to_fix

def main():
    """主函数"""
    test_dir = "test"
    if not os.path.exists(test_dir):
        print(f"Directory {test_dir} does not exist")
        sys.exit(1)
    
    files_to_fix = find_files_to_fix(test_dir)
    print(f"Found {len(files_to_fix)} files to fix")
    
    fixed_count = 0
    for file_path in files_to_fix:
        if fix_qualified_usage_in_file(file_path):
            fixed_count += 1
    
    print(f"Fixed {fixed_count} files")

if __name__ == "__main__":
    main()