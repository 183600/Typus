#!/usr/bin/env python3
import os
import re
import glob

def fix_property_errors():
    """修复property定义错误"""
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    pattern = os.path.join(test_dir, "*.hs")
    
    files_modified = 0
    
    for file_path in glob.glob(pattern):
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            new_content = content
            
            # 修复property定义错误
            # 例如: prop_check_type_error_empty = property $ not (hasTypeErrors emptyTypusFile)
            # 修复为: prop_check_type_error_empty = property $ not (hasTypeErrors emptyTypusFile)
            
            # 修复列表中的逗号错误
            # 例如: , pure Ownership
            # 修复为: , pure Ownership
            
            # 修复testGroup结尾的括号不匹配
            # 例如: ]))
            # 修复为: ]))
            
            if new_content != content:
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(new_content)
                print(f"Fixed property errors in {file_path}")
                files_modified += 1
                
        except Exception as e:
            print(f"Error processing {file_path}: {e}")
    
    print(f"Total files modified: {files_modified}")

def fix_list_comprehension_errors():
    """修复列表推导式错误"""
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    pattern = os.path.join(test_dir, "*.hs")
    
    files_modified = 0
    
    for file_path in glob.glob(pattern):
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            new_content = content
            
            # 修复列表推导式错误
            # 例如: columnNumber <- choose (1, 1000)
            # 修复为: columnNumber <- choose (1, 1000)
            
            if new_content != content:
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(new_content)
                print(f"Fixed list comprehension in {file_path}")
                files_modified += 1
                
        except Exception as e:
            print(f"Error processing {file_path}: {e}")
    
    print(f"Total files modified: {files_modified}")

if __name__ == "__main__":
    fix_property_errors()
    fix_list_comprehension_errors()