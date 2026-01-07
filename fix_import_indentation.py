#!/usr/bin/env python3
import re
import os
import sys

def fix_import_indentation():
    """修复import语句的缩进"""
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    
    # 遍历所有测试文件
    for root, dirs, files in os.walk(test_dir):
        for file_name in files:
            if file_name.endswith(".hs"):
                file_path = os.path.join(root, file_name)
                try:
                    with open(file_path, 'r') as f:
                        content = f.read()
                    
                    modified = False
                    
                    # 修复import语句的缩进
                    lines = content.split('\n')
                    new_lines = []
                    in_module = False
                    
                    for line in lines:
                        stripped = line.strip()
                        if stripped.startswith('module '):
                            in_module = True
                            new_lines.append(line)
                        elif in_module and stripped.startswith('where'):
                            in_module = False
                            new_lines.append(line)
                        elif in_module:
                            # 在module声明和where之间，保持原样
                            new_lines.append(line)
                        elif stripped.startswith('import '):
                            # 修复import语句的缩进
                            new_lines.append('import ' + stripped[7:])
                            modified = True
                        else:
                            new_lines.append(line)
                    
                    if modified:
                        with open(file_path, 'w') as f:
                            f.write('\n'.join(new_lines))
                        print(f"Fixed import indentation in {file_path}")
                
                except Exception as e:
                    print(f"Error processing {file_path}: {e}")

if __name__ == "__main__":
    fix_import_indentation()