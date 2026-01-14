#!/usr/bin/env python3
"""
修复 errorAt 函数调用的脚本 (第四版)
处理所有剩余的错误
"""

import os
import re
import glob

def fix_error_at_calls(file_path):
    """修复文件中 errorAt 函数的错误调用"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    original_content = content
    
    # 修复语法错误: error = errorAt message Error location result = execState (addError error) []
    # 应该是: error = errorAt message Error (T.pack message) location
    #         result = execState (addError error) []
    pattern1 = r'(\s+error = errorAt\s+\w+\s+\w+\s+\w+)\s+result = execState'
    replacement1 = r'\1\n      result = execState'
    content = re.sub(pattern1, replacement1, content)
    
    # 修复 errorAt 调用: errorAt (errId ++ show i) msg loc {line = i}
    # 应该是: errorAt (errId ++ show i) Error msg loc {line = i}
    pattern2 = r'errorAt\s+\([^)]+\)\s+(\w+)\s+(\w+)\s+(\w+)\s+{line = i}'
    replacement2 = r'errorAt (\1) Error \2 \3 {line = i}'
    content = re.sub(pattern2, replacement2, content)
    
    # 修复 errorAt 调用: errorAt message Error location
    # 应该是: errorAt message Error (T.pack message) location
    pattern3 = r'errorAt\s+(\w+)\s+(\w+)\s+(\w+)'
    replacement3 = r'errorAt \1 \2 (T.pack \1) \3'
    content = re.sub(pattern3, replacement3, content)
    
    # 检查是否有变化
    if content != original_content:
        with open(file_path, 'w') as f:
            f.write(content)
        print(f"修复了 {file_path} 中的 errorAt 调用")
        return True
    return False

def main():
    """主函数"""
    # 查找所有测试文件
    test_files = glob.glob("/home/runner/work/Typus/Typus/test/Test/Unit/*.hs")
    
    fixed_count = 0
    for file_path in test_files:
        if fix_error_at_calls(file_path):
            fixed_count += 1
    
    print(f"总共修复了 {fixed_count} 个文件")

if __name__ == "__main__":
    main()