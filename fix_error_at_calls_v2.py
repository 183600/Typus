#!/usr/bin/env python3
"""
修复 errorAt 函数调用的脚本 (第二版)
根据 Core.hs 中的定义，errorAt 函数的签名是：
errorAt :: String -> ErrorSeverity -> Text -> ErrorLocation -> TypeError
"""

import os
import re
import glob

def fix_error_at_calls(file_path):
    """修复文件中 errorAt 函数的错误调用"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    original_content = content
    
    # 修复重复参数的问题: errorAt message Error (T.pack message) Error (T.pack message) unknownLocation
    # 应该是: errorAt message Error (T.pack message) unknownLocation
    pattern1 = r'errorAt\s+(\w+)\s+Error\s+\(T\.pack\s+\1\)\s+Error\s+\(T\.pack\s+\1\)\s+(\w+)'
    replacement1 = r'errorAt \1 Error (T.pack \1) \2'
    content = re.sub(pattern1, replacement1, content)
    
    # 修复另一种重复参数的情况: errorAt errId Error (T.pack msg) Error (T.pack msg) loc
    # 应该是: errorAt errId Error (T.pack msg) loc
    pattern2 = r'errorAt\s+(\w+)\s+Error\s+\(T\.pack\s+(\w+)\)\s+Error\s+\(T\.pack\s+\2\)\s+(\w+)'
    replacement2 = r'errorAt \1 Error (T.pack \2) \3'
    content = re.sub(pattern2, replacement2, content)
    
    # 还有一种情况: errorAt message severity (T.pack message) unknownLocation
    # 这个是正确的，不需要修改
    
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