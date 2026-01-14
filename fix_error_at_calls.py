#!/usr/bin/env python3
"""
修复 errorAt 函数调用的脚本
根据 Core.hs 中的定义，errorAt 函数的签名是：
errorAt :: String -> ErrorSeverity -> Text -> ErrorLocation -> TypeError

但在测试文件中，它被错误地调用，参数顺序和类型都不匹配。
"""

import os
import re
import glob

def fix_error_at_calls(file_path):
    """修复文件中 errorAt 函数的错误调用"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    original_content = content
    
    # 错误模式1: errorAt message (T.pack message) unknownLocation
    # 应该是: errorAt message Error (T.pack message) unknownLocation
    pattern1 = r'errorAt\s+(\w+)\s+\(T\.pack\s+\1\)\s+(\w+)'
    replacement1 = r'errorAt \1 Error (T.pack \1) \2'
    content = re.sub(pattern1, replacement1, content)
    
    # 错误模式2: errorAt errId (T.pack msg) loc
    # 应该是: errorAt errId Error (T.pack msg) loc
    pattern2 = r'errorAt\s+(\w+)\s+\(T\.pack\s+(\w+)\)\s+(\w+)'
    replacement2 = r'errorAt \1 Error (T.pack \2) \3'
    content = re.sub(pattern2, replacement2, content)
    
    # 错误模式3: errorAt msg location
    # 应该是: errorAt msg Error (T.pack msg) location
    pattern3 = r'errorAt\s+(\w+)\s+(\w+)'
    replacement3 = r'errorAt \1 Error (T.pack \1) \2'
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