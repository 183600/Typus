#!/usr/bin/env python3
"""
修复 errorAt 函数调用的脚本 (第三版)
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
    
    # 修复模式1: errorAt "test" (T.pack "message") (ErrorLocation ...)
    # 应该是: errorAt "test" Error (T.pack "message") (ErrorLocation ...)
    pattern1 = r'errorAt\s+"([^"]+)"\s+\(T\.pack\s+"([^"]+)"\)\s+\(ErrorLocation([^)]+)\)'
    replacement1 = r'errorAt "\1" Error (T.pack "\2") (ErrorLocation\3)'
    content = re.sub(pattern1, replacement1, content)
    
    # 修复模式2: errorAt errId (T.pack msg) loc
    # 应该是: errorAt errId Error (T.pack msg) loc
    pattern2 = r'errorAt\s+(\w+)\s+\(T\.pack\s+(\w+)\)\s+(\w+)'
    replacement2 = r'errorAt \1 Error (T.pack \2) \3'
    content = re.sub(pattern2, replacement2, content)
    
    # 修复模式3: errorAt errId Error (T.pack errId) msg loc
    # 应该是: errorAt errId Error msg loc
    pattern3 = r'errorAt\s+(\w+)\s+Error\s+\(T\.pack\s+\1\)\s+(\w+)\s+(\w+)'
    replacement3 = r'errorAt \1 Error \2 \3'
    content = re.sub(pattern3, replacement3, content)
    
    # 修复模式4: errorAt ("test" ++ show i) (T.pack "message") (ErrorLocation ...)
    # 应该是: errorAt ("test" ++ show i) Error (T.pack "message") (ErrorLocation ...)
    pattern4 = r'errorAt\s+\(([^)]+)\)\s+\(T\.pack\s+"([^"]+)"\)\s+\(ErrorLocation([^)]+)\)'
    replacement4 = r'errorAt (\1) Error (T.pack "\2") (ErrorLocation\3)'
    content = re.sub(pattern4, replacement4, content)
    
    # 修复模式5: errorAt "syntax" (T.pack "syntax error") (ErrorLocation ...)
    # 应该是: errorAt "syntax" Error (T.pack "syntax error") (ErrorLocation ...)
    pattern5 = r'errorAt\s+"([^"]+)"\s+\(T\.pack\s+"([^"]+)"\)\s+\(ErrorLocation([^)]+)\)'
    replacement5 = r'errorAt "\1" Error (T.pack "\2") (ErrorLocation\3)'
    content = re.sub(pattern5, replacement5, content)
    
    # 修复模式6: errorAt "test" (T.pack "test error") (ErrorLocation ...)
    # 应该是: errorAt "test" Error (T.pack "test error") (ErrorLocation ...)
    pattern6 = r'errorAt\s+"test"\s+\(T\.pack\s+"test error"\)\s+\(ErrorLocation([^)]+)\)'
    replacement6 = r'errorAt "test" Error (T.pack "test error") (ErrorLocation\1)'
    content = re.sub(pattern6, replacement6, content)
    
    # 修复模式7: errorWithSuggestions "TEST" msg suggs loc
    # 应该是: errorWithSuggestions "TEST" Error msg suggs loc
    pattern7 = r'errorWithSuggestions\s+"([^"]+)"\s+(\w+)\s+(\w+)\s+(\w+)'
    replacement7 = r'errorWithSuggestions "\1" Error \2 \3 \4'
    content = re.sub(pattern7, replacement7, content)
    
    # 修复 warningAt 和 infoAt 的调用
    # warningAt 和 infoAt 的签名是: String -> Text -> ErrorLocation -> TypeError
    # 不需要修改
    
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