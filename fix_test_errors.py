#!/usr/bin/env python3
import re
import os
import sys

def fix_property_bool_errors(file_path):
    """修复将布尔表达式转换为属性的错误"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # 修复 True -> property True
    content = re.sub(r'(\s+)Left _ -> True\s*--', r'\1Left _ -> property True  --', content)
    content = re.sub(r'(\s+)Right _ -> True\s*--', r'\1Right _ -> property True  --', content)
    content = re.sub(r'(\s+)Left _ -> True\s*$', r'\1Left _ -> property True', content)
    content = re.sub(r'(\s+)Right _ -> True\s*$', r'\1Right _ -> property True', content)
    
    # 修复其他布尔表达式
    content = re.sub(r'(\s+)else True\s*$', r'\1else property True', content)
    content = re.sub(r'(\s+)in L\.all (.+)$', r'\1in property $ L.all \2', content)
    content = re.sub(r'(\s+)in L\.length (.+) <= L\.length (.+)$', r'\1in property $ L.length \2 <= L.length \3', content)
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed property/bool errors in {file_path}")

def fix_codeblock_constructor(file_path):
    """修复CodeBlock构造函数调用"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # 修复只有两个参数的CodeBlock调用
    content = re.sub(r'CodeBlock ([^,]+), ([^)]+)\)', r'CodeBlock \1 \2 (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0))', content)
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed CodeBlock constructor in {file_path}")

def fix_typusfile_constructor(file_path):
    """修复TypusFile构造函数调用"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # 修复只有三个参数的TypusFile调用
    content = re.sub(r'TypusFile ([^,]+), ([^,]+), ([^)]+)\)', r'TypusFile \1 \2 \3 []', content)
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed TypusFile constructor in {file_path}")

def fix_comparison_operators(file_path):
    """修复比较运算符错误"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # 在条件语句中将 === 替换为 ==
    content = re.sub(r'(if|then|else) (.+?) === (.+?) &&', r'\1 \2 == \3 &&', content)
    content = re.sub(r'&& (.+?) === (.+?)\s*$', r'&& \1 == \2', content)
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed comparison operators in {file_path}")

def main():
    # 获取所有测试文件
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    
    # 需要修复的文件列表（从错误输出中提取）
    error_files = [
        "CoreParserSpec.hs",
        "CorePropertiesQuickCheckSpec.hs",
        "CoreSourceLocationSpec.hs",
        "CoreUtilsEssentialSpec.hs",
        "CoreUtilsSpec.hs",
        "CrossModuleIntegrationQuickCheckSpec.hs",
        "CustomCompilerQuickCheckSpec.hs"
    ]
    
    for file_name in error_files:
        file_path = os.path.join(test_dir, file_name)
        if os.path.exists(file_path):
            try:
                fix_property_bool_errors(file_path)
                fix_codeblock_constructor(file_path)
                fix_typusfile_constructor(file_path)
                fix_comparison_operators(file_path)
            except Exception as e:
                print(f"Error fixing {file_path}: {e}")

if __name__ == "__main__":
    main()