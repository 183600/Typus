#!/usr/bin/env python3
"""
修复cabal文件中的重复模块问题
"""

import re
from collections import defaultdict

def find_duplicate_modules(cabal_file):
    """查找cabal文件中的重复模块"""
    with open(cabal_file, 'r') as f:
        content = f.read()
    
    # 查找test-suite部分
    test_suite_pattern = r'test-suite\s+(\w+).*?other-modules:(.*?)(?=\n\s|\n\n|\Z)'
    matches = re.findall(test_suite_pattern, content, re.DOTALL)
    
    all_modules = defaultdict(list)
    
    for suite_name, modules_section in matches:
        # 提取模块名
        modules = re.findall(r'([A-Z][A-Za-z0-9.]*)', modules_section)
        for module in modules:
            if module.startswith('Test.Unit'):
                all_modules[module].append(suite_name)
    
    # 找出重复的模块
    duplicates = {module: suites for module, suites in all_modules.items() if len(suites) > 1}
    
    return duplicates

def fix_duplicate_modules(cabal_file, duplicates):
    """修复重复模块问题"""
    with open(cabal_file, 'r') as f:
        content = f.read()
    
    # 对于每个重复模块，只保留在typus-test中的定义
    for module, suites in duplicates.items():
        if 'typus-test' in suites and 'typus-enhanced-test' in suites:
            # 从typus-enhanced-test中移除该模块
            pattern = r'(test-suite\s+typus-enhanced-test.*?other-modules:.*?)' + re.escape(module) + r'(.*?)(?=\n\s|\n\n|\Z)'
            content = re.sub(pattern, r'\1\2', content, flags=re.DOTALL)
    
    # 清理多余的空行和逗号
    content = re.sub(r',\s*\n\s*\n', ',\n', content)
    content = re.sub(r'\n\s*\n\s*\n', '\n\n', content)
    
    with open(cabal_file, 'w') as f:
        f.write(content)
    
    print(f"已修复 {len(duplicates)} 个重复模块")

if __name__ == "__main__":
    cabal_file = "/home/runner/work/Typus/Typus/typus.cabal"
    
    print("查找重复模块...")
    duplicates = find_duplicate_modules(cabal_file)
    
    if duplicates:
        print("发现以下重复模块:")
        for module, suites in duplicates.items():
            print(f"  {module}: {', '.join(suites)}")
        
        print("\n修复重复模块...")
        fix_duplicate_modules(cabal_file, duplicates)
        print("修复完成!")
    else:
        print("没有发现重复模块")
