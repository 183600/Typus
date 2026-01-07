#!/usr/bin/env python3
"""
修复test-suite部分other-modules字段的结构问题
"""
import re
from pathlib import Path

def fix_test_suite_structure():
    """修复test-suite部分的结构"""
    cabal_path = Path("typus.cabal")
    
    with open(cabal_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # 查找test-suite部分
    test_suite_start = content.find('test-suite typus-test')
    if test_suite_start == -1:
        print("未找到test-suite部分")
        return
    
    # 查找test-suite部分的结束（下一个section的开始）
    test_suite_end = content.find('\nbenchmark', test_suite_start)
    if test_suite_end == -1:
        test_suite_end = len(content)
    
    test_suite_content = content[test_suite_start:test_suite_end]
    
    # 查找第一个other-modules
    first_other_modules = test_suite_content.find('other-modules:')
    if first_other_modules == -1:
        print("未找到other-modules字段")
        return
    
    # 查找第二个other-modules
    second_other_modules = test_suite_content.find('other-modules:', first_other_modules + 1)
    if second_other_modules == -1:
        print("没有找到重复的other-modules")
        return
    
    # 查找build-depends字段
    build_depends_pos = test_suite_content.find('build-depends:', second_other_modules)
    if build_depends_pos == -1:
        print("未找到build-depends字段")
        return
    
    # 提取第一个other-modules的内容
    first_part = test_suite_content[:first_other_modules]
    modules_start = first_other_modules + len('other-modules:')
    modules_content = test_suite_content[modules_start:second_other_modules].strip()
    
    # 修复模块内容，确保最后一个模块没有逗号
    modules_lines = modules_content.split('\n')
    fixed_modules = []
    for line in modules_lines:
        line = line.strip()
        if line and not line.startswith('--'):
            if line.endswith(','):
                # 检查是否是最后一个模块
                if line == modules_lines[-1].strip() or (len(modules_lines) > 1 and line == modules_lines[-2].strip() and modules_lines[-1].strip().startswith('--')):
                    line = line[:-1]
        fixed_modules.append(line)
    
    # 重新组合
    new_modules_content = '\n        '.join(fixed_modules)
    
    # 提取剩余部分
    remaining_part = test_suite_content[build_depends_pos:]
    
    # 重新组合test-suite内容
    new_test_suite_content = (
        first_part + 
        'other-modules:\n        ' + 
        new_modules_content + 
        '\n    ' + 
        remaining_part
    )
    
    # 替换原内容
    new_content = content[:test_suite_start] + new_test_suite_content + content[test_suite_end:]
    
    # 写回文件
    with open(cabal_path, 'w', encoding='utf-8') as f:
        f.write(new_content)
    
    print("已修复test-suite部分的结构")

def main():
    """主函数"""
    fix_test_suite_structure()

if __name__ == "__main__":
    main()