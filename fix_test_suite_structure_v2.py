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
        lines = f.readlines()
    
    # 找到test-suite部分
    in_test_suite = False
    test_suite_start = -1
    first_other_modules_line = -1
    second_other_modules_line = -1
    build_depends_line = -1
    
    for i, line in enumerate(lines):
        if line.strip() == 'test-suite typus-test':
            in_test_suite = True
            test_suite_start = i
        elif in_test_suite and line.strip().startswith('other-modules:'):
            if first_other_modules_line == -1:
                first_other_modules_line = i
            elif second_other_modules_line == -1:
                second_other_modules_line = i
        elif in_test_suite and line.strip().startswith('build-depends:'):
            build_depends_line = i
            break
    
    if first_other_modules_line == -1:
        print("未找到第一个other-modules字段")
        return
    
    if second_other_modules_line == -1:
        print("没有找到重复的other-modules")
        return
    
    if build_depends_line == -1:
        print("未找到build-depends字段")
        return
    
    # 修复第一个other-modules部分
    # 确保最后一个模块没有逗号
    for i in range(first_other_modules_line + 1, second_other_modules_line):
        line = lines[i].strip()
        if line and not line.startswith('--') and line.endswith(','):
            # 检查是否是最后一个非注释、非空行
            j = i + 1
            while j < second_other_modules_line:
                next_line = lines[j].strip()
                if next_line and not next_line.startswith('--'):
                    break
                j += 1
            if j >= second_other_modules_line:
                # 这是最后一个模块
                lines[i] = line[:-1] + '\n'
    
    # 删除第二个other-modules行及其后面的模块，直到build-depends
    del lines[second_other_modules_line:build_depends_line]
    
    # 写回文件
    with open(cabal_path, 'w', encoding='utf-8') as f:
        f.writelines(lines)
    
    print("已修复test-suite部分的结构")

def main():
    """主函数"""
    fix_test_suite_structure()

if __name__ == "__main__":
    main()