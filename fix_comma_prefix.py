#!/usr/bin/env python3
"""
修复cabal文件中所有的逗号开头问题
"""
import re
from pathlib import Path

def fix_comma_prefix():
    """修复所有以逗号开头的模块行"""
    cabal_path = Path("typus.cabal")
    
    with open(cabal_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # 查找test-suite部分
    test_suite_match = re.search(r'(test-suite\s+typus-test.*?other-modules:)(.*?)(?=\s+\w+:)', content, re.DOTALL)
    if not test_suite_match:
        print("未找到test-suite部分")
        return
    
    prefix = test_suite_match.group(1)
    modules_section = test_suite_match.group(2)
    
    # 修复以逗号开头的行
    # 匹配: 任意空白 + , + 任意空白 + 模块名
    modules_section = re.sub(r'\s*,\s*([a-zA-Z])', r', \1', modules_section)
    # 匹配: 行首空白 + , + 空白 + Test.Unit.
    modules_section = re.sub(r'^(\s+),\s*(Test\.Unit\.)', r'\1\2', modules_section, flags=re.MULTILINE)
    
    # 重新组合内容
    new_content = content[:test_suite_match.start(2)] + modules_section + content[test_suite_match.end(2):]
    
    # 写回文件
    with open(cabal_path, 'w', encoding='utf-8') as f:
        f.write(new_content)
    
    print("已修复所有逗号开头问题")

def main():
    """主函数"""
    fix_comma_prefix()

if __name__ == "__main__":
    main()