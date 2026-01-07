#!/usr/bin/env python3
"""
修复cabal文件中行内注释的问题
"""
import re
from pathlib import Path

def fix_inline_comments():
    """修复行内注释问题"""
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
    
    # 修复行内注释
    # 匹配: 模块名, -- 注释
    modules_section = re.sub(r'(Test\.Unit\.\w+),\s*--([^,\n]+)', r'\1,\n        -- \2', modules_section)
    
    # 重新组合内容
    new_content = content[:test_suite_match.start(2)] + modules_section + content[test_suite_match.end(2):]
    
    # 写回文件
    with open(cabal_path, 'w', encoding='utf-8') as f:
        f.write(new_content)
    
    print("已修复行内注释问题")

def main():
    """主函数"""
    fix_inline_comments()

if __name__ == "__main__":
    main()