#!/usr/bin/env python3
"""
修复cabal文件中的格式问题
"""
import re
from pathlib import Path

def fix_cabal_format():
    """修复cabal文件的格式问题"""
    cabal_path = Path("typus.cabal")
    
    with open(cabal_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # 查找test-suite部分
    test_suite_match = re.search(r'(test-suite\s+typus-test.*?other-modules:\s*\n)((?:.*\n)*?)(\s*\w+:)', content, re.DOTALL)
    if not test_suite_match:
        print("未找到test-suite部分")
        return
    
    prefix = test_suite_match.group(1)
    modules_section = test_suite_match.group(2)
    suffix = test_suite_match.group(3)
    
    # 处理模块列表
    lines = modules_section.split('\n')
    new_lines = []
    in_module_list = False
    
    for line in lines:
        stripped = line.strip()
        
        # 跳过空行和注释
        if not stripped or stripped.startswith('--'):
            new_lines.append(line)
            continue
            
        # 检查是否是模块行
        if stripped.startswith('Test.Unit.'):
            in_module_list = True
            # 确保正确的缩进和逗号
            if not stripped.endswith(','):
                stripped += ','
            new_lines.append(f'        {stripped}')
        elif in_module_list and stripped.startswith(','):
            # 修复以逗号开头的行
            module_name = stripped[1:].strip()
            if module_name.startswith('Test.Unit.'):
                new_lines.append(f'        {module_name},')
            else:
                new_lines.append(line)
        else:
            new_lines.append(line)
    
    # 重新组合内容
    new_modules_section = '\n'.join(new_lines)
    
    # 替换原内容
    new_content = content[:test_suite_match.start(2)] + new_modules_section + content[test_suite_match.end(2):]
    
    # 写回文件
    with open(cabal_path, 'w', encoding='utf-8') as f:
        f.write(new_content)
    
    print("已修复cabal文件格式")

def main():
    """主函数"""
    fix_cabal_format()

if __name__ == "__main__":
    main()