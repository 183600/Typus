#!/usr/bin/env python3
"""
从cabal文件中移除不存在的测试模块
"""
import os
import re
from pathlib import Path

def find_existing_test_modules():
    """查找所有实际存在的测试模块"""
    test_dir = Path("test/Test/Unit")
    existing_modules = set()
    
    for file_path in test_dir.glob("*.hs"):
        # 将文件路径转换为模块名
        module_name = str(file_path.relative_to(test_dir).with_suffix(''))
        existing_modules.add(module_name)
    
    return existing_modules

def extract_modules_from_cabal(cabal_content):
    """从cabal文件内容中提取所有测试模块"""
    modules = set()
    
    # 查找test-suite部分
    test_suite_match = re.search(r'test-suite\s+typus-test.*?^(?=\s*\w|\s*$)', cabal_content, re.MULTILINE | re.DOTALL)
    if test_suite_match:
        test_suite_content = test_suite_match.group(0)
        
        # 查找other-modules部分
        other_modules_match = re.search(r'other-modules:\s*\n((?:\s+[^,\n]+,\s*\n?)*)', test_suite_content)
        if other_modules_match:
            modules_text = other_modules_match.group(1)
            # 提取模块名
            for line in modules_text.split('\n'):
                line = line.strip()
                if line and not line.startswith('--'):
                    module_name = line.rstrip(',').strip()
                    if module_name:
                        modules.add(module_name)
    
    return modules

def update_cabal_file():
    """更新cabal文件，移除不存在的模块"""
    cabal_path = Path("typus.cabal")
    
    with open(cabal_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    existing_modules = find_existing_test_modules()
    print(f"找到 {len(existing_modules)} 个实际存在的测试模块")
    
    # 查找test-suite部分
    test_suite_pattern = r'(test-suite\s+typus-test.*?other-modules:\s*\n)((?:\s+[^,\n]+,\s*\n?)*)'
    
    def replace_other_modules(match):
        prefix = match.group(1)
        modules_section = match.group(2)
        
        # 保留存在的模块
        new_modules_section = []
        for line in modules_section.split('\n'):
            line = line.strip()
            if line and not line.startswith('--'):
                module_name = line.rstrip(',').strip()
                module_path = module_name.replace('.', '/') + '.hs'
                
                # 检查模块是否存在
                module_file = Path(f"test/Test/Unit/{module_path}")
                if module_file.exists():
                    new_modules_section.append(line)
                else:
                    print(f"移除不存在的模块: {module_name}")
            elif line.startswith('--'):
                new_modules_section.append(line)
        
        # 重新构建模块列表
        if new_modules_section:
            result = prefix + '\n        ' + ',\n        '.join(new_modules_section) + '\n'
        else:
            result = prefix + '\n'
        
        return result
    
    # 应用替换
    new_content = re.sub(test_suite_pattern, replace_other_modules, content, flags=re.MULTILINE | re.DOTALL)
    
    # 写回文件
    with open(cabal_path, 'w', encoding='utf-8') as f:
        f.write(new_content)
    
    print("已更新cabal文件，移除了不存在的模块")

def main():
    """主函数"""
    update_cabal_file()

if __name__ == "__main__":
    main()