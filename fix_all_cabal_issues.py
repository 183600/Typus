#!/usr/bin/env python3
"""
全面修复cabal文件中的格式问题
"""
import re
from pathlib import Path

def fix_all_cabal_issues():
    """修复所有cabal文件格式问题"""
    cabal_path = Path("typus.cabal")
    
    with open(cabal_path, 'r', encoding='utf-8') as f:
        lines = f.readlines()
    
    new_lines = []
    in_test_suite = False
    in_other_modules = False
    prev_line_was_module = False
    
    for line in lines:
        stripped = line.strip()
        
        # 检测是否进入test-suite部分
        if line.startswith('test-suite typus-test'):
            in_test_suite = True
            new_lines.append(line)
            continue
        
        # 检测是否进入other-modules部分
        if in_test_suite and line.strip().startswith('other-modules:'):
            in_other_modules = True
            new_lines.append(line)
            continue
            
        # 检测是否离开other-modules部分
        if in_other_modules and re.match(r'^\s+\w+:', line):
            in_other_modules = False
            prev_line_was_module = False
        
        # 处理other-modules部分的内容
        if in_other_modules:
            # 跳过空行
            if not stripped:
                new_lines.append(line)
                continue
                
            # 处理注释行
            if stripped.startswith('--'):
                # 修复注释行中的逗号
                cleaned = re.sub(r'^\s*,\s*', '-- ', stripped)
                new_lines.append(f'        {cleaned}\n')
                prev_line_was_module = False
                continue
            
            # 处理模块行
            if stripped.startswith('Test.Unit.'):
                # 确保模块行以逗号结尾（除非是最后一个）
                if not stripped.endswith(',') and not stripped.endswith('Spec'):
                    stripped += ','
                new_lines.append(f'        {stripped}\n')
                prev_line_was_module = True
                continue
            
            # 处理以逗号开头的行
            if stripped.startswith(',') and 'Test.Unit.' in stripped:
                module_name = stripped[1:].strip()
                if module_name.startswith('Test.Unit.'):
                    if not module_name.endswith(','):
                        module_name += ','
                    new_lines.append(f'        {module_name}\n')
                    prev_line_was_module = True
                    continue
            
            # 其他行保持原样
            new_lines.append(line)
        else:
            new_lines.append(line)
    
    # 写回文件
    with open(cabal_path, 'w', encoding='utf-8') as f:
        f.writelines(new_lines)
    
    print("已修复所有cabal文件格式问题")

def main():
    """主函数"""
    fix_all_cabal_issues()

if __name__ == "__main__":
    main()