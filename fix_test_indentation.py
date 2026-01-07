#!/usr/bin/env python3
"""
批量修复测试文件中的缩进和语法错误
"""

import os
import re

# 需要修复的文件列表
files_to_fix = [
    "/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerCrossAnalysisSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/BoundaryConditionsAdvanced2025Spec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/CabalBoundaryConditionsSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/CabalConcurrentParsingSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/CabalCrossModuleIntegrationSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/CabalEndToEndSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/CabalErrorRecoverySpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/CabalMemorySafetySpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/CabalPerformanceSpec.hs"
]

def fix_indentation_issues(content):
    """修复常见的缩进问题"""
    lines = content.split('\n')
    fixed_lines = []
    i = 0
    while i < len(lines):
        line = lines[i]
        
        # 修复 let 绑定的缩进问题
        if re.match(r'^\s+let\s+\w+\s*=', line):
            # 确保 let 有正确的缩进
            fixed_line = re.sub(r'^(\s+)let\s+', r'    let ', line)
            fixed_lines.append(fixed_line)
            
            # 检查下一行是否是记录语法
            if i + 1 < len(lines) and lines[i + 1].strip().startswith('{'):
                # 确保记录语法有正确的缩进
                record_line = lines[i + 1]
                fixed_record = re.sub(r'^(\s+)', r'        ', record_line)
                fixed_lines.append(fixed_record)
                i += 2
                continue
        # 修复 case 表达式的缩进问题
        elif re.match(r'^\s+case\s+\w+\s+of', line):
            # 确保 case 有正确的缩进
            fixed_line = re.sub(r'^(\s+)case\s+', r'    case ', line)
            fixed_lines.append(fixed_line)
        # 修复其他变量的缩进问题
        elif re.match(r'^\s+\w+\s*=', line) and not line.strip().startswith('let'):
            # 确保变量有正确的缩进
            fixed_line = re.sub(r'^(\s+)(\w+\s*=)', r'    \2', line)
            fixed_lines.append(fixed_line)
        else:
            fixed_lines.append(line)
        
        i += 1
    
    return '\n'.join(fixed_lines)

def fix_specific_issues(content, filename):
    """修复特定文件的问题"""
    # AnalyzerCrossAnalysisSpec.hs 特定修复
    if "AnalyzerCrossAnalysisSpec.hs" in filename:
        # 修复 let 前的空格问题
        content = re.sub(r'(\s+)let\s+symbolInfo', r'    let symbolInfo', content)
        # 修复记录语法的缩进
        content = re.sub(r'(\s+){\s+symbolName', r'        { symbolName', content)
        
    # BoundaryConditionsAdvanced2025Spec.hs 特定修复
    elif "BoundaryConditionsAdvanced2025Spec.hs" in filename:
        # 确保使用 Utils 模块的函数
        content = re.sub(r'normalizeIndentation', r'Utils.normalizeIndentation', content)
        content = re.sub(r'\btrim\b', r'Utils.trim', content)
    
    return content

def main():
    for file_path in files_to_fix:
        if os.path.exists(file_path):
            with open(file_path, 'r') as f:
                content = f.read()
            
            # 应用修复
            content = fix_indentation_issues(content)
            content = fix_specific_issues(content, file_path)
            
            with open(file_path, 'w') as f:
                f.write(content)
            
            print(f"Fixed {file_path}")
        else:
            print(f"File not found: {file_path}")

if __name__ == "__main__":
    main()