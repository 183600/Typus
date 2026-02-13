#!/usr/bin/env python3

# 完全重新修复所有 Ownership 类型错误

import re

def comprehensive_fix():
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'r') as f:
        content = f.read()
    
    # 找到所有 Ownership 函数并重新写它们
    # 首先找到所有以 prop_ownership_ 开头的函数
    
    # 分割成行
    lines = content.split('\n')
    fixed_lines = []
    i = 0
    
    while i < len(lines):
        line = lines[i]
        
        # 如果我们遇到一个 ownership 函数
        if line.strip().startswith('prop_ownership_') and ':: Property' in line:
            # 添加函数签名
            fixed_lines.append(line)
            i += 1
            
            # 添加函数实现行
            if i < len(lines):
                impl_line = lines[i]
                fixed_lines.append(impl_line)
                i += 1
                
                # 现在我们需要找到并修复整个函数体
                # 继续添加行，直到遇到下一个函数或文件结束
                function_body = []
                while i < len(lines):
                    current_line = lines[i]
                    
                    # 如果遇到下一个函数定义，停止
                    if (current_line.strip().startswith('prop_') and 
                        (':: Property' in current_line or 
                         current_line.strip().startswith('-- |'))):
                        break
                    
                    # 如果我们遇到 O.analyzeOwnership，我们需要修复后面的模式
                    if 'errors = O.analyzeOwnership' in current_line:
                        function_body.append(current_line)
                        i += 1
                        
                        # 跳过接下来的错误模式，直到我们找到正确的结束
                        while i < len(lines):
                            next_line = lines[i]
                            if 'in case errors of:' in next_line:
                                # 跳过 case 语句
                                i += 1
                                # 跳过 Right 和 Left 行
                                while i < len(lines) and ('Right _ ->' in lines[i] or 'Left _ ->' in lines[i]):
                                    i += 1
                                # 添加正确的行
                                function_body.append('       in property $ null errors')
                                break
                            elif 'in property $ null errors' in next_line:
                                # 已经是正确的了
                                function_body.append(next_line)
                                i += 1
                                break
                            else:
                                # 意外的行，添加并继续
                                function_body.append(next_line)
                                i += 1
                    else:
                        function_body.append(current_line)
                        i += 1
                
                # 添加函数体到固定行
                fixed_lines.extend(function_body)
        else:
            fixed_lines.append(line)
            i += 1
    
    # 写回文件
    new_content = '\n'.join(fixed_lines)
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'w') as f:
        f.write(new_content)
    
    print("Comprehensive fix completed")

if __name__ == "__main__":
    comprehensive_fix()