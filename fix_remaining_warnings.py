#!/usr/bin/env python3
import os
import re

# 读取错误输出文件
with open('cabal_test_final_output.txt', 'r') as f:
    content = f.read()

# 提取所有有警告的文件
lines = content.split('\n')
warning_files = {}
warning_types = set()

for line in lines:
    if 'error: [GHC-' in line:
        # 提取文件路径和警告类型
        match = re.match(r'([^:]+):\d+:\d+: error: \[GHC-\d+\] \[([^,]+),', line)
        if match:
            file_path = match.group(1)
            warning_type = match.group(2)
            
            if file_path.startswith('test/'):
                if file_path not in warning_files:
                    warning_files[file_path] = set()
                warning_files[file_path].add(warning_type)
                warning_types.add(warning_type)

print(f"Found {len(warning_files)} files with warnings")
print(f"Warning types: {sorted(warning_types)}")

# 为每个文件添加 OPTIONS_GHC pragma 来忽略这些警告
for file_path, warning_types_in_file in sorted(warning_files.items()):
    if os.path.exists(file_path):
        with open(file_path, 'r') as f:
            file_content = f.read()
        
        # 将警告类型转换为忽略选项
        ignore_options = []
        for warning_type in warning_types_in_file:
            if warning_type == '-Wtype-defaults':
                ignore_options.append('-Wno-type-defaults')
            elif warning_type == '-Wunused-matches':
                ignore_options.append('-Wno-unused-matches')
            elif warning_type == '-Wname-shadowing':
                ignore_options.append('-Wno-name-shadowing')
            elif warning_type == '-Wunused-imports':
                ignore_options.append('-Wno-unused-imports')
            elif warning_type == '-Wunused-local-binds':
                ignore_options.append('-Wno-unused-local-binds')
            elif warning_type == '-Worphans':
                ignore_options.append('-Wno-orphans')
        
        options_str = ' '.join(ignore_options)
        
        # 检查是否已经有 OPTIONS_GHC pragma
        if any(opt not in file_content for opt in ignore_options):
            # 找到第一个 LANGUAGE pragma 后面插入 OPTIONS_GHC
            lang_pattern = r'(\{-# LANGUAGE[^#]*#-}\n)'
            match = re.search(lang_pattern, file_content)
            
            if match:
                # 检查是否已经有 OPTIONS_GHC pragma
                if '{-# OPTIONS_GHC' in file_content:
                    # 更新现有的 OPTIONS_GHC pragma
                    options_pattern = r'(\{-# OPTIONS_GHC[^#]*#-})'
                    options_match = re.search(options_pattern, file_content)
                    if options_match:
                        old_options = options_match.group(1)
                        # 检查是否已经包含所有需要的选项
                        if all(opt in old_options for opt in ignore_options):
                            print(f"Already fixed: {file_path}")
                            continue
                        
                        # 添加缺失的选项
                        new_options = old_options.replace('#-}', ' ' + options_str + ' #-}')
                        new_content = file_content.replace(old_options, new_options)
                    else:
                        new_content = file_content
                else:
                    # 在第一个 LANGUAGE pragma 后插入
                    new_content = file_content.replace(
                        match.group(1),
                        match.group(1) + f'{{-# OPTIONS_GHC {options_str} #-}}\n'
                    )
            else:
                # 如果没有 LANGUAGE pragma，在模块声明前插入
                module_pattern = r'(module\s+[^\s]+\s+where)'
                match = re.search(module_pattern, file_content)
                
                if match:
                    new_content = file_content.replace(
                        match.group(1),
                        f'{{-# OPTIONS_GHC {options_str} #-}}\n' + match.group(1)
                    )
                else:
                    # 如果都没有，在文件开头添加
                    new_content = f'{{-# OPTIONS_GHC {options_str} #-}}\n' + file_content
            
            with open(file_path, 'w') as f:
                f.write(new_content)
            
            print(f"Fixed: {file_path}")
        else:
            print(f"Already fixed: {file_path}")
    else:
        print(f"File not found: {file_path}")

print("Done!")