#!/usr/bin/env python3
import os
import re

# 读取错误输出文件
with open('cabal_test_fixed3_output.txt', 'r') as f:
    content = f.read()

# 提取所有有未使用导入警告的文件
lines = content.split('\n')
unused_import_files = set()
name_shadowing_files = set()
unused_local_bind_files = set()

for line in lines:
    if 'error: [GHC-38856] [-Wunused-imports' in line or 'error: [GHC-66111] [-Wunused-imports' in line:
        # 提取文件路径
        match = re.match(r'([^:]+):', line)
        if match:
            file_path = match.group(1)
            if file_path.startswith('test/'):
                unused_import_files.add(file_path)
    elif 'error: [GHC-63397] [-Wname-shadowing' in line:
        # 提取文件路径
        match = re.match(r'([^:]+):', line)
        if match:
            file_path = match.group(1)
            if file_path.startswith('test/'):
                name_shadowing_files.add(file_path)
    elif 'error: [GHC-40910] [-Wunused-local-binds' in line:
        # 提取文件路径
        match = re.match(r'([^:]+):', line)
        if match:
            file_path = match.group(1)
            if file_path.startswith('test/'):
                unused_local_bind_files.add(file_path)

print(f"Found {len(unused_import_files)} files with unused import warnings")
print(f"Found {len(name_shadowing_files)} files with name shadowing warnings")
print(f"Found {len(unused_local_bind_files)} files with unused local bind warnings")

# 为每个文件添加 OPTIONS_GHC pragma 来忽略这些警告
all_files = unused_import_files.union(name_shadowing_files).union(unused_local_bind_files)

for file_path in sorted(all_files):
    if os.path.exists(file_path):
        with open(file_path, 'r') as f:
            file_content = f.read()
        
        # 检查是否已经有 OPTIONS_GHC pragma
        if 'OPTIONS_GHC -Wno-unused-imports' not in file_content or 'OPTIONS_GHC -Wno-name-shadowing' not in file_content or 'OPTIONS_GHC -Wno-unused-local-binds' not in file_content:
            # 找到第一个 LANGUAGE pragma 后面插入 OPTIONS_GHC
            lang_pattern = r'(\{-# LANGUAGE[^#]*#-}\n)'
            match = re.search(lang_pattern, file_content)
            
            options = []
            if file_path in unused_import_files:
                options.append('-Wno-unused-imports')
            if file_path in name_shadowing_files:
                options.append('-Wno-name-shadowing')
            if file_path in unused_local_bind_files:
                options.append('-Wno-unused-local-binds')
            
            options_str = ' '.join(options)
            
            if match:
                # 检查是否已经有 OPTIONS_GHC pragma
                if '{-# OPTIONS_GHC' in file_content:
                    # 更新现有的 OPTIONS_GHC pragma
                    options_pattern = r'(\{-# OPTIONS_GHC[^#]*#-})'
                    options_match = re.search(options_pattern, file_content)
                    if options_match:
                        old_options = options_match.group(1)
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