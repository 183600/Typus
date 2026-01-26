#!/usr/bin/env python3
import os
import re

# 读取错误输出文件
with open('cabal_test_fixed2_output.txt', 'r') as f:
    content = f.read()

# 提取所有有孤儿实例警告的文件
pattern = r'([^:]+):\d+:\d+: error: \[GHC-90177\] \[-Worphans'
files = set(re.findall(pattern, content))

print(f"Found {len(files)} files with orphan instance warnings")

# 为每个文件添加 OPTIONS_GHC pragma
for file_path in files:
    if os.path.exists(file_path):
        with open(file_path, 'r') as f:
            file_content = f.read()
        
        # 检查是否已经有 OPTIONS_GHC pragma
        if 'OPTIONS_GHC -Wno-orphans' not in file_content:
            # 找到第一个 LANGUAGE pragma 后面插入 OPTIONS_GHC
            lang_pattern = r'(\{-# LANGUAGE[^#]*#-}\n)'
            match = re.search(lang_pattern, file_content)
            
            if match:
                # 在第一个 LANGUAGE pragma 后插入
                new_content = file_content.replace(
                    match.group(1),
                    match.group(1) + '{-# OPTIONS_GHC -Wno-orphans #-}\n'
                )
            else:
                # 如果没有 LANGUAGE pragma，在模块声明前插入
                module_pattern = r'(module\s+[^\s]+\s+where)'
                match = re.search(module_pattern, file_content)
                
                if match:
                    new_content = file_content.replace(
                        match.group(1),
                        '{-# OPTIONS_GHC -Wno-orphans #-}\n' + match.group(1)
                    )
                else:
                    # 如果都没有，在文件开头添加
                    new_content = '{-# OPTIONS_GHC -Wno-orphans #-}\n' + file_content
            
            with open(file_path, 'w') as f:
                f.write(new_content)
            
            print(f"Fixed: {file_path}")
        else:
            print(f"Already fixed: {file_path}")
    else:
        print(f"File not found: {file_path}")

print("Done!")