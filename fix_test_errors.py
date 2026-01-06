#!/usr/bin/env python3

import os
import re

# Find all test files
test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
fixed_files = []

for root, dirs, files in os.walk(test_dir):
    for file in files:
        if file.endswith(".hs"):
            file_path = os.path.join(root, file)
            
            # Read the file
            with open(file_path, 'r') as f:
                content = f.read()
            
            original_content = content
            
            # Fix parseTypus calls with extra parameter
            content = re.sub(r'Parser\.parseTypus\s+"[^"]*"\s+', 'Parser.parseTypus ', content)
            
            # Fix let bindings with <- operator
            content = re.sub(r'(\s+)let\s+(\w+)\s+<-', r'\1\2 <-', content)
            
            # Fix qualified names in binding position
            content = re.sub(r'L\.isInfixOf\s+', 'isInfixOf ', content)
            content = re.sub(r'L\.isPrefixOf\s+', 'isPrefixOf ', content)
            
            # Write back if changed
            if content != original_content:
                with open(file_path, 'w') as f:
                    f.write(content)
                fixed_files.append(file_path)

print(f"Fixed {len(fixed_files)} files:")
for file_path in fixed_files[:10]:  # Show first 10 files
    print(f"  {file_path}")
if len(fixed_files) > 10:
    print(f"  ... and {len(fixed_files) - 10} more files")