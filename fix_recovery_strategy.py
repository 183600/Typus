#!/usr/bin/env python3
import os
import re

# Find all test files
test_dir = "/home/runner/work/Typus/Typus/test"
fixed_files = []

for root, dirs, files in os.walk(test_dir):
    for file in files:
        if file.endswith(".hs"):
            file_path = os.path.join(root, file)
            with open(file_path, 'r') as f:
                content = f.read()
            
            # Replace RecoveryStrategy with ErrorRecovery in constructor usage
            # But be careful not to replace type definitions or local data types
            original_content = content
            
            # Pattern 1: RecoveryStrategy <$> ... (Applicative style)
            content = re.sub(
                r'RecoveryStrategy\s+<\$>\s+',
                r'ErrorRecovery <$> ',
                content
            )
            
            # Pattern 2: return $ RecoveryStrategy ... 
            content = re.sub(
                r'return\s+\$ \s*RecoveryStrategy\s+',
                r'return $ ErrorRecovery ',
                content
            )
            
            # Pattern 3: let recovery = RecoveryStrategy ...
            content = re.sub(
                r'let\s+(\w+)\s*=\s*RecoveryStrategy\s+',
                r'let \1 = ErrorRecovery ',
                content
            )
            
            # Pattern 4: recovery = RecoveryStrategy ...
            content = re.sub(
                r'(\w+)\s*=\s*RecoveryStrategy\s+',
                r'\1 = ErrorRecovery ',
                content
            )
            
            if content != original_content:
                with open(file_path, 'w') as f:
                    f.write(content)
                fixed_files.append(file_path)
                print(f"Fixed: {file_path}")

print(f"\nFixed {len(fixed_files)} files")