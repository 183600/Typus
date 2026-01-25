#!/usr/bin/env python3
"""
Fix all module declarations by directly replacing them
"""

import os
import re

def fix_all_modules_at_once():
    """Fix all module declarations in test files"""
    test_dir = 'test/Test/Unit'
    modified = 0
    
    for root, dirs, files in os.walk(test_dir):
        for file in files:
            if file.endswith('.hs'):
                filepath = os.path.join(root, file)
                
                with open(filepath, 'r') as f:
                    content = f.read()
                
                # Extract module name from filepath
                rel_path = os.path.relpath(filepath, test_dir)
                module_name = rel_path[:-3].replace('/', '.')  # Remove .hs extension
                full_module_name = f'Test.Unit.{module_name}'
                
                # Replace any module declaration with the correct one
                pattern = r'^module .+ where'
                replacement = f'module {full_module_name} where'
                
                if re.search(pattern, content, flags=re.MULTILINE):
                    content = re.sub(pattern, replacement, content, flags=re.MULTILINE)
                    with open(filepath, 'w') as f:
                        f.write(content)
                    modified += 1
    
    print(f"Modified {modified} files")

if __name__ == '__main__':
    fix_all_modules_at_once()