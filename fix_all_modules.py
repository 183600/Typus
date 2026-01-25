#!/usr/bin/env python3
"""
Fix all module declarations to be fully qualified
"""

import os
import re

def fix_module_declaration(filepath):
    """Fix module declaration in a file"""
    with open(filepath, 'r') as f:
        content = f.read()
    
    # Extract module name from filepath
    module_match = re.search(r'test/Test/Unit/(.+)\.hs$', filepath)
    if not module_match:
        return False
    
    module_name = module_match.group(1).replace('/', '.')
    full_module_name = f'Test.Unit.{module_name}'
    
    # Check if module declaration exists and is correct
    if re.search(r'^module ' + re.escape(full_module_name), content, flags=re.MULTILINE):
        return False
    
    # Replace any existing module declaration
    content = re.sub(r'^module .+ where', f'module {full_module_name} where', content, flags=re.MULTILINE)
    
    with open(filepath, 'w') as f:
        f.write(content)
    return True

def main():
    test_dir = 'test/Test/Unit'
    modified = 0
    
    for root, dirs, files in os.walk(test_dir):
        for file in files:
            if file.endswith('.hs'):
                filepath = os.path.join(root, file)
                if fix_module_declaration(filepath):
                    print(f"Fixed module declaration in {filepath}")
                    modified += 1
    
    print(f"Modified {modified} files")

if __name__ == '__main__':
    main()