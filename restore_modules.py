#!/usr/bin/env python3
"""
Restore module declarations that were accidentally removed
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
    
    # Check if module declaration exists
    if re.search(r'^module ' + re.escape(module_name), content, flags=re.MULTILINE):
        return False
    
    # Find where to insert the module declaration
    lines = content.split('\n')
    insert_idx = 0
    
    # Skip pragmas at the top
    i = 0
    while i < len(lines) and lines[i].startswith('{-#'):
        i += 1
    insert_idx = i
    
    # Insert module declaration
    lines.insert(insert_idx, f'module Test.Unit.{module_name} where')
    content = '\n'.join(lines)
    
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