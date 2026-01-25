#!/usr/bin/env python3
"""
Fix HUnit imports in all test files properly
"""

import os
import re

def fix_hunit_import(filepath):
    """Fix HUnit import in a file"""
    with open(filepath, 'r') as f:
        content = f.read()
    
    # Skip if already properly imported
    if re.search(r'^module ', content, flags=re.MULTILINE) and re.search(r'^import Test\.Tasty\.HUnit', content, flags=re.MULTILINE):
        # Check if it's after the module declaration
        lines = content.split('\n')
        module_idx = -1
        import_idx = -1
        for i, line in enumerate(lines):
            if line.startswith('module '):
                module_idx = i
            elif line.startswith('import Test.Tasty.HUnit'):
                import_idx = i
        
        if module_idx >= 0 and import_idx > module_idx:
            return False
    
    # Remove any misplaced HUnit imports
    content = re.sub(r'^import Test\.Tasty\.HUnit\s*\n?', '', content, flags=re.MULTILINE)
    
    # Find the right place to add the import
    lines = content.split('\n')
    insert_idx = 0
    
    # Skip pragmas at the top
    i = 0
    while i < len(lines) and (lines[i].startswith('{-#') or lines[i].strip() == ''):
        i += 1
    
    # Skip module declaration
    if i < len(lines) and lines[i].startswith('module '):
        i += 1
        # Skip exports
        while i < len(lines) and (lines[i].startswith(' ') or lines[i].startswith('\t') or lines[i].strip() == ''):
            i += 1
        # Skip blank line after module
        if i < len(lines) and lines[i].strip() == '':
            i += 1
    
    insert_idx = i
    
    # Insert the import
    lines.insert(insert_idx, 'import Test.Tasty.HUnit')
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
                if fix_hunit_import(filepath):
                    print(f"Fixed HUnit import in {filepath}")
                    modified += 1
    
    print(f"Modified {modified} files")

if __name__ == '__main__':
    main()