#!/usr/bin/env python3
"""
Fix HUnit imports in all test files properly
"""

import os
import re

def fix_hunit_import_properly(filepath):
    """Fix HUnit import in a file"""
    with open(filepath, 'r') as f:
        content = f.read()
    
    # Check if HUnit is needed
    needs_hunit = 'Assertion' in content or 'testCase' in content or '@?=' in content
    
    if not needs_hunit:
        return False
    
    # Remove any misplaced HUnit imports
    content = re.sub(r'^import Test\.Tasty\.HUnit\s*\n?', '', content, flags=re.MULTILINE)
    
    # Find the right place to add the import
    lines = content.split('\n')
    insert_idx = 0
    
    # Skip pragmas at the top
    i = 0
    while i < len(lines) and lines[i].startswith('{-#'):
        i += 1
    
    # Skip module declaration
    if i < len(lines) and lines[i].startswith('module '):
        i += 1
        # Skip exports
        while i < len(lines) and (lines[i].startswith(' ') or lines[i].startswith('\t')):
            i += 1
        # Skip blank lines after module
        while i < len(lines) and lines[i].strip() == '':
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
                if fix_hunit_import_properly(filepath):
                    print(f"Fixed HUnit import in {filepath}")
                    modified += 1
    
    print(f"Modified {modified} files")

if __name__ == '__main__':
    main()