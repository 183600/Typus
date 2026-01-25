#!/usr/bin/env python3
"""
Final fix for all test files
"""

import os
import re

def fix_test_file(filepath):
    """Fix all issues in a test file"""
    with open(filepath, 'r') as f:
        content = f.read()
    
    # Extract module name from filepath
    module_match = re.search(r'test/Test/Unit/(.+)\.hs$', filepath)
    if not module_match:
        return False
    
    module_name = module_match.group(1).replace('/', '.')
    full_module_name = f'Test.Unit.{module_name}'
    
    modified = False
    
    # Check if module declaration exists
    if not re.search(r'^module ', content, flags=re.MULTILINE):
        # Find where to insert the module declaration
        lines = content.split('\n')
        insert_idx = 0
        
        # Skip pragmas at the top
        i = 0
        while i < len(lines) and lines[i].startswith('{-#'):
            i += 1
        
        insert_idx = i
        
        # Insert the module declaration
        lines.insert(insert_idx, f'module {full_module_name} where')
        content = '\n'.join(lines)
        modified = True
    
    # Fix any misplaced imports
    lines = content.split('\n')
    new_lines = []
    imports_to_add = []
    
    # Collect pragmas and module
    i = 0
    while i < len(lines):
        line = lines[i]
        if line.startswith('{-#'):
            new_lines.append(line)
        elif line.startswith('module '):
            new_lines.append(line)
            # Skip exports
            i += 1
            while i < len(lines) and (lines[i].startswith(' ') or lines[i].startswith('\t')):
                new_lines.append(lines[i])
                i += 1
            # Skip blank lines
            while i < len(lines) and lines[i].strip() == '':
                i += 1
            break
        else:
            break
        i += 1
    
    # Add blank line after module
    if new_lines and not new_lines[-1].strip() == '':
        new_lines.append('')
    
    # Collect imports
    while i < len(lines):
        line = lines[i]
        if line.startswith('import '):
            imports_to_add.append(line)
            i += 1
        else:
            break
    
    # Add imports in proper order
    if 'import Test.Tasty' not in imports_to_add and ('testCase' in content or 'testGroup' in content):
        imports_to_add.append('import Test.Tasty')
    
    if 'import Test.Tasty.HUnit' not in imports_to_add and ('testCase' in content or '@?=' in content or 'Assertion' in content):
        imports_to_add.append('import Test.Tasty.HUnit')
    
    if 'import Test.Tasty.QuickCheck' not in imports_to_add and ('property' in content or 'forAll' in content or 'Arbitrary' in content):
        imports_to_add.append('import Test.Tasty.QuickCheck')
    
    # Add imports
    for imp in imports_to_add:
        new_lines.append(imp)
    
    # Add the rest of the file
    while i < len(lines):
        new_lines.append(lines[i])
        i += 1
    
    content = '\n'.join(new_lines)
    
    if modified or len(imports_to_add) > 0:
        with open(filepath, 'w') as f:
            f.write(content)
        return True
    
    return False

def main():
    test_dir = 'test/Test/Unit'
    modified = 0
    
    for root, dirs, files in os.walk(test_dir):
        for file in files:
            if file.endswith('.hs'):
                filepath = os.path.join(root, file)
                if fix_test_file(filepath):
                    print(f"Fixed {filepath}")
                    modified += 1
    
    print(f"Modified {modified} files")

if __name__ == '__main__':
    main()