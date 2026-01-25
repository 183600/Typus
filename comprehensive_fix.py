#!/usr/bin/env python3
"""
Comprehensive fix for all test files
"""

import os
import re
import glob

def fix_all_test_files():
    """Fix all test files at once"""
    test_files = glob.glob('test/Test/Unit/*.hs')
    
    modified = 0
    for filepath in test_files:
        with open(filepath, 'r') as f:
            content = f.read()
        
        # Extract module name from filepath
        rel_path = os.path.relpath(filepath, 'test/Test/Unit')
        module_name = rel_path[:-3].replace('/', '.')  # Remove .hs
        full_module_name = f'Test.Unit.{module_name}'
        
        # Remove any existing module declaration
        content = re.sub(r'^module .+ where', '', content, flags=re.MULTILINE)
        
        # Remove leading imports
        lines = content.split('\n')
        new_lines = []
        
        # Skip pragmas at the top
        i = 0
        while i < len(lines) and lines[i].startswith('{-#'):
            new_lines.append(lines[i])
            i += 1
        
        # Add module declaration
        new_lines.append(f'module {full_module_name} where')
        new_lines.append('')
        
        # Skip any imports at the beginning
        while i < len(lines) and lines[i].startswith('import '):
            i += 1
        
        # Add the rest of the file
        while i < len(lines):
            new_lines.append(lines[i])
            i += 1
        
        content = '\n'.join(new_lines)
        
        with open(filepath, 'w') as f:
            f.write(content)
        
        modified += 1
    
    print(f"Modified {modified} files")

if __name__ == '__main__':
    fix_all_test_files()