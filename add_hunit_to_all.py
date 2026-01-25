#!/usr/bin/env python3
"""
Add HUnit import to all files that use Assertion
"""

import os
import re
import subprocess

def find_files_using_assertion():
    """Find all files that use Assertion but don't import HUnit"""
    result = subprocess.run(
        ['grep', '-r', '-l', 'Assertion', 'test/Test/Unit'],
        capture_output=True, text=True
    )
    
    files = []
    for filepath in result.stdout.split('\n'):
        if filepath and filepath.endswith('.hs'):
            with open(filepath, 'r') as f:
                content = f.read()
            
            # Check if it imports HUnit
            if not re.search(r'^import Test\.Tasty\.HUnit', content, flags=re.MULTILINE):
                files.append(filepath)
    
    return files

def add_hunit_to_file(filepath):
    """Add HUnit import to a file"""
    with open(filepath, 'r') as f:
        content = f.read()
    
    # Find where to insert the import
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
    files = find_files_using_assertion()
    
    if not files:
        print("No files need HUnit imports")
        return
    
    print(f"Found {len(files)} files that need HUnit imports")
    modified = 0
    for filepath in files:
        if add_hunit_to_file(filepath):
            print(f"  Added HUnit to {filepath}")
            modified += 1
    
    print(f"Modified {modified} files")

if __name__ == '__main__':
    main()