#!/usr/bin/env python3
"""
Add HUnit import to all test files that need it
"""

import os
import re
import subprocess

def find_files_needing_hunit():
    """Run stack test and parse errors to find files that need HUnit"""
    result = subprocess.run(
        ['stack', 'test', '--flag', '*:fast', '--flag', '*:-production', 
         '--ghc-options=-O0 -rtsopts', '--jobs=1'],
        capture_output=True, text=True
    )
    
    files = set()
    for line in result.stderr.split('\n'):
        if 'Variable not in scope: testCase' in line or 'Variable not in scope: (@?=)' in line or 'Variable not in scope: assertFailure' in line:
            match = re.search(r'^(/[^:]+):', line)
            if match:
                files.add(match.group(1))
    
    return files

def add_hunit_to_file(filepath):
    """Add HUnit import to a specific file"""
    with open(filepath, 'r') as f:
        content = f.read()
    
    # Skip if already imports HUnit
    if re.search(r'^import Test\.Tasty\.HUnit', content, flags=re.MULTILINE):
        return False
    
    # Add import after module declaration
    lines = content.split('\n')
    insert_idx = 0
    for i, line in enumerate(lines):
        if line.startswith('module '):
            # Find the end of the module declaration
            while i < len(lines) and not (lines[i].startswith('import ') or lines[i].startswith('{-#') or (lines[i].strip() == '' and i > 0 and lines[i-1].strip() != '')):
                i += 1
            insert_idx = i
            break
        elif line.startswith('import ') and i > 0:
            insert_idx = i
            break
    
    lines.insert(insert_idx, 'import Test.Tasty.HUnit')
    content = '\n'.join(lines)
    
    with open(filepath, 'w') as f:
        f.write(content)
    return True

def main():
    print("Finding files that need HUnit imports...")
    files = find_files_needing_hunit()
    
    if not files:
        print("No files need HUnit imports")
        return
    
    print(f"Found {len(files)} files that need HUnit imports:")
    for f in files:
        print(f"  {f}")
    
    print("\nAdding HUnit imports...")
    modified = 0
    for filepath in files:
        if add_hunit_to_file(filepath):
            print(f"  Added HUnit import to {filepath}")
            modified += 1
    
    print(f"\nModified {modified} files")

if __name__ == '__main__':
    main()