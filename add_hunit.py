#!/usr/bin/env python3
"""
Add Test.Tasty.HUnit import to files that need it
"""

import os
import re

def add_hunit_if_needed(filepath):
    """Add HUnit import if the file uses testCase or @?= but doesn't import HUnit"""
    with open(filepath, 'r') as f:
        content = f.read()
    
    # Check if the file uses HUnit features
    uses_testCase = 'testCase' in content
    uses_assertion = '@?=' in content or 'assert' in content
    
    # Skip if already imports HUnit
    if re.search(r'^import Test\.Tasty\.HUnit', content, flags=re.MULTILINE):
        return False
    
    # Add import if needed
    if uses_testCase or uses_assertion:
        # Find where to insert the import
        lines = content.split('\n')
        insert_idx = 0
        for i, line in enumerate(lines):
            if line.startswith('import '):
                insert_idx = i + 1
            elif line.startswith('module ') and i > 0:
                break
        
        lines.insert(insert_idx, 'import Test.Tasty.HUnit')
        content = '\n'.join(lines)
        
        with open(filepath, 'w') as f:
            f.write(content)
        return True
    
    return False

def main():
    import sys
    filepath = sys.argv[1] if len(sys.argv) > 1 else None
    
    if filepath:
        if add_hunit_if_needed(filepath):
            print(f"Added Test.Tasty.HUnit import to {filepath}")
    else:
        print("Usage: python3 add_hunit.py <filepath>")

if __name__ == '__main__':
    main()
