#!/usr/bin/env python3

import re

def fix_duplicate_left_patterns(file_path):
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Pattern to find duplicate Left _ -> property True lines
    pattern = r'(Left _ -> property False\s*\n\s*)Left _ -> property True\s*\n\s*(else property True|-- |)'
    
    # Replace with correct structure
    replacement = r'\1\2'
    
    # Apply the fix multiple times
    while re.search(pattern, content, re.MULTILINE):
        content = re.sub(pattern, replacement, content, flags=re.MULTILINE)
    
    # Write back the fixed content
    with open(file_path, 'w') as f:
        f.write(content)
    
    print("Fixed duplicate Left _ -> property True patterns")

if __name__ == "__main__":
    fix_duplicate_left_patterns("/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs")