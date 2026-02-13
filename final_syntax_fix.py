#!/usr/bin/env python3

import re

def final_syntax_fix(file_path):
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix duplicate "else property True" lines
    pattern = r'(\s+else property True\n)\s+else property True\n'
    replacement = r'\1'
    content = re.sub(pattern, replacement, content, flags=re.MULTILINE)
    
    # Fix misplaced "else property True" at wrong indentation
    pattern2 = r'Left _ -> property False\n\n\s+else property True\n(\s+else property True\n)?'
    replacement2 = r'Left _ -> property False\n  else property True\n'
    content = re.sub(pattern2, replacement2, content, flags=re.MULTILINE)
    
    # Write back the fixed content
    with open(file_path, 'w') as f:
        f.write(content)
    
    print("Applied final syntax fixes")

if __name__ == "__main__":
    final_syntax_fix("/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs")