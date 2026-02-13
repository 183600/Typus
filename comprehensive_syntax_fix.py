#!/usr/bin/env python3

import re

def comprehensive_syntax_fix(file_path):
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix all patterns of double else statements
    pattern = r'Left _ -> property False\s+else property True\s+else property True'
    replacement = 'Left _ -> property False\n  else property True'
    content = re.sub(pattern, replacement, content, flags=re.MULTILINE | re.DOTALL)
    
    # Fix patterns with incorrect indentation
    pattern2 = r'Left _ -> property False\n(\s+)else property True\n(\s+)else property True'
    replacement2 = r'Left _ -> property False\n\1else property True'
    content = re.sub(pattern2, replacement2, content, flags=re.MULTILINE)
    
    # Fix standalone else lines
    pattern3 = r'(\n\s+else property True\n\s+else property True\n)'
    replacement3 = '\n  else property True\n'
    content = re.sub(pattern3, replacement3, content)
    
    # Write back the fixed content
    with open(file_path, 'w') as f:
        f.write(content)
    
    print("Applied comprehensive syntax fixes")

if __name__ == "__main__":
    comprehensive_syntax_fix("/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs")