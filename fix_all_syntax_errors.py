#!/usr/bin/env python3

import re

def fix_all_syntax_errors(file_path):
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix patterns like:
    # Left _ -> property False
    # 
    #             else property True-- | comment
    pattern1 = r'(Left _ -> property False\s*\n\s*)else property True(-- |.*?)\n'
    replacement1 = r'\1  else property True\n\2\n'
    content = re.sub(pattern1, replacement1, content, flags=re.MULTILINE)
    
    # Fix patterns like:
    # Left _ -> property False
    # 
    #             Left _ -> property True
    pattern2 = r'(Left _ -> property False\s*\n\s*)Left _ -> property True\s*\n'
    replacement2 = r'\1  else property True\n'
    content = re.sub(pattern2, replacement2, content, flags=re.MULTILINE)
    
    # Fix missing "else" in conditional expressions
    pattern3 = r'(if.*?then.*?Left _ -> property False\s*\n\s*)(else property True|-- \|)'
    replacement3 = r'\1  else property True\n\2'
    content = re.sub(pattern3, replacement3, content, flags=re.MULTILINE | re.DOTALL)
    
    # Write back the fixed content
    with open(file_path, 'w') as f:
        f.write(content)
    
    print("Fixed all syntax errors")

if __name__ == "__main__":
    fix_all_syntax_errors("/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs")