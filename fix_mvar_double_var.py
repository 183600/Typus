#!/usr/bin/env python3
import os
import re

def fix_mvar_double_var(file_path):
    """Fix double MVar variable names in test files"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix double MVar variable names
    content = re.sub(r'resultsVarVar', r'resultsVar', content)
    content = re.sub(r'resultsVar1', r'results1', content)
    content = re.sub(r'resultsVar2', r'results2', content)
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed MVar double var errors in {file_path}")

def main():
    # Fix specific files
    files_to_fix = [
        "test/Test/Unit/CabalConcurrentParsingSpec.hs",
    ]
    
    for file_path in files_to_fix:
        if os.path.exists(file_path):
            fix_mvar_double_var(file_path)

if __name__ == "__main__":
    main()