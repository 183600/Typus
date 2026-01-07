#!/usr/bin/env python3
import os
import re

def fix_module_name(file_path):
    """Fix module name in Haskell files"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Fix the first module declaration - replace 'Test.Test.Unit' with 'Test.Unit'
        content = re.sub(r'^module\s+Test\.Test\.Unit', 'module Test.Unit', content, count=1, flags=re.MULTILINE)
        
        # Write back if changed
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
        
        print(f"Fixed module name in {file_path}")
        return True
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return False

def main():
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    
    # Find all Haskell test files
    haskell_files = []
    for root, dirs, files in os.walk(test_dir):
        for file in files:
            if file.endswith('.hs'):
                haskell_files.append(os.path.join(root, file))
    
    print(f"Found {len(haskell_files)} Haskell test files")
    
    fixed_count = 0
    for file_path in haskell_files:
        if fix_module_name(file_path):
            fixed_count += 1
    
    print(f"Fixed {fixed_count} files")

if __name__ == "__main__":
    main()