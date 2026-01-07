#!/usr/bin/env python3
import os
import re

def fix_imports(file_path):
    """Fix import statements in Haskell files"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Fix truncated import statements
        # Pattern to match incomplete imports
        content = re.sub(r'^(import\s+.*?)\.\.\.$', r'\1', content, flags=re.MULTILINE)
        
        # Fix incomplete parentheses in imports
        content = re.sub(r'^(import\s+.*?)\([^)]*$', r'\1', content, flags=re.MULTILINE)
        
        # Fix incomplete module declarations
        content = re.sub(r'^(module\s+.*?)\.\.$', r'\1', content, flags=re.MULTILINE)
        
        # Fix LANGUAGE pragmas
        content = re.sub(r'^{-# LANGUAGE (.*?)\.\.$', r'{-# LANGUAGE \1 #-}', content, flags=re.MULTILINE)
        
        # Fix OPTIONS_GHC pragmas
        content = re.sub(r'^{-# OPTIONS_GHC (.*?)\.\.$', r'{-# OPTIONS_GHC \1 #-}', content, flags=re.MULTILINE)
        
        # Fix incomplete import lists
        content = re.sub(r'^(import\s+.*?)\([^)]*$', r'\1)', content, flags=re.MULTILINE)
        
        # Fix incomplete qualified imports
        content = re.sub(r'^(import\s+qualified\s+.*?)\.\.$', r'\1', content, flags=re.MULTILINE)
        
        # Write back if changed
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
        
        print(f"Fixed imports in {file_path}")
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
        if fix_imports(file_path):
            fixed_count += 1
    
    print(f"Fixed {fixed_count} files")

if __name__ == "__main__":
    main()