#!/usr/bin/env python3
import os
import re

def fix_file_structure(file_path):
    """Fix file structure in Haskell files"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Fix incomplete imports by adding missing closing parentheses
        lines = content.split('\n')
        new_lines = []
        
        for line in lines:
            # Fix incomplete import statements
            if line.strip().startswith('import ') and not line.strip().endswith(')') and '(' in line:
                # Check if it's missing closing parenthesis
                if line.count('(') > line.count(')'):
                    line = line + ')'
            
            # Fix LANGUAGE pragmas
            if line.strip().startswith('{-# LANGUAGE ') and not line.strip().endswith('#-}'):
                line = line + ' #-}'
            
            # Fix OPTIONS_GHC pragmas
            if line.strip().startswith('{-# OPTIONS_GHC ') and not line.strip().endswith('#-}'):
                line = line + ' #-}'
            
            new_lines.append(line)
        
        # Join lines back
        content = '\n'.join(new_lines)
        
        # Write back if changed
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
        
        print(f"Fixed file structure in {file_path}")
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
        if fix_file_structure(file_path):
            fixed_count += 1
    
    print(f"Fixed {fixed_count} files")

if __name__ == "__main__":
    main()