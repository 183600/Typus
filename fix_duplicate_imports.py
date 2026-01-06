#!/usr/bin/env python3
import os
import re
import sys

def fix_duplicate_imports(file_path):
    """Fix duplicate QuickCheck imports in a Haskell file"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        # Fix malformed imports like (===, (==>)) or (==>, (==>))
        content = re.sub(r'\(\s*===,\s*\(\s*==>\s*\)\s*\)', '(===, (==>))', content)
        content = re.sub(r'\(\s*==>,\s*\(\s*==>\s*\)\s*\)', '(==>)', content)
        
        # Fix multiple (==>) in the same import
        content = re.sub(r'\(\s*==>\s*\),\s*\(\s*==>\s*\)', '(==>)', content)
        content = re.sub(r'\(\s*==>\s*\),\s*\(\s*==>\s*\)', '(==>)', content)
        
        # Fix duplicate entries in import lists
        # This handles cases like (===, (==>)), (==>) -> (===, (==>))
        content = re.sub(r'\(\s*===,\s*\(\s*==>\s*\)\s*\),\s*\(\s*==>\s*\)', '(===, (==>))', content)
        
        # Fix malformed imports with extra parentheses
        content = re.sub(r'\(\s*===,\s*\(\s*==>\s*\)\s*\),\s*\(\s*==>\s*\)', '(===, (==>))', content)
        
        # Fix the specific pattern we saw
        content = re.sub(r'\(\s*===,\s*\(\s*==>\s*\)\s*\),\s*\(\s*==>\s*\)', '(===, (==>))', content)
        
        with open(file_path, 'w') as f:
            f.write(content)
        
        return True
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return False

def main():
    test_dir = "test/Test/Unit"
    fixed_count = 0
    
    for filename in os.listdir(test_dir):
        if filename.endswith(".hs"):
            file_path = os.path.join(test_dir, filename)
            if fix_duplicate_imports(file_path):
                fixed_count += 1
    
    print(f"Processed {fixed_count} files")

if __name__ == "__main__":
    main()