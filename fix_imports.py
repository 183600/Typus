#!/usr/bin/env python3
import os
import re
import sys

def fix_imports(file_path):
    """Fix QuickCheck imports in a Haskell file"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        # Check if file uses (==>)
        if '(==>)' not in content:
            return False
        
        # Check if (==>) is already imported
        if re.search(r'import.*Test\.QuickCheck.*\(==>\)', content):
            return False
        
        # Find the Test.QuickCheck import line
        quickcheck_import_pattern = r'(import\s+Test\.Tasty\.QuickCheck\s*\([^)]*)\)'
        match = re.search(quickcheck_import_pattern, content)
        
        if match:
            # Add (==>) to the existing import
            old_import = match.group(0)
            new_import = old_import.replace(')', ', (==>))')
            content = content.replace(old_import, new_import)
            
            with open(file_path, 'w') as f:
                f.write(content)
            print(f"Fixed {file_path}")
            return True
        else:
            # Check if there's a Test.Tasty.QuickCheck import without parentheses
            simple_import_pattern = r'import\s+Test\.Tasty\.QuickCheck'
            if re.search(simple_import_pattern, content):
                # Replace simple import with one that includes (==>)
                old_import = 'import Test.Tasty.QuickCheck'
                new_import = 'import Test.Tasty.QuickCheck ((==>))'
                content = content.replace(old_import, new_import)
                
                with open(file_path, 'w') as f:
                    f.write(content)
                print(f"Fixed {file_path}")
                return True
        
        return False
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return False

def main():
    test_dir = "test/Test/Unit"
    fixed_count = 0
    
    for filename in os.listdir(test_dir):
        if filename.endswith(".hs"):
            file_path = os.path.join(test_dir, filename)
            if fix_imports(file_path):
                fixed_count += 1
    
    print(f"Fixed {fixed_count} files")

if __name__ == "__main__":
    main()