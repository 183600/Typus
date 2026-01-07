#!/usr/bin/env python3
import os
import re

def fix_module_imports(file_path):
    """Fix module import errors"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        original_content = content
        
        # Fix module declarations
        content = re.sub(r'module\s+Test\.Unit\.([^\s]+)\s+where', r'module Test.Unit.\1 where', content)
        
        # Fix import statements
        content = re.sub(r'import\s+Test\.Tasty\s+\([^)]+\)', 'import Test.Tasty (TestTree, testGroup)', content)
        content = re.sub(r'import\s+Test\.Tasty\.HUnit\s+\([^)]+\)', 'import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))', content)
        content = re.sub(r'import\s+Test\.QuickCheck\s+\([^)]+\)', 'import Test.QuickCheck (Gen, choose, vectorOf, elements, Arbitrary(..))', content)
        content = re.sub(r'import\s+Test\.QuickCheck\.Arbitrary\s+\([^)]+\)', 'import Test.QuickCheck.Arbitrary (Arbitrary(..))', content)
        
        # Fix missing imports
        if 'TestTree' in content and 'import Test.Tasty' not in content:
            content = 'import Test.Tasty (TestTree, testGroup)\n' + content
        if 'testCase' in content and 'import Test.Tasty.HUnit' not in content:
            content = 'import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))\n' + content
        if 'Arbitrary' in content and 'import Test.QuickCheck' not in content:
            content = 'import Test.QuickCheck (Gen, choose, vectorOf, elements, Arbitrary(..))\n' + content
        
        # Write back if changed
        if content != original_content:
            with open(file_path, 'w') as f:
                f.write(content)
            print(f"Fixed imports in {file_path}")
            return True
        return False
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
        if fix_module_imports(file_path):
            fixed_count += 1
    
    print(f"Fixed {fixed_count} files")

if __name__ == "__main__":
    main()