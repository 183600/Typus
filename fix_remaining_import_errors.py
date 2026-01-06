#!/usr/bin/env python3
import os
import re
import glob

def fix_remaining_import_errors(file_path):
    """Fix remaining import errors in test files"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix duplicate (===, property) in import lists
    content = re.sub(
        r'import Test\.Tasty\.QuickCheck \(\(===\), property, testProperty, Property, \(===, property\), ([^)]*)\)',
        r'import Test.Tasty.QuickCheck ((===), property, testProperty, Property, \1)',
        content
    )
    
    content = re.sub(
        r'import Test\.Tasty\.QuickCheck \(\(===\), property, testProperty, Property, \(===, property\), forAll, ([^)]*)\)',
        r'import Test.Tasty.QuickCheck ((===), property, testProperty, Property, forAll, \1)',
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed remaining import errors in {file_path}")

# Get all test files
test_files = glob.glob('test/Test/Unit/*.hs')
for file_path in test_files:
    fix_remaining_import_errors(file_path)