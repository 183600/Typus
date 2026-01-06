#!/usr/bin/env python3
import os
import re
import glob

def fix_specific_errors(file_path):
    """Fix specific parse errors on input '===' """
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix parse error on input '===' in QuickCheck imports
    # The issue is that === is not a valid identifier, it needs to be surrounded by parentheses
    content = re.sub(
        r'import Test\.Tasty\.QuickCheck \(===, property, testProperty, ([^)]*)\)',
        r'import Test.Tasty.QuickCheck ((===), property, testProperty, \1)',
        content
    )
    
    content = re.sub(
        r'import Test\.Tasty\.QuickCheck \(===, property, Property, ([^)]*)\)',
        r'import Test.Tasty.QuickCheck ((===), property, Property, \1)',
        content
    )
    
    content = re.sub(
        r'import Test\.Tasty\.QuickCheck \(===, property, testProperty, Property, ([^)]*)\)',
        r'import Test.Tasty.QuickCheck ((===), property, testProperty, Property, \1)',
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed specific errors in {file_path}")

# Get all test files
test_files = glob.glob('test/Test/Unit/*.hs')
for file_path in test_files:
    fix_specific_errors(file_path)