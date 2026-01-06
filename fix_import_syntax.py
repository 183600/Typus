#!/usr/bin/env python3
import os
import re
import glob

def fix_import_syntax(file_path):
    """Fix import syntax errors in a test file"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix LANGUAGE pragma syntax
    content = re.sub(r'{-# LANGUAGE ([^}]*) #-}, FlexibleInstances', r'{-# LANGUAGE \1, FlexibleInstances #-}', content)
    content = re.sub(r'{-# LANGUAGE ([^}]*) #-}, FlexibleInstances', r'{-# LANGUAGE \1, FlexibleInstances #-}', content)
    
    # Fix import list syntax
    # Remove duplicate property in import list
    content = re.sub(r'import Test\.Tasty\.QuickCheck \(([^)]*property, property[^)]*)\)', r'import Test.Tasty.QuickCheck (\1)', content)
    
    # Fix malformed import lists
    content = re.sub(r'import Test\.Tasty\.QuickCheck \(([^)]*===, property[^)]*)\)', r'import Test.Tasty.QuickCheck (===, property, \1)', content)
    
    # Fix Arbitrary syntax in import list
    content = re.sub(r'import Test\.Tasty\.QuickCheck \(([^)]*Arbitrary\(\.\.\), property[^)]*)\)', r'import Test.Tasty.QuickCheck (Arbitrary(..), property, \1)', content)
    
    # Remove property from Arbitrary parentheses
    content = re.sub(r'Arbitrary\(\.\., property\)', r'Arbitrary(..)', content)
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed import syntax in {file_path}")

# Get all test files
test_files = glob.glob('test/Test/Unit/*.hs')
for file_path in test_files:
    fix_import_syntax(file_path)