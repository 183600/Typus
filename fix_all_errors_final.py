#!/usr/bin/env python3
import os
import re
import glob

def fix_all_errors_final(file_path):
    """Fix all remaining errors in test files"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix LANGUAGE pragma syntax errors
    content = re.sub(r'{-# LANGUAGE OverloadedStrings #-, FlexibleInstances}', 
                     r'{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}', content)
    content = re.sub(r'{-# LANGUAGE ScopedTypeVariables #-, FlexibleInstances}', 
                     r'{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}', content)
    content = re.sub(r'{-# LANGUAGE CPP #-, FlexibleInstances}', 
                     r'{-# LANGUAGE CPP, FlexibleInstances #-}', content)
    
    # Fix import lists with duplicate ===
    content = re.sub(r'import Test\.Tasty\.QuickCheck \(===, property, ===, property, ===, property, ===, property, ===, property, ([^)]*)\)', 
                     r'import Test.Tasty.QuickCheck (===, property, \1)', content)
    
    # Fix QuickCheckTests syntax
    content = re.sub(r'import Test\.Tasty\.QuickCheck \(testProperty, QuickCheckTests\(\.\., property\)\)', 
                     r'import Test.Tasty.QuickCheck (testProperty, property)', content)
    
    # Fix Property syntax
    content = re.sub(r'import Test\.Tasty\.QuickCheck \(testProperty, Property\(\.\., property\), ([^)]*)\)', 
                     r'import Test.Tasty.QuickCheck (testProperty, Property, \1)', content)
    
    # Fix (==>, property) syntax
    content = re.sub(r'import Test\.Tasty\.QuickCheck \(testProperty, Property, \(==>, property\), ([^)]*)\)', 
                     r'import Test.Tasty.QuickCheck (testProperty, Property, (==>), \1)', content)
    
    # Fix Property(.., property) syntax
    content = re.sub(r'import Test\.Tasty\.QuickCheck \(testProperty, Property\(\.\., property\), ([^)]*)\)', 
                     r'import Test.Tasty.QuickCheck (testProperty, Property, \1)', content)
    
    # Fix QuickCheckTests(.., property) syntax
    content = re.sub(r'import Test\.Tasty\.QuickCheck \(testProperty, QuickCheckTests\(\.\., property\)\)', 
                     r'import Test.Tasty.QuickCheck (testProperty, property)', content)
    
    # Fix parse error on input ','
    content = re.sub(r'import Test\.Tasty\.QuickCheck \(testProperty, Property, \(==>, property\), ([^)]*)\)', 
                     r'import Test.Tasty.QuickCheck (testProperty, Property, (==>), \1)', content)
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed all errors in {file_path}")

# Get all test files
test_files = glob.glob('test/Test/Unit/*.hs')
for file_path in test_files:
    fix_all_errors_final(file_path)