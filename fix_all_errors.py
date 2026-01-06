#!/usr/bin/env python3
import os
import re
import glob

def fix_all_errors(file_path):
    """Fix all remaining errors in a test file"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix LANGUAGE pragma syntax errors
    content = re.sub(r'{-# LANGUAGE ([^}]*) #-}, FlexibleInstances', r'{-# LANGUAGE \1, FlexibleInstances #-}', content)
    content = re.sub(r'{-# LANGUAGE LambdaCase #-, FlexibleInstances}', r'{-# LANGUAGE LambdaCase, FlexibleInstances #-}', content)
    content = re.sub(r'{-# LANGUAGE TemplateHaskell #-, FlexibleInstances}', r'{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}', content)
    content = re.sub(r'{-# LANGUAGE TypeSynonymInstances #-, FlexibleInstances}', r'{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}', content)
    
    # Fix import lists with malformed QuickCheckTests
    content = re.sub(r'import Test\.Tasty\.QuickCheck \([^)]*QuickCheckTests\([^)]*\), property[^)]*\)', 
                     r'import Test.Tasty.QuickCheck (testProperty, property)', content)
    
    # Fix malformed import lists with === and property
    content = re.sub(r'import Test\.Tasty\.QuickCheck \((===, property, [^)]*)\)', 
                     r'import Test.Tasty.QuickCheck (===, property, \1)', content)
    
    # Fix import list with duplicate property
    content = re.sub(r'import Test\.Tasty\.QuickCheck \(([^)]*===, property[^)]*===, property[^)]*)\)', 
                     r'import Test.Tasty.QuickCheck (===, property, \1)', content)
    
    # Fix import list with testProperties at the end
    content = re.sub(r'import Test\.Tasty\.QuickCheck \(([^)]*testProperties[^)]*===, property[^)]*)\)', 
                     r'import Test.Tasty.QuickCheck (testProperties, property)', content)
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed all errors in {file_path}")

# Get all test files
test_files = glob.glob('test/Test/Unit/*.hs')
for file_path in test_files:
    fix_all_errors(file_path)