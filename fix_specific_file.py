#!/usr/bin/env python3
import os
import re

def fix_specific_file(file_path):
    """Fix a specific file with import issues"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Fix specific import issues
        content = re.sub(r'import Test\.Tasty\.HUnit \(testCase, assertFailure, assertBool, \(@\?=\)', 
                        'import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))', content)
        
        content = re.sub(r'import TestSupport\.QuickCheck$', 
                        'import TestSupport.QuickCheck ()', content)
        
        content = re.sub(r'import Test\.QuickCheck \(Gen, choose, vectorOf, elements, Arbitrary\(\.\.\), \(==>\), forAll, counterexample, classify, property, \(\.&&\.\), \)', 
                        'import Test.QuickCheck (Gen, choose, vectorOf, elements, Arbitrary(..), (==>), forAll, counterexample, classify, property, (.&&.))', content)
        
        content = re.sub(r'import Test\.QuickCheck\.Arbitrary \(Arbitrary\(\.\.\)', 
                        'import Test.QuickCheck.Arbitrary (Arbitrary(..))', content)
        
        # Write back if changed
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
        
        print(f"Fixed specific file {file_path}")
        return True
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return False

def main():
    # Fix the specific file mentioned in the error
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/AdditionalCabalQuickCheckTestSpec.hs"
    fix_specific_file(file_path)

if __name__ == "__main__":
    main()