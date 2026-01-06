#!/usr/bin/env python3
import os
import re
import glob

def fix_remaining_compilation_errors(file_path):
    """Fix remaining compilation errors in test files"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix missing testProperty, Property, and (===) in QuickCheck imports
    if 'Test.Tasty.QuickCheck' in content and 'testProperty' not in content:
        content = re.sub(
            r'import Test\.Tasty\.QuickCheck \(([^)]*)\)',
            r'import Test.Tasty.QuickCheck (testProperty, property, Property, (===), (==>), \1)',
            content
        )
    
    # Fix missing Property in QuickCheck imports
    if 'Test.Tasty.QuickCheck' in content and 'Property' not in content:
        content = re.sub(
            r'import Test\.Tasty\.QuickCheck \(([^)]*)\)',
            r'import Test.Tasty.QuickCheck (Property, \1)',
            content
        )
    
    # Fix missing Arbitrary in QuickCheck imports
    if 'instance Arbitrary' in content and 'Arbitrary' not in content:
        content = re.sub(
            r'import Test\.Tasty\.QuickCheck \(([^)]*)\)',
            r'import Test.Tasty.QuickCheck (Arbitrary(..), \1)',
            content
        )
    
    # Fix missing reject in QuickCheck imports (remove it)
    content = re.sub(
        r'import Test\.Tasty\.QuickCheck \([^)]*reject[^)]*\)',
        r'import Test.Tasty.QuickCheck (property, Arbitrary(..), Gen, Property, (==>), forAll, choose, listOf1, elements, oneof, sized, suchThat, (===))',
        content
    )
    
    # Fix Parser.parseTypus calls
    content = re.sub(
        r'Parser\.parseTypus "test" input',
        r'Parser.parseTypus input',
        content
    )
    
    # Fix SemanticIR constructor calls
    content = re.sub(
        r'SemanticIR file "" [] [] Set\.empty',
        r'SemanticIR file (Compiler.GoAst.GoModule "" [] [] []) [] [] Set.empty',
        content
    )
    
    content = re.sub(
        r'SemanticIR file code [] [] Set\.empty',
        r'SemanticIR file (Compiler.GoAst.GoModule "" [] [] []) [] [] Set.empty',
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed remaining compilation errors in {file_path}")

# Get all test files
test_files = glob.glob('test/Test/Unit/*.hs')
for file_path in test_files:
    fix_remaining_compilation_errors(file_path)