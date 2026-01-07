#!/usr/bin/env python3
import os
import re

def fix_complex_imports(file_path):
    """Fix complex import issues in Haskell files"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Fix specific import issues
        # Pattern 1: Missing closing parenthesis
        content = re.sub(r'import Test\.QuickCheck \(Gen, choose, vectorOf, elements, Arbitrary\(\.\.\)', 
                        'import Test.QuickCheck (Gen, choose, vectorOf, elements, Arbitrary(..))', content)
        
        # Pattern 2: Empty import
        content = re.sub(r'^(import TestSupport\.QuickCheck)$', 
                        r'\1 ()', content, flags=re.MULTILINE)
        
        # Pattern 3: Import with misplaced content
        content = re.sub(r'import Data\.List\s*\n\s*in \(levelToInt level1 <= levelToInt level2\) \|\| \(levelToInt level1 > levelToInt level2\)', 
                        'import Data.List', content)
        
        # Pattern 4: Import with extra content
        content = re.sub(r'import EnhancedDebug\s*\n.*?test_debug_message_creation :: IO \(\)', 
                        'import EnhancedDebug\n\ntest_debug_message_creation :: IO ()', content, flags=re.DOTALL)
        
        # Write back if changed
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
        
        print(f"Fixed complex imports in {file_path}")
        return True
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return False

def main():
    # Get list of files with errors
    result = os.popen('cabal test --test-show-details=never 2>&1 | grep -i "error:" | cut -d: -f1 | sort | uniq').read()
    error_files = [f.strip() for f in result.split('\n') if f.strip()]
    
    print(f"Found {len(error_files)} files with errors")
    
    for file_path in error_files:
        if os.path.exists(file_path):
            fix_complex_imports(file_path)
    
    print("Fixed all error files")

if __name__ == "__main__":
    main()