#!/usr/bin/env python3
import os
import re

def fix_mixed_imports_and_types(file_path):
    """Fix mixed imports and type definitions in Haskell files"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Fix mixed imports and type definitions
        # Pattern 1: Import with type definition
        content = re.sub(r'import Parser\s*\nimport Types \(TypusFile\(\.\.\.\), FileDirectives\(\.\.\.\), BlockDirectives\)\s*deriving \(Eq, Show, Generic\)', 
                        'import Parser\nimport Types (TypusFile(..), FileDirectives(..), BlockDirectives)\n\nnewtype TestString = TestString String\n  deriving (Eq, Show, Generic)', content)
        
        # Pattern 2: Empty import
        content = re.sub(r'^(import Test\.QuickCheck)$', 
                        r'\1 (Gen, choose, vectorOf, elements, Arbitrary(..))', content, flags=re.MULTILINE)
        
        # Pattern 3: Import with function definition
        content = re.sub(r'import\s+(\S+)\s*\n\s*\([^)]*\)\s*\n.*?(==>|===|::|->|let|where|case|if)', 
                        r'import \1\n\n', content, flags=re.DOTALL)
        
        # Write back if changed
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
        
        print(f"Fixed mixed imports and types in {file_path}")
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
            fix_mixed_imports_and_types(file_path)
    
    print("Fixed all error files")

if __name__ == "__main__":
    main()