#!/usr/bin/env python3
import os
import re

def fix_broken_imports(file_path):
    """Fix broken imports in Haskell files"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Fix broken import statement
        content = re.sub(r'import Test\.Tasty\s+\n\s+\(x \+ y\) \+\s+z === x \+ \(y \+ z\)', 
                        'import Test.Tasty (TestTree, testGroup)', content)
        
        # Fix other common broken patterns
        # Pattern 1: Missing closing parenthesis with extra content
        content = re.sub(r'import\s+(\S+)\s*\n\s*\([^)]*\)\s*\n.*?===', 
                        r'import \1 (TestTree, testGroup)', content, flags=re.DOTALL)
        
        # Write back if changed
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
        
        print(f"Fixed broken imports in {file_path}")
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
            fix_broken_imports(file_path)
    
    print("Fixed all error files")

if __name__ == "__main__":
    main()