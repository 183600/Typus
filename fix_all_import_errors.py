#!/usr/bin/env python3
import os
import re

def fix_all_import_errors(file_path):
    """Fix all import errors in Haskell files"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Fix all common import errors
        # Pattern 1: Import with function definition
        content = re.sub(r'import\s+(\S+)\s*\n\s*\([^)]*\)\s*\n.*?(==>|===|::|->|let|where|case|if)', 
                        r'import \1\n\n', content, flags=re.DOTALL)
        
        # Pattern 2: Import with incomplete parenthesis
        content = re.sub(r'import\s+(\S+)\s*\n\s*\([^)]*$', 
                        r'import \1', content, flags=re.MULTILINE)
        
        # Pattern 3: Import with extra content
        content = re.sub(r'import\s+(\S+)\s*\n\s*[^a-zA-Z_].*?(import|\n\n|\n--|\n\s*\w)', 
                        r'import \1\2', content, flags=re.DOTALL)
        
        # Pattern 4: Empty import
        content = re.sub(r'^(import\s+\S+)$', 
                        r'\1 ()', content, flags=re.MULTILINE)
        
        # Pattern 5: Import with misplaced content
        content = re.sub(r'import\s+(\S+)\s*\n\s*[^a-zA-Z_].*?(\n\n|\n--|\n\s*\w)', 
                        r'import \1\2', content, flags=re.DOTALL)
        
        # Write back if changed
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
        
        print(f"Fixed all import errors in {file_path}")
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
            fix_all_import_errors(file_path)
    
    print("Fixed all error files")

if __name__ == "__main__":
    main()