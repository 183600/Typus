#!/usr/bin/env python3
import os
import re

def fix_last_error_file(file_path):
    """Fix the last error file"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Fix specific file
        if "AdvancedOwnershipQuickCheckSpec.hs" in file_path:
            content = re.sub(r'import TestSupport\.Arbitrary\s*\nimport Ownership\s*\n\s*\(ord1 == ord2\)', 
                            'import TestSupport.Arbitrary ()\nimport Ownership', content, flags=re.DOTALL)
        
        # Write back if changed
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
        
        print(f"Fixed last error file {file_path}")
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
            fix_last_error_file(file_path)
    
    print("Fixed all error files")

if __name__ == "__main__":
    main()