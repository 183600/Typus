#!/usr/bin/env python3
import os
import re
import sys

def fix_final_import_errors(file_path):
    """Fix final import list errors"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix import list errors
    content = re.sub(
        r', errorAt "test-id" , hasCategory',
        r', hasCategory',
        content
    )
    
    content = re.sub(
        r', errorAt "test-id" , ErrorSubLevel\(\.\.\)',
        r', ErrorSubLevel(..)',
        content
    )
    
    content = re.sub(
        r', errorAt "test-id" , filterCombinedErrorsBySeverity',
        r', filterCombinedErrorsBySeverity',
        content
    )
    
    content = re.sub(
        r', errorAt "test-id" , _atRange',
        r', _atRange',
        content
    )
    
    content = re.sub(
        r', errorAt "test-id" , formatErrors',
        r', formatErrors',
        content
    )
    
    content = re.sub(
        r', infoAt "test-id" warningRecovery',
        r', infoAt, warningRecovery',
        content
    )
    
    content = re.sub(
        r', errorAt "test-id" , fatalErrorWithCategory',
        r', fatalErrorWithCategory',
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"Fixed final imports in {file_path}")

def main():
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    
    # Find all test files
    for file_name in os.listdir(test_dir):
        if file_name.endswith(".hs"):
            file_path = os.path.join(test_dir, file_name)
            try:
                fix_final_import_errors(file_path)
            except Exception as e:
                print(f"Error fixing {file_name}: {e}")

if __name__ == "__main__":
    main()