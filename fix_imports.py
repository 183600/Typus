#!/usr/bin/env python3
import os
import re
import sys

def fix_import_errors(file_path):
    """Fix import list errors"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix import list errors
    content = re.sub(
        r', errorAt "test-id" , isAtLeast',
        r', isAtLeast',
        content
    )
    
    content = re.sub(
        r', errorAt "test-id" , shouldContinueAfter',
        r', shouldContinueAfter',
        content
    )
    
    content = re.sub(
        r', errorAt "test-id" , infoRecovery',
        r', infoRecovery',
        content
    )
    
    content = re.sub(
        r', errorAt "test-id" , _atLocation',
        r', _atLocation',
        content
    )
    
    content = re.sub(
        r', errorAt "test-id" , getErrorColumn',
        r', getErrorColumn',
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"Fixed imports in {file_path}")

def main():
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    
    # Find all test files
    for file_name in os.listdir(test_dir):
        if file_name.endswith(".hs"):
            file_path = os.path.join(test_dir, file_name)
            try:
                fix_import_errors(file_path)
            except Exception as e:
                print(f"Error fixing {file_name}: {e}")

if __name__ == "__main__":
    main()