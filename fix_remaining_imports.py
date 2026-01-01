#!/usr/bin/env python3
import os
import re
import sys

def fix_remaining_import_errors(file_path):
    """Fix remaining import list errors"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix import list errors
    content = re.sub(
        r', errorAt "test-id" , compareSeverity',
        r', compareSeverity',
        content
    )
    
    content = re.sub(
        r', errorAt "test-id" , infoAt',
        r', infoAt',
        content
    )
    
    content = re.sub(
        r', warningAt "test-id" formatError',
        r', warningAt, formatError',
        content
    )
    
    content = re.sub(
        r', errorAt "test-id" , customRecovery',
        r', customRecovery',
        content
    )
    
    content = re.sub(
        r', errorAt "test-id" , ErrorSeverity\(\.\.\)',
        r', ErrorSeverity(..)',
        content
    )
    
    content = re.sub(
        r', errorAt "test-id" , filterByCategory',
        r', filterByCategory',
        content
    )
    
    content = re.sub(
        r', errorAt "test-id" , infoWithCategory',
        r', infoWithCategory',
        content
    )
    
    content = re.sub(
        r', errorAt "test-id" , ErrorLocation\(\.\.\)',
        r', ErrorLocation(..)',
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"Fixed remaining imports in {file_path}")

def main():
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    
    # Find all test files
    for file_name in os.listdir(test_dir):
        if file_name.endswith(".hs"):
            file_path = os.path.join(test_dir, file_name)
            try:
                fix_remaining_import_errors(file_path)
            except Exception as e:
                print(f"Error fixing {file_name}: {e}")

if __name__ == "__main__":
    main()