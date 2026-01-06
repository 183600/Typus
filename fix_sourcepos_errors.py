#!/usr/bin/env python3
import os
import re
import glob

def fix_sourcepos_errors(file_path):
    """Fix SourcePos constructor errors in test files"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix SourcePos constructor calls
    content = re.sub(
        r'SourcePos "" 1 1',
        r'startPos',
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed SourcePos errors in {file_path}")

# Get all test files
test_files = glob.glob('test/Test/Unit/*.hs')
for file_path in test_files:
    fix_sourcepos_errors(file_path)