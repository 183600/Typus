#!/usr/bin/env python3
import os
import re

def fix_errorlocation_errors(file_path):
    """Fix ErrorLocation constructor calls in test files"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix ErrorLocation constructor calls
    content = re.sub(r'ErrorLocation \(startPos\) Nothing', r'ErrorLocation Nothing 1 1 Nothing Nothing', content)
    content = re.sub(r'ErrorLocation \(SourcePos.*?\) Nothing', r'ErrorLocation Nothing 1 1 Nothing Nothing', content)
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed ErrorLocation errors in {file_path}")

def main():
    # Fix specific files
    files_to_fix = [
        "test/Test/Unit/AdvancedErrorHandlerQuickCheckSpec.hs",
    ]
    
    for file_path in files_to_fix:
        if os.path.exists(file_path):
            fix_errorlocation_errors(file_path)

if __name__ == "__main__":
    main()