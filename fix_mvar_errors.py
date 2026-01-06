#!/usr/bin/env python3
import os
import re

def fix_mvar_errors(file_path):
    """Fix MVar errors in test files"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix MVar binding issues
    content = re.sub(r'(\s+)results <- newEmptyMVar', r'\1resultsVar <- newEmptyMVar', content)
    content = re.sub(r'putMVar results', r'putMVar resultsVar', content)
    content = re.sub(r'takeMVar results', r'takeMVar resultsVar', content)
    content = re.sub(r'finalResults <- takeMVar results', r'finalResults <- takeMVar resultsVar', content)
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed MVar errors in {file_path}")

def main():
    # Fix specific files
    files_to_fix = [
        "test/Test/Unit/CabalConcurrentParsingSpec.hs",
        "test/Test/Unit/AdvancedErrorHandlerQuickCheckSpec.hs"
    ]
    
    for file_path in files_to_fix:
        if os.path.exists(file_path):
            fix_mvar_errors(file_path)

if __name__ == "__main__":
    main()