#!/usr/bin/env python3
import os
import re
import sys

def fix_error_handlers(file_path):
    """Fix common error handler issues"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix T.unpack import
    if 'import qualified Data.Text as T' in content and 'T.unpack' in content and '(unpack' not in content:
        content = content.replace(
            'import qualified Data.Text as T',
            'import qualified Data.Text as T (pack, unpack)'
        )
    
    # Fix ErrorLocation constructor calls
    content = re.sub(
        r'ErrorLocation\s+(\w+)\s+"([^"]+)"',
        r'ErrorLocation (Just "\2") 1 1 Nothing Nothing',
        content
    )
    
    # Fix errorAt function calls
    content = re.sub(
        r'errorAt\s+([^)]+)\s+([^)]+)\s+(\w+)',
        r'errorAt "test-id" \2 \3',
        content
    )
    
    # Fix warningAt function calls
    content = re.sub(
        r'warningAt\s+([^)]+)\s+([^)]+)\s+(\w+)',
        r'warningAt "test-id" \2 \3',
        content
    )
    
    # Fix infoAt function calls
    content = re.sub(
        r'infoAt\s+([^)]+)\s+([^)]+)\s+(\w+)',
        r'infoAt "test-id" \2 \3',
        content
    )
    
    # Fix TypeError pattern matching
    content = re.sub(
        r'TypeError\s+(\w+)\s+(\w+)\s+(\w+)\s+(\w+)\s+(\w+)\s+(\w+)\s+(\w+)\s+(\w+)\s+(\w+)\s+(\w+)\s+(\w+)',
        r'TypeError errId \1 \2 \3 \4 \5 \6 \7 \8 \9 \10 \11',
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"Fixed {file_path}")

def main():
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    
    # Find all test files
    for file_name in os.listdir(test_dir):
        if file_name.endswith(".hs"):
            file_path = os.path.join(test_dir, file_name)
            try:
                fix_error_handlers(file_path)
            except Exception as e:
                print(f"Error fixing {file_name}: {e}")

if __name__ == "__main__":
    main()