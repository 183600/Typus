#!/usr/bin/env python3
import os
import binascii

def fix_bom(file_path):
    """Fix BOM and other encoding issues in Haskell files"""
    try:
        with open(file_path, 'rb') as f:
            content = f.read()
        
        # Check for BOM and remove it
        if content.startswith(b'\xef\xbb\xbf'):
            content = content[3:]
            with open(file_path, 'wb') as f:
                f.write(content)
            print(f"Removed BOM from {file_path}")
            return True
        
        # Check for other non-ASCII characters at the beginning
        i = 0
        while i < len(content) and content[i] > 127:
            i += 1
        
        if i > 0:
            content = content[i:]
            with open(file_path, 'wb') as f:
                f.write(content)
            print(f"Removed non-ASCII prefix from {file_path}")
            return True
            
        return False
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return False

def main():
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    
    # Find all Haskell test files
    haskell_files = []
    for root, dirs, files in os.walk(test_dir):
        for file in files:
            if file.endswith('.hs'):
                haskell_files.append(os.path.join(root, file))
    
    print(f"Found {len(haskell_files)} Haskell test files")
    
    fixed_count = 0
    for file_path in haskell_files:
        if fix_bom(file_path):
            fixed_count += 1
    
    print(f"Fixed {fixed_count} files")

if __name__ == "__main__":
    main()