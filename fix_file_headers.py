#!/usr/bin/env python3
import os
import re

def fix_file_header(file_path):
    """Fix the header of Haskell files"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        original_content = content
        
        # Remove any non-ASCII characters from the beginning of the file
        content = re.sub(r'^[^\x00-\x7F]+', '', content)
        
        # Ensure the file starts with proper module declaration or imports
        lines = content.split('\n')
        new_lines = []
        skip_empty = True
        
        for line in lines:
            if skip_empty and line.strip() == '':
                continue
            skip_empty = False
            new_lines.append(line)
        
        content = '\n'.join(new_lines)
        
        # Write back if changed
        if content != original_content:
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(content)
            print(f"Fixed header in {file_path}")
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
        if fix_file_header(file_path):
            fixed_count += 1
    
    print(f"Fixed {fixed_count} files")

if __name__ == "__main__":
    main()