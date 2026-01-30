#!/usr/bin/env python3
import os
import re
import sys

def fix_file(file_path):
    """Fix TypusFile and FileDirectives constructor arguments in a file."""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        original_content = content
        
        # Fix TypusFile patterns with 3 arguments to 4
        # Pattern: TypusFile (FileDirectives ...) _ _ _ _
        content = re.sub(
            r'TypusFile\s*\(\s*([^()]+)\s*\)\s+_\s+_\s+_\s+_',
            lambda m: f"TypusFile ({m.group(1)}) _ _ _",
            content
        )
        
        # Pattern: TypusFile (FileDirectives ...) _ _ _
        content = re.sub(
            r'TypusFile\s*\(\s*([^()]+)\s*\)\s+_\s+_\s+_',
            lambda m: f"TypusFile ({m.group(1)}) _ _ _",
            content
        )
        
        # Fix FileDirectives patterns with 4 arguments to 3
        # Pattern: FileDirectives _ (Just ...) _ _
        content = re.sub(
            r'FileDirectives\s+(_\s*\(?\s*Just\s+[^)]+\)\s*)\s+_\s+_',
            r'FileDirectives \1 _',
            content
        )
        
        # Pattern: FileDirectives (Just ...) _ _ _
        content = re.sub(
            r'FileDirectives\s*\(\s*(Just\s+[^)]+)\s*\)\s+_\s+_',
            r'FileDirectives (\1) _',
            content
        )
        
        # Write back if changed
        if content != original_content:
            with open(file_path, 'w') as f:
                f.write(content)
            print(f"Fixed: {file_path}")
            return True
        return False
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return False

def main():
    if len(sys.argv) != 2:
        print("Usage: python3 fix_typus_file.py <test_directory>")
        sys.exit(1)
    
    test_dir = sys.argv[1]
    fixed_count = 0
    
    # Find all .hs files in the test directory
    for root, dirs, files in os.walk(test_dir):
        for file in files:
            if file.endswith('.hs'):
                file_path = os.path.join(root, file)
                if fix_file(file_path):
                    fixed_count += 1
    
    print(f"Fixed {fixed_count} files")

if __name__ == "__main__":
    main()