#!/usr/bin/env python3
import os
import re

def fix_module_case_capital(file_path):
    """Fix module declaration case in Haskell files"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Extract module name from file path
        relative_path = os.path.relpath(file_path, "/home/runner/work/Typus/Typus")
        module_name = relative_path.replace('/', '.').replace('.hs', '').replace('\\', '.')
        
        # Fix the first module declaration - capitalize the first letter
        content = re.sub(r'^module\s+(\S+)', lambda m: f"module {m.group(1)[0].upper() + m.group(1)[1:]}", content, count=1, flags=re.MULTILINE)
        
        # Write back if changed
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
        
        print(f"Fixed module case capital in {file_path}")
        return True
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
        if fix_module_case_capital(file_path):
            fixed_count += 1
    
    print(f"Fixed {fixed_count} files")

if __name__ == "__main__":
    main()