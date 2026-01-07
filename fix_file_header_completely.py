#!/usr/bin/env python3
import os
import re

def fix_file_header_completely(file_path):
    """Fix the header of Haskell files completely"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Extract module name from file path
        relative_path = os.path.relpath(file_path, "/home/runner/work/Typus/Typus")
        module_name = relative_path.replace('/', '.').replace('.hs', '').replace('\\', '.')
        
        # Split content into lines
        lines = content.split('\n')
        
        # Find the first non-empty, non-comment line
        first_code_line = 0
        for i, line in enumerate(lines):
            stripped = line.strip()
            if stripped and not stripped.startswith('--') and not stripped.startswith('{-', '-}'):
                first_code_line = i
                break
        
        # Create new header
        new_lines = [f"module {module_name} where"]
        
        # Add the rest of the lines, skipping any existing module declarations
        for i, line in enumerate(lines[first_code_line:], start=first_code_line):
            if not re.match(r'^module\s+', line):
                new_lines.append(line)
        
        # Write back
        new_content = '\n'.join(new_lines)
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(new_content)
        
        print(f"Fixed header completely in {file_path}")
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
        if fix_file_header_completely(file_path):
            fixed_count += 1
    
    print(f"Fixed {fixed_count} files")

if __name__ == "__main__":
    main()