#!/usr/bin/env python3
import os
import re

def fix_syntax_errors(file_path):
    """Fix syntax errors in Haskell files"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Fix duplicate imports and syntax errors
        # Remove duplicate imports
        lines = content.split('\n')
        new_lines = []
        seen_imports = set()
        
        for line in lines:
            # Check if it's an import line
            if line.strip().startswith('import '):
                # Normalize the import line for comparison
                normalized = re.sub(r'\s+', ' ', line.strip())
                if normalized not in seen_imports:
                    seen_imports.add(normalized)
                    new_lines.append(line)
            else:
                new_lines.append(line)
        
        # Fix common syntax errors
        content = '\n'.join(new_lines)
        
        # Fix double closing parentheses
        content = re.sub(r'\)\)', ')', content)
        
        # Fix imports with extra parentheses at end
        content = re.sub(r'^(import\s+.*)\)\s*$', r'\1', content, flags=re.MULTILINE)
        
        # Fix imports with extra parentheses and commas
        content = re.sub(r'^(import\s+.*?),\s*\)\s*$', r'\1', content, flags=re.MULTILINE)
        
        # Fix imports with extra parentheses and content
        content = re.sub(r'^(import\s+.*?)\)\s*\)\s*$', r'\1', content, flags=re.MULTILINE)
        
        # Write back if changed
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
        
        print(f"Fixed syntax errors in {file_path}")
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
        if fix_syntax_errors(file_path):
            fixed_count += 1
    
    print(f"Fixed {fixed_count} files")

if __name__ == "__main__":
    main()