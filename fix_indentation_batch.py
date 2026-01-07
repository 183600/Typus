#!/usr/bin/env python3
import os
import re

def fix_indentation_errors(file_path):
    """Fix common indentation errors in Haskell test files"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        original_content = content
        
        # Fix various indentation patterns
        patterns = [
            # Fix do block indentation
            (r'(\s+)do\s*\n(\s+)(\w+)', r'\1do\n\2    \3'),
            
            # Fix let bindings in do blocks
            (r'(\s+)let\s+(\w+)', r'\1let \2'),
            
            # Fix case expressions
            (r'(\s+)(\w+)\s+->', r'\1\2 ->'),
            
            # Fix assertEqual indentation
            (r'(\s+)assertEqual', r'\1    assertEqual'),
            
            # Fix testCase indentation
            (r'(\s+)testCase', r'\1    testCase'),
            
            # Fix testProperty indentation
            (r'(\s+)testProperty', r'\1    testProperty'),
            
            # Fix record field indentation
            (r'(\s+)(\w+)\s*=', r'\1          \2 ='),
            
            # Fix list comprehension indentation
            (r'(\s+),\s*(do|return)', r'\1    , \2'),
            
            # Fix instance Arbitrary indentation
            (r'(\s+)instance\s+', r'\1instance '),
            
            # Fix arbitrary function indentation
            (r'(\s+)arbitrary\s*=', r'\1    arbitrary ='),
        ]
        
        for pattern, replacement in patterns:
            content = re.sub(pattern, replacement, content, flags=re.MULTILINE)
        
        # Write back if changed
        if content != original_content:
            with open(file_path, 'w') as f:
                f.write(content)
            print(f"Fixed indentation in {file_path}")
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
        if fix_indentation_errors(file_path):
            fixed_count += 1
    
    print(f"Fixed indentation in {fixed_count} files")

if __name__ == "__main__":
    main()