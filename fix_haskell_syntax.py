#!/usr/bin/env python3
import os
import re

def fix_haskell_file(file_path):
    """Fix common Haskell syntax errors in a file"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        original_content = content
        
        # Fix function definitions with extra spaces
        content = re.sub(r'(\w+)\s+::\s+(\w+)\s+\n\s+(\w+)\s+=', r'\1 :: \2\n\1 =', content)
        
        # Fix testCase indentation
        content = re.sub(r'\[\s+testCase\s+"([^"]+)"\s+\$\s+do', r'[ testCase "\1" $ do', content)
        
        # Fix let bindings
        content = re.sub(r'let\s+(\w+)\s*=', r'let \1 =', content)
        
        # Fix case expressions
        content = re.sub(r'case\s+(\w+)\s+of', r'case \1 of', content)
        
        # Fix newtype definitions
        content = re.sub(r'newtype\s+(\w+)\s*=', r'newtype \1 =', content)
        
        # Fix deriving clauses
        content = re.sub(r'deriving\s*\(([^)]+)\)', r'deriving (\1)', content)
        
        # Fix instance definitions
        content = re.sub(r'instance\s+(\w+)\s+where', r'instance \1 where', content)
        
        # Fix arbitrary functions
        content = re.sub(r'arbitrary\s*=\s*do', r'arbitrary = do', content)
        
        # Fix testGroup definitions
        content = re.sub(r'testGroup\s+"([^"]+)"', r'testGroup "\1"', content)
        
        # Write back if changed
        if content != original_content:
            with open(file_path, 'w') as f:
                f.write(content)
            print(f"Fixed syntax in {file_path}")
            return True
        return False
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return False

def main():
    # Fix the specific problematic file
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/AdditionalCabalTestSpec.hs"
    if fix_haskell_file(file_path):
        print(f"Fixed {file_path}")
    else:
        print(f"No changes needed for {file_path}")

if __name__ == "__main__":
    main()