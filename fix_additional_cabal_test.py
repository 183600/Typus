#!/usr/bin/env python3
import os
import re

def fix_additional_cabal_test_spec():
    """Fix AdditionalCabalTestSpec.hs file"""
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/AdditionalCabalTestSpec.hs"
    
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        # Fix specific patterns in this file
        # Fix testCase indentation
        content = re.sub(r'\s+,+\s+testCase\s+"([^"]+)"\s+\$\s+do', r'\n    , testCase "\1" $ do', content)
        
        # Fix let bindings
        content = re.sub(r'let\s+source\s*=', r'let source =', content)
        
        # Fix case expressions
        content = re.sub(r'case\s+parseTypus\s+source\s+of', r'case parseTypus source of', content)
        
        # Fix list literals
        content = re.sub(r'\[\s*"([^"]+)"\s*\]', r'[ "\1" ]', content)
        
        # Write back
        with open(file_path, 'w') as f:
            f.write(content)
        
        print(f"Fixed {file_path}")
        return True
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return False

def main():
    fix_additional_cabal_test_spec()

if __name__ == "__main__":
    main()