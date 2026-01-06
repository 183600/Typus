#!/usr/bin/env python3
import os
import re
import subprocess

def fix_import_parens():
    """Fix paren issues in imports"""
    # Fix ConciseSourceLocationQuickCheckSpec.hs
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/ConciseSourceLocationQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix the malformed import
    content = re.sub(
        r'import Test\.Tasty\.QuickCheck \((\([^)]+\), [^)]+)\)',
        r'import Test.Tasty.QuickCheck (\1)',
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    print("Fixed import parens in ConciseSourceLocationQuickCheckSpec.hs")
    
    # Fix ConciseSyntaxValidatorQuickCheckSpec.hs
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/ConciseSyntaxValidatorQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix the malformed import
    content = re.sub(
        r'import Test\.Tasty\.QuickCheck \(([^)]* \([^)]+\), [^)]+)\)',
        r'import Test.Tasty.QuickCheck (\1)',
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    print("Fixed import parens in ConciseSyntaxValidatorQuickCheckSpec.hs")

def fix_import_placement():
    """Fix import placement in ConciseUtilsQuickCheckSpec.hs"""
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/ConciseUtilsQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Check if there's a stray import in the middle
    lines = content.split('\n')
    new_lines = []
    imports_section = True
    for i, line in enumerate(lines):
        if line.startswith('import ') and i > 10:  # Import not at the top
            continue  # Skip this line
        new_lines.append(line)
    
    content = '\n'.join(new_lines)
    
    with open(file_path, 'w') as f:
        f.write(content)
    print("Fixed import placement in ConciseUtilsQuickCheckSpec.hs")

def main():
    """Main function to run remaining fixes"""
    print("Starting to fix remaining compilation errors...")
    
    fix_import_parens()
    fix_import_placement()
    
    print("All fixes applied. Running cabal test to check for remaining errors...")
    cmd = "GHCRTS='-M2G -A16m' cabal test -j1 --flags='fast -production' --ghc-options='-O0 -rtsopts' --test-options='+RTS -M1024m -A16m -RTS' --test-show-details=direct 2>&1 | grep -A 5 'error:' | head -100"
    stdout, stderr = subprocess.run(cmd, shell=True, capture_output=True, text=True).stdout, subprocess.run(cmd, shell=True, capture_output=True, text=True).stderr
    
    if stdout:
        print("Remaining errors:")
        print(stdout)
    else:
        print("No more compilation errors found!")

if __name__ == "__main__":
    main()