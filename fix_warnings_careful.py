#!/usr/bin/env python3
"""
Fix compilation warnings more carefully
"""

import os
import re
import sys

def fix_file(filepath):
    """Fix warnings in a specific file"""
    with open(filepath, 'r') as f:
        content = f.read()
    
    modified = False
    
    # Check if the file actually uses HUnit features
    uses_testCase = 'testCase' in content
    uses_assertion = '@?=' in content or 'assert' in content
    
    # If HUnit is not used, remove the import
    if not uses_testCase and not uses_assertion:
        if re.search(r'^import Test\.Tasty\.HUnit\s*$', content, flags=re.MULTILINE):
            content = re.sub(r'^import Test\.Tasty\.HUnit\s*\n?', '', content, flags=re.MULTILINE)
            modified = True
            print(f"  Removed unused Test.Tasty.HUnit import")
    
    # Fix specific issues in BoundaryConditionComprehensiveSpec
    if 'BoundaryConditionComprehensiveSpec' in filepath:
        if 'import Compiler.Errors.Core (TypeError(..), ErrorSeverity(..),' in content:
            content = re.sub(
                r'import Compiler\.Errors\.Core \(TypeError\(\.\.\.\), ErrorSeverity\(\.\.\.\),',
                'import Compiler.Errors.Core (ErrorSeverity(..),',
                content
            )
            modified = True
            print(f"  Fixed unused TypeError import")
    
    # Fix span shadowing
    if 'let span =' in content:
        lines = content.split('\n')
        new_lines = []
        for line in lines:
            if 'let span =' in line and not ('spanStart' in line or 'spanEnd' in line):
                line = line.replace('let span =', 'let sourceSpan =')
                modified = True
                print(f"  Fixed span shadowing")
            new_lines.append(line)
        content = '\n'.join(new_lines)
    
    # Add orphan pragma for BoundaryConditionsEnhancedQuickCheckSpec
    if 'BoundaryConditionsEnhancedQuickCheckSpec' in filepath:
        if 'OPTIONS_GHC -Wno-orphans' not in content:
            lines = content.split('\n')
            insert_idx = 0
            for i, line in enumerate(lines):
                if line.startswith('{-# LANGUAGE'):
                    insert_idx = i + 1
                elif line.startswith('module'):
                    break
            lines.insert(insert_idx, '{-# OPTIONS_GHC -Wno-orphans #-}')
            content = '\n'.join(lines)
            modified = True
            print(f"  Added orphan pragma")
    
    # Write back if modified
    if modified:
        with open(filepath, 'w') as f:
            f.write(content)
        return True
    return False

def main():
    test_dir = 'test/Test/Unit'
    modified_files = []
    
    for root, dirs, files in os.walk(test_dir):
        for file in files:
            if file.endswith('.hs'):
                filepath = os.path.join(root, file)
                print(f"Checking {filepath}")
                if fix_file(filepath):
                    modified_files.append(filepath)
    
    print(f"\nModified {len(modified_files)} files")

if __name__ == '__main__':
    main()