#!/usr/bin/env python3
"""
Fix compilation warnings in test files
"""

import os
import re
import sys

def fix_unused_imports(filepath):
    """Remove unused imports from a file"""
    with open(filepath, 'r') as f:
        content = f.read()
    
    # Remove unused Test.Tasty.HUnit imports
    content = re.sub(r'^import Test\.Tasty\.HUnit\s*$', '', content, flags=re.MULTILINE)
    
    # Remove unused TypeError import from BoundaryConditionComprehensiveSpec
    if 'BoundaryConditionComprehensiveSpec' in filepath:
        content = re.sub(
            r'import Compiler\.Errors\.Core \(TypeError\(\.\.\.\), ErrorSeverity\(\.\.\.\),',
            'import Compiler.Errors.Core (ErrorSeverity(..),',
            content
        )
    
    # Write back
    with open(filepath, 'w') as f:
        f.write(content)

def fix_span_shadowing(filepath):
    """Fix span name shadowing"""
    with open(filepath, 'r') as f:
        content = f.read()
    
    # Replace 'let span =' with 'let sourceSpan =' (but be careful not to replace function calls)
    lines = content.split('\n')
    new_lines = []
    for line in lines:
        if 'let span =' in line:
            line = line.replace('let span =', 'let sourceSpan =')
        new_lines.append(line)
    
    with open(filepath, 'w') as f:
        f.write('\n'.join(new_lines))

def fix_orphan_instances(filepath):
    """Add OPTIONS_GHC pragma to suppress orphan warnings"""
    with open(filepath, 'r') as f:
        content = f.read()
    
    # Add pragma at the top if not present
    if 'OPTIONS_GHC -Wno-orphans' not in content:
        lines = content.split('\n')
        # Find the language pragmas
        insert_idx = 0
        for i, line in enumerate(lines):
            if line.startswith('{-# LANGUAGE'):
                insert_idx = i + 1
            elif line.startswith('module'):
                break
        
        lines.insert(insert_idx, '{-# OPTIONS_GHC -Wno-orphans #-}')
        content = '\n'.join(lines)
    
    with open(filepath, 'w') as f:
        f.write(content)

def main():
    test_dir = 'test/Test/Unit'
    
    for root, dirs, files in os.walk(test_dir):
        for file in files:
            if file.endswith('.hs'):
                filepath = os.path.join(root, file)
                print(f"Processing {filepath}")
                
                # Fix common issues
                fix_unused_imports(filepath)
                
                # Fix span shadowing in specific files
                if 'BoundaryConditionsEnhancedQuickCheckSpec' in file:
                    fix_span_shadowing(filepath)
                
                # Fix orphan instances
                if 'BoundaryConditionsEnhancedQuickCheckSpec' in file:
                    fix_orphan_instances(filepath)

if __name__ == '__main__':
    main()