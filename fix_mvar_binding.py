#!/usr/bin/env python3
import os
import re

def fix_mvar_binding(file_path):
    """Fix MVar binding issues in test files"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix MVar binding issues by ensuring proper do block
    # Find all testCase blocks and ensure proper indentation
    lines = content.split('\n')
    new_lines = []
    i = 0
    while i < len(lines):
        line = lines[i]
        new_lines.append(line)
        
        # If we find a testCase line, ensure the next line with MVar binding is properly indented
        if 'testCase' in line and '$ do' in line:
            # Look ahead for the next non-empty line
            j = i + 1
            while j < len(lines) and (lines[j].strip() == '' or lines[j].strip().startswith('--')):
                new_lines.append(lines[j])
                j += 1
            
            if j < len(lines) and 'resultsVar <- newEmptyMVar' in lines[j]:
                # Ensure this line is properly indented
                if not lines[j].startswith('            '):
                    new_lines[-1] = '            ' + lines[j].strip()
                else:
                    new_lines.append(lines[j])
                i = j
            else:
                i = j
        else:
            i += 1
    
    with open(file_path, 'w') as f:
        f.write('\n'.join(new_lines))
    print(f"Fixed MVar binding errors in {file_path}")

def main():
    # Fix specific files
    files_to_fix = [
        "test/Test/Unit/CabalConcurrentParsingSpec.hs",
    ]
    
    for file_path in files_to_fix:
        if os.path.exists(file_path):
            fix_mvar_binding(file_path)

if __name__ == "__main__":
    main()