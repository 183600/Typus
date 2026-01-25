#!/usr/bin/env python3
"""
Add module declarations to files that are missing them
"""

import os
import re

def add_module_if_missing(filepath):
    """Add module declaration if missing"""
    with open(filepath, 'r') as f:
        content = f.read()
    
    # Skip if already has module declaration
    if re.search(r'^module ', content, flags=re.MULTILINE):
        return False
    
    # Extract module name from filepath
    module_match = re.search(r'test/Test/Unit/(.+).hs$', filepath)
    if not module_match:
        return False
    
    module_name = module_match.group(1).replace('/', '.')
    full_module_name = f'Test.Unit.{module_name}'
    
    # Find where to insert the module declaration
    lines = content.split('
')
    insert_idx = 0
    
    # Skip pragmas at the top
    i = 0
    while i < len(lines) and lines[i].startswith('{-#'):
        i += 1
    
    insert_idx = i
    
    # Insert the module declaration
    lines.insert(insert_idx, f'module {full_module_name} where')
    content = '
'.join(lines)
    
    with open(filepath, 'w') as f:
        f.write(content)
    return True

def main():
    # Find files with GHC-28623 errors
    import subprocess
    result = subprocess.run(
        ['stack', 'test', '--flag', '*:fast', '--flag', '*:-production', 
         '--ghc-options=-O0 -rtsopts', '--jobs=1'],
        capture_output=True, text=True
    )
    
    files = set()
    for line in result.stderr.split('
'):
        if 'GHC-28623' in line and 'File name does not match module name' in line:
            match = re.search(r'^(/[^:]+):', line)
            if match:
                files.add(match.group(1))
    
    print(f"Found {len(files)} files that need module declarations")
    
    modified = 0
    for filepath in files:
        if add_module_if_missing(filepath):
            print(f"  Added module to {filepath}")
            modified += 1
    
    print(f"Modified {modified} files")

if __name__ == '__main__':
    main()