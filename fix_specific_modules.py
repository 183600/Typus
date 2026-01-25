#!/usr/bin/env python3
"""
Fix module declarations for specific files
"""

import os
import re

def fix_module_declaration(filepath):
    """Fix module declaration in a file"""
    with open(filepath, 'r') as f:
        content = f.read()
    
    # Extract module name from filepath
    module_match = re.search(r'test/Test/Unit/(.+)\.hs$', filepath)
    if not module_match:
        return False
    
    module_name = module_match.group(1).replace('/', '.')
    full_module_name = f'Test.Unit.{module_name}'
    
    # Remove any existing module declaration
    content = re.sub(r'^module .+ where', '', content, flags=re.MULTILINE)
    
    # Find where to insert the module declaration
    lines = content.split('\n')
    insert_idx = 0
    
    # Skip pragmas at the top
    i = 0
    while i < len(lines) and lines[i].startswith('{-#'):
        i += 1
    
    insert_idx = i
    
    # Insert the module declaration
    lines.insert(insert_idx, f'module {full_module_name} where')
    content = '\n'.join(lines)
    
    with open(filepath, 'w') as f:
        f.write(content)
    return True

def main():
    files_to_fix = [
        'test/Test/Unit/AdditionalQuickCheckTestSuiteSpec.hs',
        'test/Test/Unit/AdditionalQuickCheckTestsSpec.hs',
        'test/Test/Unit/AdvancedTextProcessingSpec.hs',
        'test/Test/Unit/BoundaryConditionComprehensiveSpec.hs',
        'test/Test/Unit/BoundaryConditionsEnhancedQuickCheckSpec.hs'
    ]
    
    modified = 0
    for filepath in files_to_fix:
        if os.path.exists(filepath):
            if fix_module_declaration(filepath):
                print(f"Fixed module declaration in {filepath}")
                modified += 1
    
    print(f"Modified {modified} files")

if __name__ == '__main__':
    main()