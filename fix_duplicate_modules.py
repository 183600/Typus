#!/usr/bin/env python3
"""
Fix duplicate module declarations
"""

import os
import re

def fix_duplicate_modules():
    """Fix duplicate module declarations"""
    test_dir = 'test/Test/Unit'
    modified = 0
    
    for root, dirs, files in os.walk(test_dir):
        for file in files:
            if file.endswith('.hs'):
                filepath = os.path.join(root, file)
                
                with open(filepath, 'r') as f:
                    content = f.read()
                
                # Check for duplicate module declarations
                module_pattern = r'^module .+ where'
                modules = re.findall(module_pattern, content, flags=re.MULTILINE)
                
                if len(modules) > 1:
                    # Keep only the first module declaration
                    lines = content.split('\n')
                    new_lines = []
                    seen_module = False
                    
                    for line in lines:
                        if re.match(module_pattern, line):
                            if not seen_module:
                                new_lines.append(line)
                                seen_module = True
                        else:
                            new_lines.append(line)
                    
                    content = '\n'.join(new_lines)
                    
                    with open(filepath, 'w') as f:
                        f.write(content)
                    
                    modified += 1
                    print(f"Fixed duplicate module in {filepath}")
    
    print(f"Modified {modified} files")

if __name__ == '__main__':
    fix_duplicate_modules()