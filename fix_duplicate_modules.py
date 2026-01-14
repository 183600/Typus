#!/usr/bin/env python3
"""
Fix duplicate modules in typus.cabal file
"""

def fix_duplicate_modules():
    with open('typus.cabal', 'r') as f:
        content = f.read()
    
    # Split content into lines
    lines = content.split('\n')
    
    # Find all occurrences of duplicate modules
    modules_to_remove = []
    
    # Track seen modules
    seen_modules = set()
    
    # Find all module lines
    for i, line in enumerate(lines):
        line = line.strip()
        if line.startswith('Test.Unit.'):
            module = line.rstrip(',')
            if module in seen_modules:
                modules_to_remove.append((i, module))
                print(f"Found duplicate {module} at line {i+1}")
            else:
                seen_modules.add(module)
    
    # Remove duplicates from bottom to top to preserve line numbers
    for i, module in sorted(modules_to_remove, reverse=True):
        lines.pop(i)
        print(f"Removed duplicate {module} at line {i+1}")
    
    # Write the fixed content back
    with open('typus.cabal', 'w') as f:
        f.write('\n'.join(lines))
    
    print("Fixed duplicate modules in typus.cabal")

if __name__ == "__main__":
    fix_duplicate_modules()