#!/usr/bin/env python3
"""
Fix specific duplicate modules in typus.cabal file
"""

def fix_specific_duplicates():
    with open('typus.cabal', 'r') as f:
        content = f.read()
    
    # Split content into lines
    lines = content.split('\n')
    
    # Modules to track for duplicates
    modules_to_track = [
        'Test.Unit.ErrorHandlerRecoverySpec',
        'Test.Unit.SourceLocationMathSpec',
        'Test.Unit.SyntaxValidatorBoundarySpec'
    ]
    
    # Find all occurrences of tracked modules
    module_positions = {module: [] for module in modules_to_track}
    
    for i, line in enumerate(lines):
        for module in modules_to_track:
            if module in line and not line.strip().startswith('--'):
                module_positions[module].append(i)
    
    # Remove duplicates (keep first occurrence)
    removed_count = 0
    for module, positions in module_positions.items():
        if len(positions) > 1:
            # Keep first occurrence, remove others
            for pos in reversed(positions[1:]):
                lines.pop(pos)
                removed_count += 1
                print(f"Removed duplicate {module} at line {pos+1}")
    
    # Write the fixed content back
    with open('typus.cabal', 'w') as f:
        f.write('\n'.join(lines))
    
    print(f"Fixed {removed_count} duplicate modules in typus.cabal")

if __name__ == "__main__":
    fix_specific_duplicates()