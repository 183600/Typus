#!/usr/bin/env python3

# Read the cabal file
with open('typus.cabal', 'r') as f:
    lines = f.readlines()

# Fix the syntax issue with commas at the beginning of other-modules sections
fixed_lines = []
in_other_modules = False
first_module = True

for line in lines:
    stripped = line.strip()
    
    # Check if we're at the start of an other-modules section
    if stripped == 'other-modules:':
        in_other_modules = True
        first_module = True
        fixed_lines.append(line)
        continue
    
    # Check if we're leaving the other-modules section
    if in_other_modules and stripped and not stripped.startswith(',') and not stripped.startswith('Test.') and not stripped.startswith('--') and not stripped.startswith('        ') and not stripped.startswith('#'):
        in_other_modules = False
        first_module = False
    
    # Fix the first module in other-modules section
    if in_other_modules and first_module and stripped.startswith(','):
        # Remove the comma from the first module
        fixed_line = line.replace(',', '        ', 1)  # Replace with proper indentation
        fixed_lines.append(fixed_line)
        first_module = False
        continue
    
    fixed_lines.append(line)

# Write the fixed cabal file
with open('typus.cabal', 'w') as f:
    f.writelines(fixed_lines)

print("Fixed cabal syntax issues")