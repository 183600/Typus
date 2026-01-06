#!/usr/bin/env python3

# Read the missing modules
with open('missing_modules.txt', 'r') as f:
    missing_modules = f.read().strip().split('\n')

# Clean up module names
clean_modules = []
for module in missing_modules:
    module = module.strip()
    if module and module.startswith('Test.Unit'):
        clean_modules.append(module)

# Read the cabal file
with open('typus.cabal', 'r') as f:
    lines = f.readlines()

# Remove all Test.Unit modules from benchmark section
new_lines = []
in_benchmark = False

for line in lines:
    if 'benchmark typus-bench' in line:
        in_benchmark = True
        new_lines.append(line)
        continue
    
    if in_benchmark and line.strip() == 'other-modules:':
        new_lines.append(line)
        # Skip all Test.Unit modules in benchmark other-modules
        continue
    
    if in_benchmark and line.strip().startswith('Test.Unit'):
        # Skip Test.Unit modules in benchmark other-modules
        continue
    
    if in_benchmark and line.strip() and not line.startswith('        ') and not line.startswith('\t'):
        in_benchmark = False
        new_lines.append(line)
        continue
    
    new_lines.append(line)

# Find the test-suite other-modules section and add all missing modules
test_suite_start = -1
for i, line in enumerate(new_lines):
    if 'test-suite typus-test' in line:
        test_suite_start = i
        break

if test_suite_start != -1:
    # Find the other-modules line in test-suite
    other_modules_line = -1
    for i in range(test_suite_start, len(new_lines)):
        if new_lines[i].strip() == 'other-modules:':
            other_modules_line = i
            break
    
    if other_modules_line != -1:
        # Add all missing modules after the other-modules line
        insert_pos = other_modules_line + 1
        for module in clean_modules:
            new_lines.insert(insert_pos, f'        , {module}\n')
            insert_pos += 1

# Write the updated cabal file
with open('typus.cabal', 'w') as f:
    f.writelines(new_lines)

print(f"Added {len(clean_modules)} missing modules to test-suite other-modules")