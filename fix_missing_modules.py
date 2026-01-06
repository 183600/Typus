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
    cabal_content = f.read()

# Find the other-modules section in test-suite
lines = cabal_content.split('\n')
other_modules_start = -1
other_modules_end = -1

for i, line in enumerate(lines):
    if 'test-suite typus-test' in line:
        # Look for other-modules in this test-suite
        for j in range(i, len(lines)):
            if lines[j].strip() == 'other-modules:':
                other_modules_start = j
                # Find the end of this section
                for k in range(j + 1, len(lines)):
                    if lines[k].strip() and not lines[k].startswith('        ') and not lines[k].startswith('\t'):
                        other_modules_end = k
                        break
                break
        break

if other_modules_start == -1:
    print("Could not find other-modules section")
    exit(1)

# Insert the missing modules
insert_pos = other_modules_start + 1
for module in clean_modules:
    lines.insert(insert_pos, f'        , {module}')
    insert_pos += 1

# Write the updated cabal file
with open('typus.cabal', 'w') as f:
    f.write('\n'.join(lines))

print(f"Added {len(clean_modules)} missing modules to typus.cabal")