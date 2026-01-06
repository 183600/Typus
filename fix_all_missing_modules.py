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

# Remove all Test.Unit modules from benchmark section
lines = cabal_content.split('\n')
new_lines = []
in_benchmark = False
in_benchmark_other_modules = False

for i, line in enumerate(lines):
    if 'benchmark typus-bench' in line:
        in_benchmark = True
        new_lines.append(line)
        continue
    
    if in_benchmark and line.strip() == 'other-modules:':
        in_benchmark_other_modules = True
        new_lines.append(line)
        continue
    
    if in_benchmark and in_benchmark_other_modules:
        if line.strip() and not line.startswith('        ') and not line.startswith('\t'):
            in_benchmark = False
            in_benchmark_other_modules = False
            new_lines.append(line)
        elif line.strip().startswith('Test.Unit'):
            # Skip Test.Unit modules in benchmark other-modules
            continue
        else:
            new_lines.append(line)
        continue
    
    if in_benchmark and line.strip().startswith('build-depends:'):
        in_benchmark = False
        new_lines.append(line)
        continue
    
    new_lines.append(line)

# Find the test-suite other-modules section and add all missing modules
for i, line in enumerate(new_lines):
    if 'test-suite typus-test' in line:
        # Look for other-modules in this test-suite
        for j in range(i, len(new_lines)):
            if new_lines[j].strip() == 'other-modules:':
                # Insert all missing modules after the other-modules line
                insert_pos = j + 1
                for module in clean_modules:
                    new_lines.insert(insert_pos, f'        , {module}')
                    insert_pos += 1
                break
        break

# Write the updated cabal file
with open('typus.cabal', 'w') as f:
    f.write('\n'.join(new_lines))

print(f"Added {len(clean_modules)} missing modules to test-suite other-modules")