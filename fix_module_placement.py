#!/usr/bin/env python3

# Read the cabal file
with open('typus.cabal', 'r') as f:
    lines = f.readlines()

# Find the benchmark other-modules section and remove the test modules
# Then add them to the test-suite other-modules section

benchmark_other_modules_start = -1
benchmark_other_modules_end = -1
test_suite_other_modules_start = -1
test_suite_other_modules_end = -1

test_modules = []

# First, find the sections
for i, line in enumerate(lines):
    if 'benchmark typus-bench' in line:
        for j in range(i, len(lines)):
            if lines[j].strip() == 'other-modules:':
                benchmark_other_modules_start = j
                # Find the end of this section
                for k in range(j + 1, len(lines)):
                    if lines[k].strip() and not lines[k].startswith('        ') and not lines[k].startswith('\t'):
                        benchmark_other_modules_end = k
                        break
                break
        break

for i, line in enumerate(lines):
    if 'test-suite typus-test' in line:
        for j in range(i, len(lines)):
            if lines[j].strip() == 'other-modules:':
                test_suite_other_modules_start = j
                # Find the end of this section
                for k in range(j + 1, len(lines)):
                    if lines[k].strip() and not lines[k].startswith('        ') and not lines[k].startswith('\t'):
                        test_suite_other_modules_end = k
                        break
                break
        break

# Extract test modules from benchmark section
if benchmark_other_modules_start != -1 and benchmark_other_modules_end != -1:
    for i in range(benchmark_other_modules_start + 1, benchmark_other_modules_end):
        line = lines[i].strip()
        if line and line.startswith('Test.Unit'):
            test_modules.append(line)
            # Mark this line for removal
            lines[i] = ""

# Remove empty lines from benchmark section
new_lines = []
for line in lines:
    if line.strip() or not (benchmark_other_modules_start < lines.index(line) < benchmark_other_modules_end):
        new_lines.append(line)
lines = new_lines

# Add test modules to test-suite section
if test_suite_other_modules_start != -1 and test_suite_other_modules_end != -1:
    insert_pos = test_suite_other_modules_end
    for module in test_modules:
        lines.insert(insert_pos, f"        {module}\n")
        insert_pos += 1

# Write the fixed cabal file
with open('typus.cabal', 'w') as f:
    f.writelines(lines)

print(f"Moved {len(test_modules)} test modules from benchmark to test-suite section")