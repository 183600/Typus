#!/usr/bin/env python3

# Read the file
with open('/home/runner/work/Typus/Typus/test/Test/Unit/UtilsStringProcessingSpec.hs', 'r') as f:
    lines = f.readlines()

# Process each line
new_lines = []
for line in lines:
    # Replace lines that start with "  in " and don't already have "property $"
    if line.strip().startswith("in ") and "property $" not in line:
        new_lines.append("  in property $ " + line.strip()[3:] + "\n")
    else:
        new_lines.append(line)

# Write the file back
with open('/home/runner/work/Typus/Typus/test/Test/Unit/UtilsStringProcessingSpec.hs', 'w') as f:
    f.writelines(new_lines)

print("Fixed all Bool vs Property type mismatches")