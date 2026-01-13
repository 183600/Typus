#!/usr/bin/env python3
import re

# Read the file
with open('/home/runner/work/Typus/Typus/test/Test/Unit/UtilsStringProcessingSpec.hs', 'r') as f:
    content = f.read()

# Pattern to match property functions that return Bool instead of Property
pattern = r'(prop_\w+ :: .+?\nprop_\w+ .+?\n(?:.*?\n)*?.*?)in\s+([^p].*)'

# Replace with property wrapper
def replace_func(match):
    func_content = match.group(1)
    return_expr = match.group(2)
    return f"{func_content}in property $ {return_expr}"

# Apply the replacement
content = re.sub(pattern, replace_func, content, flags=re.MULTILINE | re.DOTALL)

# Write the file back
with open('/home/runner/work/Typus/Typus/test/Test/Unit/UtilsStringProcessingSpec.hs', 'w') as f:
    f.write(content)

print("Fixed Bool vs Property type mismatches")