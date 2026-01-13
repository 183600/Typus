#!/usr/bin/env python3
import re

# Read the file
with open('/home/runner/work/Typus/Typus/test/Test/Unit/UtilsStringProcessingSpec.hs', 'r') as f:
    content = f.read()

# Replace all instances of "in <expression>" with "in property $ <expression>"
# where the expression is not already wrapped in property
content = re.sub(r'\nin\s+(?!property\s+\$)(.+)$', r'\nin property $ \1', content, flags=re.MULTILINE)

# Write the file back
with open('/home/runner/work/Typus/Typus/test/Test/Unit/UtilsStringProcessingSpec.hs', 'w') as f:
    f.write(content)

print("Fixed remaining Bool vs Property type mismatches")