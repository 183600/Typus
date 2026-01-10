#!/usr/bin/env python3
import re

# Read the file
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestStringEncodingSpec.hs', 'r') as f:
    content = f.read()

# Fix parseTypus calls
content = re.sub(r'parseTypus input "([^"]+)"', r'parseTypus input', content)

# Write the file back
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestStringEncodingSpec.hs', 'w') as f:
    f.write(content)

print("Fixed parseTypus calls in TestStringEncodingSpec.hs")