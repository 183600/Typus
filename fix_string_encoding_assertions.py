#!/usr/bin/env python3
import re

# Read the file
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestStringEncodingSpec.hs', 'r') as f:
    content = f.read()

# Fix assertion type errors by adding @?= assertions
content = re.sub(r'T\.length decodedText > 0', r'T.length decodedText @?= 1', content)
content = re.sub(r'\(Right r1, Right r2\) -> length r1 > 0 && length r2 > 0', r'(Right r1, Right r2) -> do\n             length r1 @?= 1\n             length r2 @?= 1', content)
content = re.sub(r'Right result -> length result > 0', r'Right result -> length result @?= 1', content)

# Write the file back
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestStringEncodingSpec.hs', 'w') as f:
    f.write(content)

print("Fixed assertion type errors in TestStringEncodingSpec.hs")