#!/usr/bin/env python3
import re

# Read the file
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestSourceLocationMathPropertiesSpec.hs', 'r') as f:
    content = f.read()

# Fix the type mismatch by using errorLine and errorColumn functions
content = re.sub(r'posLine errLoc @\?= 5', r'errorLine errLoc @?= 5', content)
content = re.sub(r'posColumn errLoc @\?= 10', r'errorColumn errLoc @?= 10', content)
content = re.sub(r'endLine errLoc @\?= Just 7', r'errorEndLine errLoc @?= Just 7', content)
content = re.sub(r'endColumn errLoc @\?= Just 15', r'errorEndColumn errLoc @?= Just 15', content)

# Write the file back
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestSourceLocationMathPropertiesSpec.hs', 'w') as f:
    f.write(content)

print("Fixed type mismatch in TestSourceLocationMathPropertiesSpec.hs")