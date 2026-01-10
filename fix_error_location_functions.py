#!/usr/bin/env python3
import re

# Read the file
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestSourceLocationMathPropertiesSpec.hs', 'r') as f:
    content = f.read()

# Fix the undefined functions by using the line and column fields directly
content = re.sub(r'errorLine errLoc @\?= 5', r'line errLoc @?= 5', content)
content = re.sub(r'errorColumn errLoc @\?= 10', r'column errLoc @?= 10', content)
content = re.sub(r'errorEndLine errLoc @\?= Just 7', r'endLine errLoc @?= Just 7', content)
content = re.sub(r'errorEndColumn errLoc @\?= Just 15', r'endColumn errLoc @?= Just 15', content)

# Write the file back
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestSourceLocationMathPropertiesSpec.hs', 'w') as f:
    f.write(content)

print("Fixed undefined functions in TestSourceLocationMathPropertiesSpec.hs")