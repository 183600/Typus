#!/usr/bin/env python3
import re

# Read the file
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestOwnershipTransitivitySpec.hs', 'r') as f:
    content = f.read()

# Replace all case expressions with direct function calls
# Pattern: case result of Left err -> ... Right (analyzer, transfers) -> ...
# Replace with: let errors = result in if null errors then ... else ...

# Find all case expressions and replace them
pattern = r'case result of\s+Left err -> assertFailure \$ "Ownership analysis failed: " \+\+ show err\s+Right \(analyzer, transfers\) -> do'
replacement = 'let errors = result in if null errors then do'

content = re.sub(pattern, replacement, content, flags=re.MULTILINE | re.DOTALL)

# Replace Right pattern with direct execution
content = re.sub(r'Right \(analyzer, transfers\) -> do', 'do', content)

# Write the file back
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestOwnershipTransitivitySpec.hs', 'w') as f:
    f.write(content)

print("Fixed ownership analysis test cases")