#!/usr/bin/env python3
import re

# Read the file
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestParserDirectivesSpec.hs', 'r') as f:
    content = f.read()

# Fix the double SourceLocation references
content = re.sub(r'SourceLocation\.SourceLocation\.', 'SourceLocation.', content)

# Write the file back
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestParserDirectivesSpec.hs', 'w') as f:
    f.write(content)

print("Fixed double SourceLocation references")