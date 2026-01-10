#!/usr/bin/env python3
import re

# Read the file
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestParserDirectivesSpec.hs', 'r') as f:
    content = f.read()

# Replace ambiguous references
content = re.sub(r'\bfdOwnership\b', 'Parser.fdOwnership', content)
content = re.sub(r'\bfdDependentTypes\b', 'Parser.fdDependentTypes', content)
content = re.sub(r'\bfdConstraints\b', 'Parser.fdConstraints', content)
content = re.sub(r'\bbdOwnership\b', 'Parser.bdOwnership', content)
content = re.sub(r'\bbdDependentTypes\b', 'Parser.bdDependentTypes', content)
content = re.sub(r'\bbdConstraints\b', 'Parser.bdConstraints', content)
content = re.sub(r'\bLocated\b', 'SourceLocation.Located', content)
content = re.sub(r'\bSourcePos\b', 'SourceLocation.SourcePos', content)

# Write the file back
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestParserDirectivesSpec.hs', 'w') as f:
    f.write(content)

print("Fixed ambiguous references")