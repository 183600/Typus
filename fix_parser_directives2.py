#!/usr/bin/env python3
import re

# Read the file
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestParserDirectivesSpec.hs', 'r') as f:
    content = f.read()

# Replace Parser. qualified references with local ones in pattern matches
# We need to be careful not to replace the ones in import statements or type signatures
# We'll use a more targeted approach

# First, let's find all occurrences and fix them manually
# This is a bit tricky because we need to preserve the structure

# Replace in pattern matches and case expressions
content = re.sub(r'Parser\.fdOwnership\s+result', 'fdOwnership result', content)
content = re.sub(r'Parser\.fdDependentTypes\s+result', 'fdDependentTypes result', content)
content = re.sub(r'Parser\.fdConstraints\s+result', 'fdConstraints result', content)
content = re.sub(r'Parser\.bdOwnership\s+result', 'bdOwnership result', content)
content = re.sub(r'Parser\.bdDependentTypes\s+result', 'bdDependentTypes result', content)
content = re.sub(r'Parser\.bdConstraints\s+result', 'bdConstraints result', content)

# Write the file back
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestParserDirectivesSpec.hs', 'w') as f:
    f.write(content)

print("Fixed Parser. qualified references")