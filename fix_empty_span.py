#!/usr/bin/env python3
import re

# Read the file
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestParserErrorRecoverySpec.hs', 'r') as f:
    content = f.read()

# Fix the undefined emptySpan variable by creating a simple span
content = re.sub(r'locatedAt pos value = Located value pos emptySpan',
                r'locatedAt pos value = Located value pos (spanBetween pos pos)', content)

# Write the file back
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestParserErrorRecoverySpec.hs', 'w') as f:
    f.write(content)

print("Fixed undefined emptySpan in TestParserErrorRecoverySpec.hs")