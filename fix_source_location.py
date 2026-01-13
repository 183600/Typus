#!/usr/bin/env python3

import re

# Read the file
with open('/home/runner/work/Typus/Typus/test/Test/Unit/CoreErrorHandlerQuickCheckSpec.hs', 'r') as f:
    content = f.read()

# Replace all occurrences of SourceLocation span with ErrorLocation (Just "test") 0 0 Nothing Nothing
content = content.replace('SourceLocation span', 'ErrorLocation (Just "test") 0 0 Nothing Nothing')

# Write the file back
with open('/home/runner/work/Typus/Typus/test/Test/Unit/CoreErrorHandlerQuickCheckSpec.hs', 'w') as f:
    f.write(content)

print("Replaced all SourceLocation span with ErrorLocation")