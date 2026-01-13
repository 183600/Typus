#!/usr/bin/env python3

import re

# Read the file
with open('/home/runner/work/Typus/Typus/test/Test/Unit/CoreErrorHandlerQuickCheckSpec.hs', 'r') as f:
    content = f.read()

# Fix variable names
content = content.replace("getErrors errors'", "getErrors collector'")
content = content.replace("getWarnings warnings'", "getWarnings collector'")
content = content.replace("getInfo infos'", "getInfo collector'")

# Write the file back
with open('/home/runner/work/Typus/Typus/test/Test/Unit/CoreErrorHandlerQuickCheckSpec.hs', 'w') as f:
    f.write(content)

print("Fixed variable names")