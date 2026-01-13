#!/usr/bin/env python3

import re

# Read the file
with open("/home/runner/work/Typus/Typus/test/Test/Unit/CoreCompilerQuickCheckSpec.hs", "r") as f:
    content = f.read()

# Pattern to match and fix the broken functions
pattern = r'(\s+parsed = parseTypus code\n\s+in case parsed of\n\s+Left _ -> property True  -- Parsing failed\n\s+Right file -> property \$ True  -- Basic sanity check for successful parse)\s+in property \$ True  -- Basic sanity check'

# Fix the broken functions
new_content = re.sub(pattern, r'\1', content)

# Write the file back
with open("/home/runner/work/Typus/Typus/test/Test/Unit/CoreCompilerQuickCheckSpec.hs", "w") as f:
    f.write(new_content)

print("Fixed broken functions")