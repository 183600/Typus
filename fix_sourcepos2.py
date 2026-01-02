#!/usr/bin/env python3

import re

# Read the file
with open('/home/runner/work/Typus/Typus/test/Test/Unit/CompilerIRPropertiesSpec.hs', 'r') as f:
    content = f.read()

# Fix SourcePos constructor calls - it needs 3 arguments (line, column, offset)
# Replace all instances of SourcePos with more than 3 arguments
content = re.sub(r'SourcePos (\d+) (\d+) (\d+) \d+ \d+', lambda m: f"SourcePos {m.group(1)} {m.group(2)} {m.group(3)}", content)
content = re.sub(r'SourcePos (\d+) (\d+) \d+ \d+', lambda m: f"SourcePos {m.group(1)} {m.group(2)} 0", content)
content = re.sub(r'SourcePos 1 \(length (\w+) \+ 1\)', r'SourcePos 1 (length \1 + 1) (length \1)', content)
content = re.sub(r'SourcePos n \(length (\w+) \+ 1\)', r'SourcePos n (length \1 + 1) 0', content)

# Fix the goIR issue
content = re.sub(r'property \$ show goIR `seq` True', 'property $ show (emitGo semanticIR) `seq` True', content)

# Write the file back
with open('/home/runner/work/Typus/Typus/test/Test/Unit/CompilerIRPropertiesSpec.hs', 'w') as f:
    f.write(content)

print("Fixed CompilerIRPropertiesSpec.hs again")