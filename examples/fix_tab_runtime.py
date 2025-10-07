#!/usr/bin/env python3

# Read the Compiler.hs file
with open('/home/qwe12345678/typus2/src/Compiler.hs', 'r') as f:
    content = f.read()

# Find the line with runtime imports and add the tab+runtime pattern
old_line = 'trimmed == "\\\"runtime\\\"" ||'
new_line = 'trimmed == "\\\"runtime\\\"" || trimmed == "\\truntime" ||'

content = content.replace(old_line, new_line)

# Write the file back
with open('/home/qwe12345678/typus2/src/Compiler.hs', 'w') as f:
    f.write(content)

print("Fixed Compiler.hs")