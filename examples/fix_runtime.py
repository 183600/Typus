#!/usr/bin/env python3

# Read the file
with open('/home/qwe12345678/typus2/src/Compiler.hs', 'r') as f:
    content = f.read()

# Replace the problematic line
old_line = '          trimmed == \"\\\"strings\\\"\" || trimmed == \"\\\"math\\\"\"'
new_line = '          trimmed == \"\\\"strings\\\"\" || trimmed == \"\\\"math\\\"\" || trimmed == \"\\\"runtime\\\"\"'

content = content.replace(old_line, new_line)

# Write back
with open('/home/qwe12345678/typus2/src/Compiler.hs', 'w') as f:
    f.write(content)

print("Fixed Compiler.hs")