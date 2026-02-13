#!/usr/bin/env python3

# Read the file
with open('/home/qwe12345678/typus2/src/Compiler.hs', 'r') as f:
    content = f.read()

# Replace the problematic line in cleanCodeBlocks
old_line = '        not (isPrefixOf "import" line) &&'
new_line = '        not (isPrefixOf "import" line) &&'

# We're not changing this line for now, but we need to fix the logic
# Instead, let's add a new function to properly handle import blocks

# Find the location to insert the new function
insert_pos = content.find('-- Clean code blocks by removing duplicate package declarations and imports')

if insert_pos != -1:
    # Insert the new function before cleanCodeBlocks
    new_function = """-- Remove import block: lines between "import (" and ")"
removeImportBlock :: [String] -> [String]
removeImportBlock [] = []
removeImportBlock (line:rest) 
  | isPrefixOf "import (" line = dropWhile (not . (== ")")) rest
  | otherwise = line : removeImportBlock rest

"""
    
    content = content[:insert_pos] + new_function + content[insert_pos:]
    
    # Now modify the cleanCodeBlocks function to use removeImportBlock
    old_filter = '    filteredLines = filter (\\line -> \n        not (isPrefixOf "package main" line) && \n        not (isPrefixOf "import" line) &&\n        not (isImportLine line)) linesList'
    
    new_filter = '    filteredLines = removeImportBlock $ filter (\\line -> \n        not (isPrefixOf "package main" line) && \n        not (isImportLine line)) linesList'
    
    content = content.replace(old_filter, new_filter)
    
    # Write back
    with open('/home/qwe12345678/typus2/src/Compiler.hs', 'w') as f:
        f.write(content)
    
    print("Fixed cleanCodeBlocks function")
else:
    print("Could not find insertion point")