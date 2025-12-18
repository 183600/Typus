#!/usr/bin/env python3

# Read the file
with open('/home/qwe12345678/typus2/src/Compiler.hs', 'r') as f:
    content = f.read()

# Find and replace the removeImportBlock function
old_function = """-- Remove import block: lines between "import (" and ")"
removeImportBlock :: [String] -> [String]
removeImportBlock [] = []
removeImportBlock (line:rest) 
  | isPrefixOf "import (" line = dropWhile (not . (== ")")) rest
  | otherwise = line : removeImportBlock rest"""

new_function = """-- Remove import block: lines between "import (" and ")"
removeImportBlock :: [String] -> [String]
removeImportBlock [] = []
removeImportBlock (line:rest) 
  | isPrefixOf "import (" line = 
      let (importBlock, remaining) = span (not . (== ")")) rest
      in drop 1 remaining  -- Drop the closing ")" as well
  | otherwise = line : removeImportBlock rest"""

content = content.replace(old_function, new_function)

# Write back
with open('/home/qwe12345678/typus2/src/Compiler.hs', 'w') as f:
    f.write(content)

print("Fixed removeImportBlock function")