#!/usr/bin/env python3

# Read the file
with open('/home/qwe12345678/typus2/src/Compiler.hs', 'r') as f:
    content = f.read()

# Find the cleanCodeBlocks function and replace it
old_function = """-- Clean code blocks by removing duplicate package declarations and imports
cleanCodeBlocks :: String -> String
cleanCodeBlocks content = 
  let
    linesList = lines content
    -- Remove lines that start with "package main" or "import" 
    -- as these are already handled by the header and imports generation
    -- Also remove lines that are just import statements (like "fmt" or "sync")
    -- Also remove lines that are just quoted import statements (like "\\\"fmt\\\"")
    filteredLines = filter (\\line -> 
        not (isPrefixOf "package main" line) && 
        not (isPrefixOf "import" line) &&
        not (isImportLine line)) linesList
  in
    unlines filteredLines"""

new_function = """-- Clean code blocks by removing duplicate package declarations and imports
cleanCodeBlocks :: String -> String
cleanCodeBlocks content = 
  let
    linesList = lines content
    -- Remove lines that start with "package main" 
    -- as these are already handled by the header and imports generation
    -- Remove the entire import block (import ( ... ))
    -- Also remove lines that are just import statements (like "fmt" or "sync")
    -- Also remove lines that are just quoted import statements (like "\\\"fmt\\\"")
    filteredLines = removeImportBlock $ filter (\\line -> 
        not (isPrefixOf "package main" line) && 
        not (isImportLine line)) linesList
  in
    unlines filteredLines
  where
    -- Remove import block: lines between "import (" and ")"
    removeImportBlock [] = []
    removeImportBlock (line:rest) 
      | isPrefixOf "import (" line = dropWhile (not . (== ")")) rest
      | otherwise = line : removeImportBlock rest"""

content = content.replace(old_function, new_function)

# Write back
with open('/home/qwe12345678/typus2/src/Compiler.hs', 'w') as f:
    f.write(content)

print("Fixed cleanCodeBlocks function")