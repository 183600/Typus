#!/usr/bin/env python3

# Read the file
with open('/home/qwe12345678/typus2/src/Compiler.hs', 'r') as f:
    content = f.read()

# Find and replace the removeImportBlock function and cleanCodeBlocks function
old_functions = """-- Remove import block: lines between "import (" and ")"
removeImportBlock :: [String] -> [String]
removeImportBlock [] = []
removeImportBlock (line:rest) 
  | isPrefixOf "import (" line = 
      let (importBlock, remaining) = span (not . (== ")")) rest
      in drop 1 remaining  -- Drop the closing ")" as well
  | otherwise = line : removeImportBlock rest

-- Clean code blocks by removing duplicate package declarations and imports
cleanCodeBlocks :: String -> String
cleanCodeBlocks content = 
  let
    linesList = lines content
    -- Remove lines that start with "package main" 
    -- as these are already handled by the header and imports generation
    -- Remove the entire import block (import ( ... ))
    -- Also remove lines that are just import statements (like "fmt" or "sync")
    -- Also remove lines that are just quoted import statements (like "\\"fmt\\"")
    filteredLines = removeImportBlock $ filter (\\line -> 
        not (isPrefixOf "package main" line) && 
        not (isImportLine line)) linesList
  in
    unlines filteredLines"""

new_functions = """-- Remove block: lines between "start" and "end"
removeBlock :: String -> String -> [String] -> [String]
removeBlock start end [] = []
removeBlock start end (line:rest) 
  | isPrefixOf start line = 
      let (blockLines, remaining) = span (not . (== end)) rest
      in drop 1 remaining  -- Drop the closing line as well
  | otherwise = line : removeBlock start end rest

-- Remove import block: lines between "import (" and ")"
removeImportBlock :: [String] -> [String]
removeImportBlock = removeBlock "import (" ")"

-- Remove var block: lines between "var (" and ")"
removeVarBlock :: [String] -> [String]
removeVarBlock = removeBlock "var (" ")"

-- Remove const block: lines between "const (" and ")"
removeConstBlock :: [String] -> [String]
removeConstBlock = removeBlock "const (" ")"

-- Clean code blocks by removing duplicate package declarations and imports
cleanCodeBlocks :: String -> String
cleanCodeBlocks content = 
  let
    linesList = lines content
    -- Remove lines that start with "package main" 
    -- as these are already handled by the header and imports generation
    -- Remove the entire import block (import ( ... ))
    -- Remove the entire var block (var ( ... ))
    -- Remove the entire const block (const ( ... ))
    -- Also remove lines that are just import statements (like "fmt" or "sync")
    -- Also remove lines that are just quoted import statements (like "\\"fmt\\"")
    filteredLines = removeConstBlock $ removeVarBlock $ removeImportBlock $ filter (\\line -> 
        not (isPrefixOf "package main" line) && 
        not (isImportLine line)) linesList
  in
    unlines filteredLines"""

content = content.replace(old_functions, new_functions)

# Write back
with open('/home/qwe12345678/typus2/src/Compiler.hs', 'w') as f:
    f.write(content)

print("Fixed cleanCodeBlocks function to handle var and const blocks")