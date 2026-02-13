#!/usr/bin/env python3

# Read the Compiler.hs file
with open('/home/qwe12345678/typus2/src/Compiler.hs', 'r') as f:
    content = f.read()

# Find and replace the cleanCodeBlocks function
old_function = '''-- Clean code blocks by removing duplicate package declarations and imports
cleanCodeBlocks :: String -> String
cleanCodeBlocks content = 
  let
    linesList = lines content
    -- Remove lines that start with "package main" or "import" 
    -- as these are already handled by the header and imports generation
    -- Also remove lines that are just import statements (like "fmt" or "sync")
    -- Also remove lines that are just quoted import statements (like "\"fmt\"")
    filteredLines = filter (\line -> 
        not (isPrefixOf "package main" line) && 
        not (isPrefixOf "import" line) &&
        not (isImportLine line)) linesList
  in
    unlines filteredLines
  where
    -- Check if a line is just an import statement (like "fmt" or "sync")
    isImportLine line = 
      let trimmed = trim line
      in (trimmed == "fmt" || trimmed == "sync" || trimmed == "time" || 
          trimmed == "unsafe" || trimmed == "os" || trimmed == "io" || 
          trimmed == "strings" || trimmed == "math" || trimmed == "runtime" ||
          trimmed == "\"fmt\"" || trimmed == "\"sync\"" || trimmed == "\"time\"" || 
          trimmed == "\"unsafe\"" || trimmed == "\"os\"" || trimmed == "\"io\"" || 
          trimmed == "\"strings\"" || trimmed == "\"math\"" || trimmed == "\"runtime\"")'''

new_function = '''-- Clean code blocks by removing duplicate package declarations and imports
cleanCodeBlocks :: String -> String
cleanCodeBlocks content = 
  let
    linesList = lines content
    -- Remove package declarations and import blocks
    filteredLines = removeImportBlock $ filter (\line -> 
        not (isPrefixOf "package main" line) && 
        not (isImportLine line)) linesList
  in
    unlines filteredLines
  where
    -- Check if a line is just an import statement (like "fmt" or "sync")
    isImportLine line = 
      let trimmed = trim line
      in (trimmed == "fmt" || trimmed == "sync" || trimmed == "time" || 
          trimmed == "unsafe" || trimmed == "os" || trimmed == "io" || 
          trimmed == "strings" || trimmed == "math" || trimmed == "runtime" ||
          trimmed == "\"fmt\"" || trimmed == "\"sync\"" || trimmed == "\"time\"" || 
          trimmed == "\"unsafe\"" || trimmed == "\"os\"" || trimmed == "\"io\"" || 
          trimmed == "\"strings\"" || trimmed == "\"math\"" || trimmed == "\"runtime\"")
    
    -- Remove entire import blocks including the opening "import (" and closing ")"
    removeImportBlock :: [String] -> [String]
    removeImportBlock [] = []
    removeImportBlock (line:rest) 
      | isPrefixOf "import (" line = 
          let (importBlock, remaining) = span (not . (== ")")) rest
          in drop 1 remaining  -- Drop the closing ")" as well
      | otherwise = line : removeImportBlock rest'''

content = content.replace(old_function, new_function)

# Write the file back
with open('/home/qwe12345678/typus2/src/Compiler.hs', 'w') as f:
    f.write(content)

print("Fixed cleanCodeBlocks function to properly handle import blocks")