#!/usr/bin/env python3

# Test what content is being processed by generateImports
import subprocess

# Create a simple Haskell script to debug generateImports
haskell_code = '''
module Main where

import Data.List (intercalate)
import Data.Char (isSpace)

-- Simulate the generateImports function
generateImports content = 
  let
    hasFmt = "fmt." `isInfixOf` content || "fmt\n" `isInfixOf` content || "\\"fmt\\"" `isInfixOf` content
    hasMath = "math." `isInfixOf` content || "\\"math\\"" `isInfixOf` content
    hasTime = "time." `isInfixOf` content || "\\"time\\"" `isInfixOf` content
    hasOs = "os." `isInfixOf` content || "\\"os\\"" `isInfixOf` content
    hasIo = "io." `isInfixOf` content || "\\"io\\"" `isInfixOf` content
    hasStrings = "strings." `isInfixOf` content || "\\"strings\\"" `isInfixOf` content
    hasSync = "sync." `isInfixOf` content || "\\"sync\\"" `isInfixOf` content
    hasRuntime = "runtime." `isInfixOf` content || "\\"runtime\\"" `isInfixOf` content
    hasUnsafe = "unsafe." `isInfixOf` content || "\\"unsafe\\"" `isInfixOf` content
    imports = filter (not . null) [
        if hasFmt then "    \"fmt\"" else "",
        if hasMath then "    \"math\"" else "",
        if hasTime then "    \"time\"" else "",
        if hasOs then "    \"os\"" else "",
        if hasIo then "    \"io\"" else "",
        if hasStrings then "    \"strings\"" else "",
        if hasSync then "    \"sync\"" else "",
        if hasRuntime then "    \"runtime\"" else "",
        if hasUnsafe then "    \"unsafe\"" else ""
      ]
  in
    if null imports 
      then ""
      else "import (\\n" ++ intercalate "\\n" imports ++ "\\n)"

main :: IO ()
main = do
  -- Read the simple_test.typus file
  content <- readFile "examples/simple_test.typus"
  putStrLn "=== ORIGINAL CONTENT ==="
  putStrLn content
  putStrLn ""
  
  putStrLn "=== ANALYSIS ==="
  putStrLn $ "hasFmt: " ++ show ("fmt." `isInfixOf` content || "fmt\n" `isInfixOf` content || "\\"fmt\\"" `isInfixOf` content)
  putStrLn $ "hasRuntime: " ++ show ("runtime." `isInfixOf` content || "\\"runtime\\"" `isInfixOf` content)
  putStrLn ""
  
  putStrLn "=== GENERATED IMPORTS ==="
  putStrLn $ generateImports content
'''

# Write the Haskell code to a file
with open('/home/qwe12345678/typus2/examples/debug_generate_imports.hs', 'w') as f:
    f.write(haskell_code)

# Compile and run the Haskell code
print("Compiling and running debug script...")
subprocess.run(['cd', '/home/qwe12345678/typus2', '&&', 'ghc', 'examples/debug_generate_imports.hs', '-o', 'examples/debug_generate_imports'], shell=True)
result = subprocess.run(['cd', '/home/qwe12345678/typus2', '&&', './examples/debug_generate_imports'], shell=True, capture_output=True, text=True)

print("OUTPUT:")
print(result.stdout)
if result.stderr:
    print("ERROR:")
    print(result.stderr)