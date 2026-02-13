-- This script tests a more complex case to see if we can reproduce the issue

import Data.Char (isSpace)
import Data.List (isPrefixOf, isInfixOf, intercalate, filter, span)

-- Simulate CodeBlock data type
data CodeBlock = CodeBlock {
    cbContent :: String
  } deriving Show

-- Simulate TypusFile data type
data TypusFile = TypusFile {
    tfDirectives :: (),
    tfBlocks :: [CodeBlock]
  } deriving Show

-- Utility function to trim whitespace
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- Generate imports section
generateImports :: TypusFile -> String
generateImports typusFile = 
  let
    content = intercalate "\n" $ map cbContent (tfBlocks typusFile)
    hasFmt = "fmt." `isInfixOf` content || "fmt\n" `isInfixOf` content || "\"fmt\"" `isInfixOf` content
    hasMath = "math." `isInfixOf` content || "\"math\"" `isInfixOf` content
    hasTime = "time." `isInfixOf` content || "\"time\"" `isInfixOf` content
    hasOs = "os." `isInfixOf` content || "\"os\"" `isInfixOf` content
    hasIo = "io." `isInfixOf` content || "\"io\"" `isInfixOf` content
    hasStrings = "strings." `isInfixOf` content || "\"strings\"" `isInfixOf` content
    hasSync = "sync." `isInfixOf` content || "\"sync\"" `isInfixOf` content
    hasRuntime = "runtime." `isInfixOf` content || "\"runtime\"" `isInfixOf` content
    hasUnsafe = "unsafe." `isInfixOf` content || "\"unsafe\"" `isInfixOf` content
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
      else "import (\n" ++ intercalate "\n" imports ++ "\n)"

-- Check if a line is just an import statement (like "fmt" or "sync")
isImportLine :: String -> Bool
isImportLine line = 
  let trimmed = trim line
  in (trimmed == "fmt" || trimmed == "sync" || trimmed == "time" || 
      trimmed == "unsafe" || trimmed == "os" || trimmed == "io" || 
      trimmed == "strings" || trimmed == "math" || trimmed == "runtime" ||
      trimmed == "\"fmt\"" || trimmed == "\"sync\"" || trimmed == "\"time\"" || 
      trimmed == "\"unsafe\"" || trimmed == "\"os\"" || trimmed == "\"io\"" || 
      trimmed == "\"strings\"" || trimmed == "\"math\"" || trimmed == "\"runtime\"")

-- Remove import block: lines between "import (" and ")"
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
    -- Also remove lines that are just quoted import statements (like "\"fmt\"")
    filteredLines = removeImportBlock $ filter (\line -> 
        not (isPrefixOf "package main" line) && 
        not (isImportLine line)) linesList
  in
    unlines filteredLines

-- Transform code blocks with all necessary fixes
transformCodeBlocks :: [CodeBlock] -> String
transformCodeBlocks blocks = 
  let
    blockContents = map cbContent blocks
    -- Filter out EOF lines
    filteredContents = filter (not . null) blockContents
    -- Apply all transformations
    transformedContents = map transformCode filteredContents
  in
    intercalate "\n" transformedContents

-- Transform code to fix syntax issues
transformCode :: String -> String
transformCode content = 
  let
    linesContent = lines content
    transformedLines = map transformLine linesContent
  in
    unlines transformedLines

-- Transform a line to fix syntax issues
transformLine :: String -> String
transformLine line = line

-- Simulate the full generation process
generateGoCode :: TypusFile -> String
generateGoCode typusFile = 
  let
    -- Generate file header (only once)
    header = "package main\n"
    
    -- Generate imports (only once)
    imports = generateImports typusFile
    
    -- Generate code blocks with all necessary transformations
    blocks = transformCodeBlocks (tfBlocks typusFile)
    
    -- Remove duplicate package declarations and imports from code blocks
    cleanedBlocks = cleanCodeBlocks blocks
    
    -- Combine all parts
    allParts = filter (not . null) [header, imports, cleanedBlocks]
  in
    intercalate "\n" allParts ++ "\n"

main :: IO ()
main = do
    -- Test with a more complex case: multiple code blocks
    let typusFile = TypusFile {
            tfDirectives = (),
            tfBlocks = [
                CodeBlock {
                    cbContent = "package main\n\nimport (\n\t\"fmt\"\n\t\"runtime\"\n)\n"
                },
                CodeBlock {
                    cbContent = "func main() {\n\tfmt.Println(\"Hello, world!\")\n\tfmt.Println(\"Runtime:\", runtime.GOOS)\n}"
                }
            ]
        }
    
    putStrLn "Testing with multiple code blocks..."
    
    let imports = generateImports typusFile
    putStrLn "\nGenerated imports:"
    putStrLn imports
    
    let blocks = transformCodeBlocks (tfBlocks typusFile)
    putStrLn "\nTransformed blocks:"
    putStrLn blocks
    
    let cleanedBlocks = cleanCodeBlocks blocks
    putStrLn "\nCleaned blocks:"
    putStrLn cleanedBlocks
    
    let goCode = generateGoCode typusFile
    putStrLn "\nFinal Go code:"
    putStrLn goCode