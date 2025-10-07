import Data.Char (isSpace)
import Data.List (isPrefixOf, isInfixOf, intercalate, filter)

-- Utility function to trim whitespace
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- Generate imports section
generateImports :: String -> String
generateImports content = 
  let
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
  | isPrefixOf "import (" line = dropWhile (not . (== ")")) rest
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

-- Simulate the full generation process
generateGoCode :: String -> String
generateGoCode content = 
  let
    -- Generate file header (only once)
    header = "package main\n"
    
    -- Generate imports (only once)
    imports = generateImports content
    
    -- This simulates the original content (blocks)
    blocks = content
    
    -- Remove duplicate package declarations and imports from code blocks
    cleanedBlocks = cleanCodeBlocks blocks
    
    -- Combine all parts
    allParts = filter (not . null) [header, imports, cleanedBlocks]
  in
    intercalate "\n" allParts ++ "\n"

main :: IO ()
main = do
    let content = "package main\n\nimport (\n\t\"fmt\"\n\t\"runtime\"\n)\n\nfunc main() {\n\tfmt.Println(\"Hello, world!\")\n\tfmt.Println(\"Runtime:\", runtime.GOOS)\n}"
    
    putStrLn "Original content:"
    putStrLn content
    putStrLn "\nGenerated Go code:"
    putStrLn $ generateGoCode content