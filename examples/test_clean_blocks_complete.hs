import Data.Char (isSpace)
import Data.List (isPrefixOf, span)

-- Utility function to trim whitespace
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

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

main :: IO ()
main = do
    let content = "package main\n\nimport (\n\t\"fmt\"\n\t\"runtime\"\n)\n\nfunc main() {\n\tfmt.Println(\"Hello, world!\")\n\tfmt.Println(\"Runtime:\", runtime.GOOS)\n}"
    
    putStrLn "Original content:"
    putStrLn content
    putStrLn "\nCleaned content:"
    putStrLn $ cleanCodeBlocks content