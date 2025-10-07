import Data.Char (isSpace)
import Data.List (isPrefixOf)

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

-- Clean code blocks by removing duplicate package declarations and imports
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

main :: IO ()
main = do
    let testContent = "package main\n\nimport (\n\t\"fmt\"\n\t\"runtime\"\n)\n\nfunc main() {\n\tfmt.Println(\"Hello, world!\")\n\tfmt.Println(\"Runtime:\", runtime.GOOS)\n}"
    
    putStrLn "Original content:"
    putStrLn testContent
    putStrLn "\nCleaned content:"
    putStrLn $ cleanCodeBlocks testContent