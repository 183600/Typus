import Data.List (isPrefixOf, span)

-- Remove import block: lines between "import (" and ")"
removeImportBlock :: [String] -> [String]
removeImportBlock [] = []
removeImportBlock (line:rest) 
  | isPrefixOf "import (" line = 
      let (importBlock, remaining) = span (not . (== ")")) rest
      in drop 1 remaining  -- Drop the closing ")" as well
  | otherwise = line : removeImportBlock rest

main :: IO ()
main = do
    let linesList = [
            "package main",
            "",
            "import (",
            "\t\"fmt\"",
            "\t\"runtime\"",
            ")",
            "",
            "func main() {",
            "\tfmt.Println(\"Hello, world!\")",
            "\tfmt.Println(\"Runtime:\", runtime.GOOS)",
            "}"
          ]
    
    putStrLn "Original lines:"
    mapM_ putStrLn linesList
    
    let filteredLines = removeImportBlock linesList
    
    putStrLn "\nAfter removeImportBlock:"
    mapM_ putStrLn filteredLines