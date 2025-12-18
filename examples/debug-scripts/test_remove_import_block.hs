import Data.List (isPrefixOf)

removeImportBlock :: [String] -> [String]
removeImportBlock [] = []
removeImportBlock (line:rest) 
  | isPrefixOf "import (" line = dropWhile (not . (== ")")) rest
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