import Utils
import Data.List (isPrefixOf)

main :: IO ()
main = do
  let s = ""
  let nested = unlines $ ["    func outer() {", "        func inner() {", "            " ++ s, "        }", "    }"]
  let normalized = Utils.normalizeIndentation nested
  let normLines = lines normalized
  let linesWith4Spaces = filter (isPrefixOf "    ") normLines
  
  putStrLn $ "s: " ++ show s
  putStrLn $ "nested: " ++ show nested
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "normLines: " ++ show normLines
  putStrLn $ "Total lines: " ++ show (length normLines)
  putStrLn $ "Lines with 4 spaces: " ++ show (length linesWith4Spaces)
  putStrLn $ "Test passes (fewer lines with 4 spaces): " ++ show (length linesWith4Spaces < length normLines)
  putStrLn $ "not null normalized: " ++ show (not (null normalized))