import Utils
import Data.List (isPrefixOf)

main :: IO ()
main = do
  putStrLn "=== Testing normalizeIndentation nested ==="
  
  let s = ""
  let nested = unlines $ ["    func outer() {", "        func inner() {", "            " ++ s, "        }", "    }"]
  let normalized = normalizeIndentation nested
  let normLines = lines normalized
  
  putStrLn $ "s: " ++ show s
  putStrLn $ "nested: " ++ show nested
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "normLines: " ++ show normLines
  
  let testResult = if null s
                   then all (not . isPrefixOf "    ") normLines && not (null normalized)
                   else all (not . isPrefixOf "    ") normLines && not (null normalized)
  
  putStrLn $ "Test result: " ++ show testResult
  putStrLn $ "Expected: True"
  
  putStrLn "\n=== Checking each line for leading spaces ==="
  mapM_ (\line -> do
    let hasLeadingSpaces = isPrefixOf "    " line
    putStrLn $ show line ++ " has leading spaces: " ++ show hasLeadingSpaces
    ) normLines