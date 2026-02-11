import Utils
import Data.List (isPrefixOf)

main :: IO ()
main = do
  putStrLn "=== Testing normalizeIndentation code block ==="
  
  let s = ""
  let codeBlock = unlines $ ["    if condition {", "        // do something", "        return " ++ s, "    }"]
  let normalized = normalizeIndentation codeBlock
  let normLines = lines normalized
  let nonCommentLines = filter (not . isPrefixOf "//") normLines
  
  putStrLn $ "s: " ++ show s
  putStrLn $ "codeBlock: " ++ show codeBlock
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "normLines: " ++ show normLines
  putStrLn $ "nonCommentLines: " ++ show nonCommentLines
  
  let testResult = if null s
                   then all (not . isPrefixOf "    ") nonCommentLines
                   else all (not . isPrefixOf "    ") nonCommentLines && not (null normalized)
  
  putStrLn $ "Test result: " ++ show testResult
  putStrLn $ "Expected: True"
  
  putStrLn "\n=== Checking each line ==="
  mapM_ (\line -> do
    let hasLeadingSpaces = isPrefixOf "    " line
    putStrLn $ show line ++ " has leading spaces: " ++ show hasLeadingSpaces
    ) nonCommentLines