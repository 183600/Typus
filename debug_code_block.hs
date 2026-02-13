import qualified Utils as U
import Data.List (isPrefixOf)

main :: IO ()
main = do
  -- Test case for normalizeIndentation code block with ""
  let s = ""
      codeBlock = unlines $ ["    if condition {", "        // do something", "        return " ++ s, "    }"]
      normalized = U.normalizeIndentation codeBlock
      normLines = lines normalized
      nonCommentLines = filter (not . isPrefixOf "//") normLines
  putStrLn $ "s: " ++ show s
  putStrLn $ "codeBlock: " ++ show codeBlock
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "normLines: " ++ show normLines
  putStrLn $ "nonCommentLines: " ++ show nonCommentLines
  putStrLn $ "Lines with 4 spaces prefix: " ++ show (length (filter (isPrefixOf "    ") normLines))
  putStrLn $ "Total lines: " ++ show (length normLines)
  putStrLn $ "Test passes: " ++ show (length (filter (isPrefixOf "    ") normLines) < length normLines)
  
  -- Test case for normalizeIndentation nested with ""
  let s2 = ""
      nested = unlines $ ["    func outer() {", "        func inner() {", "            " ++ s2, "        }", "    }"]
      normalized2 = U.normalizeIndentation nested
      normLines2 = lines normalized2
  putStrLn $ "\ns2: " ++ show s2
  putStrLn $ "nested: " ++ show nested
  putStrLn $ "normalized2: " ++ show normalized2
  putStrLn $ "normLines2: " ++ show normLines2
  putStrLn $ "Lines with 4 spaces prefix: " ++ show (length (filter (isPrefixOf "    ") normLines2))
  putStrLn $ "Total lines: " ++ show (length normLines2)
  putStrLn $ "Test passes: " ++ show (length (filter (isPrefixOf "    ") normLines2) < length normLines2 && not (null normalized2))