import qualified Utils as U
import Data.List (isPrefixOf, isInfixOf)

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
  
  -- Check if it's detected as a code block
  let inputLines = lines codeBlock
      isCodeBlock = any (`isInfixOf` codeBlock) ["if condition", "func outer", "func inner", "return", "{", "}", "//"] || 
                    (any (`isPrefixOf` "    ") inputLines && any (`isInfixOf` "{") inputLines) ||
                    (any (`isPrefixOf` "        ") inputLines && any (`isInfixOf` "func") inputLines)
  putStrLn $ "isCodeBlock: " ++ show isCodeBlock