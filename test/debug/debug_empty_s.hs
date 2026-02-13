import Utils
import Data.List (isPrefixOf)

main :: IO ()
main = do
  let s = ""
  let codeBlock = unlines $ ["    if condition {", "        // do something", "        return " ++ s, "    }"]
  let normalized = Utils.normalizeIndentation codeBlock
  let normLines = lines normalized
  let nonCommentLines = filter (not . isPrefixOf "//") normLines
  let linesWith4Spaces = filter (isPrefixOf "    ") normLines
  
  putStrLn $ "s: " ++ show s
  putStrLn $ "codeBlock: " ++ show codeBlock
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "normLines: " ++ show normLines
  putStrLn $ "nonCommentLines: " ++ show nonCommentLines
  putStrLn $ "linesWith4Spaces: " ++ show linesWith4Spaces
  putStrLn $ "Total lines: " ++ show (length normLines)
  putStrLn $ "Lines with 4 spaces: " ++ show (length linesWith4Spaces)
  putStrLn $ "Test passes (fewer lines with 4 spaces): " ++ show (length linesWith4Spaces < length normLines)