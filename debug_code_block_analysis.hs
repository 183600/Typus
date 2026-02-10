import Utils
import Data.List (isPrefixOf)

main :: IO ()
main = do
  let s = ""
      codeBlock = unlines $ ["    if condition {", "        // do something", "        return " ++ s, "    }"]
      normalized = normalizeIndentation codeBlock
      normLines = lines normalized
      -- 检查非注释行是否没有前导空格
      nonCommentLines = filter (not . isPrefixOf "//") normLines
  
  putStrLn $ "s: " ++ show s
  putStrLn $ "codeBlock: " ++ show codeBlock
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "normLines: " ++ show normLines
  putStrLn $ "nonCommentLines: " ++ show nonCommentLines
  putStrLn $ "all (not . isPrefixOf "    ") nonCommentLines: " ++ show (all (not . isPrefixOf "    ") nonCommentLines)
  
  -- 分析每一行
  putStrLn "\nLine-by-line analysis:"
  mapM_ (\line -> do
    putStrLn $ "Line: " ++ show line
    putStrLn $ "  Starts with 4 spaces: " ++ show (isPrefixOf "    " line)
    putStrLn $ "  Is comment: " ++ show (isPrefixOf "//" (dropWhile isSpace line))
  ) normLines