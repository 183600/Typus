import Data.Char (isSpace)
import Data.List (isPrefixOf)

-- 模拟normalizeIndentation的多行处理
debugNormalizeIndentation :: String -> String
debugNormalizeIndentation input = 
  let inputLines = lines input
      -- 过滤掉空行来计算公共前缀
      nonEmptyLines = filter (not . null) inputLines
      -- 只考虑前导空白字符
      leadingWhitespace str = takeWhile isSpace str
      allLeading = map leadingWhitespace nonEmptyLines
      -- 找出最短的长度
      minLength = if null allLeading then 0 else minimum (map length allLeading)
      -- 检查每个位置是否在所有非空字符串中都是相同的空白字符
      checkPrefix pos = 
        if pos >= minLength || null allLeading
          then False
          else let charAtPos = map (!! pos) allLeading
               in case charAtPos of
                    [] -> False
                    (firstChar:_) -> all (== firstChar) charAtPos && isSpace firstChar
      -- 找出公共前缀的长度
      commonLength = length $ takeWhile checkPrefix [0..]
      commonPrefix = if null nonEmptyLines 
                     then "" 
                     else take (minLength `min` commonLength) (leadingWhitespace (head nonEmptyLines))
      -- 移除公共前缀
      removeCommonPrefix line = 
        if null line  -- 空行保持不变
          then line
          else if commonPrefix `isPrefixOf` line
               then drop (length commonPrefix) line
               else line
      processedLines = map removeCommonPrefix inputLines
  in unlines processedLines

main :: IO ()
main = do
    let codeBlock = "    if condition {\n        // do something\n        return \n    }"
    putStrLn "Input code block:"
    putStrLn $ show codeBlock
    putStrLn "\nInput lines:"
    mapM_ (putStrLn . show) $ lines codeBlock
    putStrLn "\nNon-empty lines:"
    mapM_ (putStrLn . show) $ filter (not . null) $ lines codeBlock
    putStrLn "\nLeading whitespace:"
    mapM_ (putStrLn . show) $ map (takeWhile isSpace) $ filter (not . null) $ lines codeBlock
    putStrLn "\nMin length:"
    putStrLn $ show $ minimum $ map length $ map (takeWhile isSpace) $ filter (not . null) $ lines codeBlock
    putStrLn "\nNormalized:"
    putStrLn $ debugNormalizeIndentation codeBlock
    putStrLn "\nNormalized lines:"
    mapM_ putStrLn $ lines $ debugNormalizeIndentation codeBlock