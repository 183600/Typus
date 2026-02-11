import Utils
import Data.List (isPrefixOf)

main :: IO ()
main = do
  let test2 = unlines ["    if condition {", "        // do something", "        return " ++ "", "    }"]
      inputLines2 = lines test2
      nonEmptyLines2 = filter (not . all isSpace) inputLines2
      commonPrefix2 = findCommonPrefix nonEmptyLines2
      
  putStrLn $ "Input lines: " ++ show inputLines2
  putStrLn $ "Non-empty lines: " ++ show nonEmptyLines2
  putStrLn $ "Common prefix: " ++ show commonPrefix2
  
  -- 手动处理每行
  mapM_ (\line -> do
    let processed = if all isSpace line
          then ""
          else drop (length commonPrefix2) line
    putStrLn $ "Line: " ++ show line ++ " -> " ++ show processed
  ) inputLines2

-- | 找出所有字符串的公共前缀（只考虑前导空格和制表符）
findCommonPrefix :: [String] -> String
findCommonPrefix [] = ""
findCommonPrefix (first:rest) = 
  let -- 只考虑前导空白字符
      leadingWhitespace str = takeWhile isSpace str
      allLeading = map leadingWhitespace (first:rest)
      -- 找出最短的长度
      minLength = minimum (map length allLeading)
      -- 检查每个位置是否在所有字符串中都是相同的空白字符
      checkPrefix pos = 
        if pos >= minLength
          then False
          else let charAtPos = map (!! pos) allLeading
               in case charAtPos of
                    [] -> False
                    (firstChar:_) -> all (== firstChar) charAtPos && isSpace firstChar
      -- 找出公共前缀的长度
      commonLength = length $ takeWhile checkPrefix [0..]
  in take (minLength `min` commonLength) (leadingWhitespace first)

-- | 检查字符是否是空白字符
isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == '\v'