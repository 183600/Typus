import Utils
import Data.List (isPrefixOf)

main :: IO ()
main = do
  -- Test case 2: normalizeIndentation code block with empty string
  let test2 = unlines ["    if condition {", "        // do something", "        return " ++ "", "    }"]
      inputLines2 = lines test2
      nonEmptyLines2 = filter (not . all isSpace) inputLines2
      commonPrefix2 = findCommonPrefix nonEmptyLines2
      result2 = normalizeIndentation test2
      result2Lines = lines result2
      nonCommentLines2 = filter (not . isPrefixOf "//") result2Lines
      
      -- 手动测试每行的处理
      manualLines = map (\line -> 
        if all isSpace line
          then ""
          else drop (length commonPrefix2) line
      ) inputLines2
      
  putStrLn $ "Test 2 - Input: " ++ show test2
  putStrLn $ "Test 2 - Input lines: " ++ show inputLines2
  putStrLn $ "Test 2 - Non-empty lines: " ++ show nonEmptyLines2
  putStrLn $ "Test 2 - Common prefix: " ++ show commonPrefix2
  putStrLn $ "Test 2 - Common prefix length: " ++ show (length commonPrefix2)
  putStrLn $ "Test 2 - Manual lines: " ++ show manualLines
  putStrLn $ "Test 2 - Output: " ++ show result2
  putStrLn $ "Test 2 - Output lines: " ++ show result2Lines
  putStrLn $ "Test 2 - Non-comment lines: " ++ show nonCommentLines2
  putStrLn $ "Test 2 - Non-comment lines start with spaces: " ++ show (any (isPrefixOf "    ") nonCommentLines2)
  
  -- 检查每一行的前缀
  putStrLn "\nLine-by-line analysis:"
  mapM_ (\(i, line) -> do
    putStrLn $ "Line " ++ show i ++ ": " ++ show line
    putStrLn $ "  Starts with 4 spaces: " ++ show (isPrefixOf "    " line)
    putStrLn $ "  Is comment: " ++ show (isPrefixOf "//" (dropWhile isSpace line))
  ) (zip [0..] result2Lines)

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