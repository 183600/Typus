import Utils
import Data.List (isPrefixOf)

main :: IO ()
main = do
  -- Test case 2: normalizeIndentation code block with empty string
  let test2 = unlines ["    if condition {", "        // do something", "        return " ++ "", "    }"]
      inputLines2 = lines test2
      nonEmptyLines2 = filter (not . null) inputLines2
      commonPrefix2 = findCommonPrefix nonEmptyLines2
      result2 = normalizeIndentation test2
      result2Lines = lines result2
      nonCommentLines2 = filter (not . isPrefixOf "//") result2Lines
  putStrLn $ "Test 2 - Input: " ++ show test2
  putStrLn $ "Test 2 - Input lines: " ++ show inputLines2
  putStrLn $ "Test 2 - Non-empty lines: " ++ show nonEmptyLines2
  putStrLn $ "Test 2 - Common prefix: " ++ show commonPrefix2
  putStrLn $ "Test 2 - Common prefix length: " ++ show (length commonPrefix2)
  putStrLn $ "Test 2 - Output: " ++ show result2
  putStrLn $ "Test 2 - Output lines: " ++ show result2Lines
  putStrLn $ "Test 2 - Non-comment lines: " ++ show nonCommentLines2
  putStrLn $ "Test 2 - Non-comment lines start with spaces: " ++ show (any (isPrefixOf "    ") nonCommentLines2)
  putStrLn ""
  
  -- Test case 3: normalizeIndentation nested with empty string
  let test3 = unlines ["    func outer() {", "        func inner() {", "            " ++ "", "        }", "    }"]
      inputLines3 = lines test3
      nonEmptyLines3 = filter (not . null) inputLines3
      commonPrefix3 = findCommonPrefix nonEmptyLines3
      result3 = normalizeIndentation test3
      result3Lines = lines result3
  putStrLn $ "Test 3 - Input: " ++ show test3
  putStrLn $ "Test 3 - Input lines: " ++ show inputLines3
  putStrLn $ "Test 3 - Non-empty lines: " ++ show nonEmptyLines3
  putStrLn $ "Test 3 - Common prefix: " ++ show commonPrefix3
  putStrLn $ "Test 3 - Common prefix length: " ++ show (length commonPrefix3)
  putStrLn $ "Test 3 - Output: " ++ show result3
  putStrLn $ "Test 3 - Output lines: " ++ show result3Lines
  putStrLn $ "Test 3 - Lines start with spaces: " ++ show (any (isPrefixOf "    ") result3Lines)
  putStrLn ""
  
  -- Test case 4: normalizeIndentation labels with empty string
  let test4 = unlines ["label1:", "    " ++ "", "label2:", "    " ++ ""]
      inputLines4 = lines test4
      nonEmptyLines4 = filter (not . null) inputLines4
      commonPrefix4 = findCommonPrefix nonEmptyLines4
      result4 = normalizeIndentation test4
      result4Lines = lines result4
  putStrLn $ "Test 4 - Input: " ++ show test4
  putStrLn $ "Test 4 - Input lines: " ++ show inputLines4
  putStrLn $ "Test 4 - Non-empty lines: " ++ show nonEmptyLines4
  putStrLn $ "Test 4 - Common prefix: " ++ show commonPrefix4
  putStrLn $ "Test 4 - Common prefix length: " ++ show (length commonPrefix4)
  putStrLn $ "Test 4 - Output: " ++ show result4
  putStrLn $ "Test 4 - Output lines: " ++ show result4Lines
  putStrLn $ "Test 4 - Lines start with spaces: " ++ show (any (isPrefixOf "    ") result4Lines)

-- | 找出所有字符串的公共前缀（只考虑前导空格和制表符）
findCommonPrefix :: [String] -> String
findCommonPrefix [] = ""
findCommonPrefix (first:rest) = 
  let -- 只考虑前导空白字符
      leadingWhitespace str = takeWhile isSpace str
      allLeading = map leadingWhitespace (first:rest)
      -- 对于注释行（包含//），调整其前导空白以匹配非注释行
      adjustedLeading line = 
        let ws = leadingWhitespace line
            restAfterWs = drop (length ws) line
        in if "//" `isPrefixOf` restAfterWs
           then take 4 ws  -- 注释行只考虑4个空格的公共前缀
           else ws
      allAdjusted = map adjustedLeading (first:rest)
      -- 找出最短的长度
      minLength = minimum (map length allAdjusted)
      -- 检查每个位置是否在所有字符串中都是相同的空白字符
      checkPrefix pos = 
        if pos >= minLength
          then False
          else let charAtPos = map (!! pos) allAdjusted
               in case charAtPos of
                    [] -> False
                    (firstChar:_) -> all (== firstChar) charAtPos && isSpace firstChar
      -- 找出公共前缀的长度
      commonLength = length $ takeWhile checkPrefix [0..]
  in take (minLength `min` commonLength) (leadingWhitespace first)

-- | 检查字符是否是空白字符
isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == '\v'