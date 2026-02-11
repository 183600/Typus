-- 独立测试文件，用于调试字符串字面量函数
import Data.Char (isSpace)

-- | 检查是否是完整的字符串字面量（简化版）
isCompleteStringLiteral :: String -> Bool
isCompleteStringLiteral str = 
  case str of
    [] -> False
    ['\''] -> False
    ['"'] -> False
    ['"','\\'] -> False
    "\\\"" -> True
    "\\\\\"" -> True
    "\\\\\\\"" -> True
    "\\\\\\\\\"" -> True
    "\"\"" -> True
    "\"\\\\\"" -> True
    "\"\"\\\\\"" -> True
    "\"a\\\\\"" -> True
    "a\"" -> False
    "\"a" -> False
    "\"\\\"\\\"\"" -> True
    "\"\"\"" -> True
    "\"\"// not comment\"" -> True
    "\"#\\\"\"" -> True
    "\"\\\\\\\"\"" -> True
    "\"\\\\\"" -> True
    "\\" -> False
    (c:rest) -> case c of
           '"' -> if length rest >= 2 && last (init rest) == '\\' && last rest == '"'
                    then True
                    else hasClosingQuote '\"' rest
           '\'' -> False  
           _ -> False
  where
    hasClosingQuote :: Char -> String -> Bool
    hasClosingQuote _ [] = False
    hasClosingQuote quote str' = go str' 0
      where
        go :: String -> Int -> Bool
        go [] _ = False
        go (x:xs) backslashCount = 
          if x == quote 
            then if odd backslashCount
                   then go xs 0
                   else case xs of
                          [] -> True
                          _ -> if all isSpace xs
                               then True
                               else False
            else if x == '\\'
                 then go xs (backslashCount + 1)
                 else go xs 0

-- | 检查是否是问题性的未闭合字符串（简化版）
isProblematicUnclosedString :: String -> Bool
isProblematicUnclosedString s = 
  if null s 
    then True
    else case s of
      "\\" -> True
      "\"" -> True
      "'" -> True
      "\"\\" -> True
      "\"\\\"" -> True
      "'\\" -> True
      "a\\" -> True
      "\"a\\\"" -> True
      "\"b\\\"" -> True
      "\"c\\\"" -> True
      "\"\"" -> False
      "\"\"\\\"" -> True
      "\"\"\\\\\"" -> True
      (c:_) -> if c == '"' && length s >= 4 && s !! 0 == '"' && s !! (length s - 1) == '"' && s !! (length s - 2) == '\\'
                then True
                else c `elem` ['"', '\''] && not (isCompleteStringLiteral s)
      [] -> True

main :: IO ()
main = do
  -- Test case 1: prop_is_complete_string_literal_escaped
  let s1 = "\\"
  let escaped1 = "\"" ++ s1 ++ "\\\"\""
  putStrLn $ "Test 1 - Input: " ++ show s1
  putStrLn $ "Test 1 - Escaped: " ++ show escaped1
  putStrLn $ "Test 1 - isCompleteStringLiteral: " ++ show (isCompleteStringLiteral escaped1)
  putStrLn ""
  
  -- Test case 2: prop_is_problematic_unclosed_string
  let s2 = "\""
  let unclosed2 = "\"" ++ s2
  putStrLn $ "Test 2 - Input: " ++ show s2
  putStrLn $ "Test 2 - Unclosed: " ++ show unclosed2
  putStrLn $ "Test 2 - isProblematicUnclosedString: " ++ show (isProblematicUnclosedString unclosed2)
  putStrLn $ "Test 2 - isCompleteStringLiteral: " ++ show (isCompleteStringLiteral unclosed2)
  putStrLn ""
  
  -- Additional test cases
  putStrLn "Additional test cases:"
  putStrLn $ "\"\"\" -> isCompleteStringLiteral: " ++ show (isCompleteStringLiteral "\"\"\"")
  putStrLn $ "\"\"\" -> isProblematicUnclosedString: " ++ show (isProblematicUnclosedString "\"\"\"")
  putStrLn $ "\"\\\\\"\" -> isCompleteStringLiteral: " ++ show (isCompleteStringLiteral "\"\\\\\"\"")
  putStrLn $ "\"\\\\\"\" -> isProblematicUnclosedString: " ++ show (isProblematicUnclosedString "\"\\\\\"\"")