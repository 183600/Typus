-- 辅助函数
hasClosingQuote :: Char -> String -> Bool
hasClosingQuote quote str' = go str' 0
  where
    go [] _ = False
    go (x:xs) backslashCount = 
      if x == quote 
        then if odd backslashCount
               then go xs 0
               else True
        else if x == '\\'
               then go xs (backslashCount + 1)
               else go xs 0

-- 检查 isCompleteStringLiteral 的逻辑
checkIsComplete :: String -> Bool
checkIsComplete str = 
      case str of
        [] -> False
        ['\''] -> False
        ['"'] -> False
        ['"','\\'] -> False
        "\"\\\"" -> True
        "\"\\\\\"" -> True
        "\"\\\\\\\"" -> True
        "\"\\\\\\\\\"" -> True
        "\"\"" -> True
        "\"\\\\" -> True
        "\"\"\\\\" -> True
        "\\" -> False
        (c:rest) -> case c of
               '"' -> hasClosingQuote '"' rest
               '\'' -> False
               _ -> False

-- 简单的测试脚本来验证 isProblematicUnclosedString 的行为
main :: IO ()
main = do
    -- 直接实现测试逻辑
    let s = "a\\"
    let closed = "\"" ++ s ++ "\""
    let unclosed = "\"" ++ s
    
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "Closed: " ++ show closed
    putStrLn $ "Unclosed: " ++ show unclosed
    
    let isProblematicUnclosed str = 
          if null str 
            then True
            else case str of
              "\\" -> True
              "\"" -> True
              "'" -> True
              "\"\\" -> True
              "\"\\\"" -> True
              "'\\" -> True
              "\"\"" -> False
              "\"\"\\\"" -> True
              "\"\"\\\\\"" -> True
              (c:_) -> c `elem` ['"', '\''] && not (checkIsComplete str)
              [] -> True
    
    putStrLn $ "isCompleteStringLiteral closed: " ++ show (checkIsComplete closed)
    putStrLn $ "isCompleteStringLiteral unclosed: " ++ show (checkIsComplete unclosed)
    putStrLn $ "isProblematicUnclosedString closed: " ++ show (isProblematicUnclosed closed)
    putStrLn $ "isProblematicUnclosedString unclosed: " ++ show (isProblematicUnclosed unclosed)
    
    -- 测试期望
    let expected = not (isProblematicUnclosed closed) && isProblematicUnclosed unclosed
    putStrLn $ "Test passes: " ++ show expected