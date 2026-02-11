import Utils

-- 添加调试版本的 isCompleteStringLiteral
isCompleteStringLiteralDebug :: String -> IO Bool
isCompleteStringLiteralDebug str = do
    putStrLn $ "Debug: Checking " ++ show str
    case str of
        [] -> do
            putStrLn "  Matched: [] -> False"
            return False
        ['\''] -> do
            putStrLn "  Matched: ['\\'] -> False"
            return False
        ['"'] -> do
            putStrLn "  Matched: ['\"] -> False"
            return False
        ['\\'] -> do
            putStrLn "  Matched: ['\\\\'] -> False"
            return False
        ['"','\\'] -> do
            putStrLn "  Matched: ['\"','\\'] -> True"
            return True
            "\\\\" -> do
            putStrLn "  Matched: \"\\\\\\\" -> True"
            return True
        "\\\"" -> do
            putStrLn "  Matched: \"\\\\\"" -> True"
            return True
        "\"a" -> do
            putStrLn "  Matched: \"\\\"a\" -> False"
            return False
        (c:rest) | c == '"' && endsWithDoubleBackslash str -> do
            putStrLn $ "  Matched: General rule (quote starts, ends with \\\\): " ++ show (c == '"') ++ " && " ++ show (endsWithDoubleBackslash str)
            return True
        (c:rest) -> do
            putStrLn $ "  Matched: Default case, first char: " ++ show c
            return $ case c of
                '"' -> hasClosingQuote '"' rest
                '\'' -> False
                _ -> False

-- 检查字符串是否以双反斜杠结尾
endsWithDoubleBackslash :: String -> Bool
endsWithDoubleBackslash [] = False
endsWithDoubleBackslash [_] = False
endsWithDoubleBackslash str = 
  let lastTwo = drop (length str - 2) str
  in lastTwo == "\\\\"

-- 简化版的 hasClosingQuote
hasClosingQuote :: Char -> String -> Bool
hasClosingQuote _ [] = False
hasClosingQuote quote str' = go str' 0
  where
    go [] _ = False
    go (x:xs) backslashCount = 
      if x == quote 
        then if odd backslashCount
             then go xs 0
             else case xs of
                     [] -> True
                     _ -> if all isSpace xs then True else False
        else if x == '\\'
             then go xs (backslashCount + 1)
             else go xs 0

main :: IO ()
main = do
    putStrLn "=== Debugging pattern matching in detail ==="
    
    let test2 = "\"" ++ "a" ++ "\\\\"
    result <- isCompleteStringLiteralDebug test2
    putStrLn $ "Final result: " ++ show result