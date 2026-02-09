import Utils

main :: IO ()
main = do
    let test1 = "\"\\"
    let test2 = "'\\"
    
    putStrLn $ "测试 " ++ show test1 ++ ":"
    putStrLn $ "  isCompleteStringLiteral result: " ++ show (isCompleteStringLiteral test1)
    putStrLn $ "  期望: False (根据测试)"
    
    putStrLn $ "\n测试 " ++ show test2 ++ ":"
    putStrLn $ "  isCompleteStringLiteral result: " ++ show (isCompleteStringLiteral test2)
    putStrLn $ "  期望: False (根据测试)"
    
    -- 分析
    putStrLn "\n分析 isCompleteStringLiteral \"\\\\\"\\\\\":" 
    case test1 of
        [] -> putStrLn "空字符串"
        ['\''] -> putStrLn "匹配 ['\\\']"
        ['"] -> putStrLn "匹配 [\\"\\\"]"
        ['\\'] -> putStrLn "匹配 ['\\\\']"
        (c:rest) -> do
            putStrLn $ "首字符: " ++ show c
            case c of
                '"' -> putStrLn $ "双引号，hasClosingQuote: " ++ show (hasClosingQuote rest)
                '\'' -> putStrLn $ "单引号，hasClosingQuote: " ++ show (hasClosingQuote rest)
                _ -> putStrLn "其他字符"
    
    putStrLn "\n分析 isCompleteStringLiteral \"'\\\\\":" 
    case test2 of
        [] -> putStrLn "空字符串"
        ['\''] -> putStrLn "匹配 ['\\\']"
        ['"] -> putStrLn "匹配 [\\"\\\"]"
        ['\\'] -> putStrLn "匹配 ['\\\\']"
        (c:rest) -> do
            putStrLn $ "首字符: " ++ show c
            case c of
                '"' -> putStrLn $ "双引号，hasClosingQuote: " ++ show (hasClosingQuote rest)
                '\'' -> putStrLn $ "单引号，hasClosingQuote: " ++ show (hasClosingQuote rest)
                _ -> putStrLn "其他字符"
  where
    hasClosingQuote :: String -> Bool
    hasClosingQuote [] = False
    hasClosingQuote ['\\'] = False
    hasClosingQuote ('\\':'\"':[]) = True
    hasClosingQuote ('\\':'\'':[]) = True
    hasClosingQuote ('\\':x:xs) = hasClosingQuote xs
    hasClosingQuote ('\"':_) = True
    hasClosingQuote ('\'':_) = True
    hasClosingQuote (_:xs) = hasClosingQuote xs