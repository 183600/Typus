import Utils

main :: IO ()
main = do
    -- 详细分析 isCompleteStringLiteral "''a\" 的行为
    let input = "''a\\"
    
    putStrLn $ "详细分析 isCompleteStringLiteral " ++ show input
    
    -- 手动实现 hasClosingQuote 来跟踪执行路径
    let result = hasClosingQuoteImpl (tail input)  -- 跳过第一个引号
    putStrLn $ "hasClosingQuote 结果: " ++ show result
    putStrLn $ "isCompleteStringLiteral 结果: " ++ show (isCompleteStringLiteral input)
    
    -- 逐步跟踪
    putStrLn "\n逐步跟踪:"
    trackHasClosingQuote (tail input) 0
    
    -- 分析问题
    putStrLn "\n问题分析:"
    putStrLn $ "1. 输入 " ++ show input ++ " 以 ' 开头"
    putStrLn "2. 调用 hasClosingQuote \"'a\\\\\""
    putStrLn "3. hasClosingQuote \"'a\\\\\" 看到第一个字符是 '，匹配 ('\\'':_) 模式"
    putStrLn "4. 直接返回 True"
    putStrLn "5. 但测试期望返回 False"
    
    putStrLn "\n可能的解决方案:"
    putStrLn "1. 修改 hasClosingQuote 函数，使其更严格地检查引号后的内容"
    putStrLn "2. 或者修改 isCompleteStringLiteral 函数的逻辑"
    
  where
    hasClosingQuoteImpl :: String -> Bool
    hasClosingQuoteImpl [] = False
    hasClosingQuoteImpl ['\\'] = False
    hasClosingQuoteImpl ('\\':'"':[]) = True
    hasClosingQuoteImpl ('\\':'\'':[]) = True
    hasClosingQuoteImpl ('\\':x:xs) = hasClosingQuoteImpl xs
    hasClosingQuoteImpl ('"':[]) = True
    hasClosingQuoteImpl ('\'':[]) = True
    hasClosingQuoteImpl ('"':'"':_) = True
    hasClosingQuoteImpl ('\'':'\':_) = True
    hasClosingQuoteImpl ('"':'\\':_) = False
    hasClosingQuoteImpl ('\':'\\':_) = False
    hasClosingQuoteImpl ('"':_) = True
    hasClosingQuoteImpl ('\':_) = True
    hasClosingQuoteImpl (_:xs) = hasClosingQuoteImpl xs
    
    trackHasClosingQuote :: String -> Int -> IO ()
    trackHasClosingQuote [] depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote [] = False"
    trackHasClosingQuote ['\\'] depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ['\\\\'] = False"
    trackHasClosingQuote ('\\':'"':[]) depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\\\\':'\"':[]) = True"
    trackHasClosingQuote ('\\':'\'':[]) depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\\\\':'\\':[]) = True"
    trackHasClosingQuote ('\\':x:xs) depth = do
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\\\\':" ++ show x ++ ":" ++ show xs ++ ") = hasClosingQuote " ++ show xs
        trackHasClosingQuote xs (depth + 2)
    trackHasClosingQuote ('"':[]) depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\"':[]) = True"
    trackHasClosingQuote ('\'':[]) depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\\':[]) = True"
    trackHasClosingQuote ('"':'"':xs) depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\"':'\":" ++ show xs ++ ") = True"
    trackHasClosingQuote ('\'':'\':xs) depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\\':'\\:" ++ show xs ++ ") = True"
    trackHasClosingQuote ('"':'\\':xs) depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\"':'\\\\:" ++ show xs ++ ") = False"
    trackHasClosingQuote ('\':'\\':xs) depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\\':'\\\\:" ++ show xs ++ ") = False"
    trackHasClosingQuote ('"':xs) depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\":" ++ show xs ++ ") = True"
    trackHasClosingQuote ('\':xs) depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\\:" ++ show xs ++ ") = True"
    trackHasClosingQuote (c:xs) depth = do
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote (" ++ show c ++ ":" ++ show xs ++ ") = hasClosingQuote " ++ show xs
        trackHasClosingQuote xs (depth + 2)