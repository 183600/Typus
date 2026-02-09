import Utils

main :: IO ()
main = do
    let testInput = "'\\"
    
    putStrLn $ "详细分析 isCompleteStringLiteral " ++ show testInput
    
    -- 手动实现新的 hasClosingQuote 来跟踪执行路径
    let result = hasClosingQuoteImpl testInput
    putStrLn $ "新的 hasClosingQuote 结果: " ++ show result
    putStrLn $ "isCompleteStringLiteral 结果: " ++ show (isCompleteStringLiteral testInput)
    
    -- 逐步跟踪
    putStrLn "\n逐步跟踪:"
    trackHasClosingQuote testInput 0
    
    -- 测试其他关键输入
    let testInputs = ["'", "\"", "'\\", "\"\\", "'\\\"", "\"\\\""]
    
    putStrLn "\n测试其他关键输入:"
    mapM_ testInput' testInputs
    
  where
    hasClosingQuoteImpl :: String -> Bool
    hasClosingQuoteImpl [] = False
    hasClosingQuoteImpl ['\\'] = False
    hasClosingQuoteImpl ('\\':'"':[]) = True
    hasClosingQuoteImpl ('\\':'\':[]) = True
    hasClosingQuoteImpl ('\\':x:xs) = hasClosingQuoteImpl xs
    hasClosingQuoteImpl ('"':[]) = True
    hasClosingQuoteImpl ('\':[]) = True
    hasClosingQuoteImpl ('"':'"':_) = True
    hasClosingQuoteImpl ('\':'\':_) = True
    hasClosingQuoteImpl ('"':_) = False
    hasClosingQuoteImpl ('\':_) = False
    hasClosingQuoteImpl (_:xs) = hasClosingQuoteImpl xs
    
    trackHasClosingQuote :: String -> Int -> IO ()
    trackHasClosingQuote [] depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote [] = False"
    trackHasClosingQuote ['\\'] depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ['\\\\'] = False"
    trackHasClosingQuote ('\\':'"':[]) depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\\\\':'\"':[]) = True"
    trackHasClosingQuote ('\\':'\':[]) depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\\\\':'\\':[]) = True"
    trackHasClosingQuote ('\\':x:xs) depth = do
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\\\\':" ++ show x ++ ":" ++ show xs ++ ") = hasClosingQuote " ++ show xs
        trackHasClosingQuote xs (depth + 2)
    trackHasClosingQuote ('"':[]) depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\"':[]) = True"
    trackHasClosingQuote ('\':[]) depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\\':[]) = True"
    trackHasClosingQuote ('"':'"':xs) depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\"':'\"':" ++ show xs ++ ") = True"
    trackHasClosingQuote ('\':'\':xs) depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\\':'\\':" ++ show xs ++ ") = True"
    trackHasClosingQuote ('"':xs) depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\"':" ++ show xs ++ ") = False"
    trackHasClosingQuote ('\':xs) depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\\':" ++ show xs ++ ") = False"
    trackHasClosingQuote (c:xs) depth = do
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote (" ++ show c ++ ":" ++ show xs ++ ") = hasClosingQuote " ++ show xs
        trackHasClosingQuote xs (depth + 2)
    
    testInput' :: String -> IO ()
    testInput' input = do
        let result = isCompleteStringLiteral input
        putStrLn $ "  " ++ show input ++ " -> " ++ show result