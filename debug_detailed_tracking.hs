import Utils

main :: IO ()
main = do
    let testInput = "'\\"
    
    putStrLn $ "详细分析 isCompleteStringLiteral " ++ show testInput
    
    -- 手动实现 hasClosingQuote 来跟踪执行路径
    let result = hasClosingQuoteImpl testInput
    putStrLn $ "hasClosingQuote 结果: " ++ show result
    putStrLn $ "isCompleteStringLiteral 结果: " ++ show (isCompleteStringLiteral testInput)
    
    -- 逐步跟踪
    putStrLn "\n逐步跟踪:"
    trackHasClosingQuote testInput 0
    
  where
    hasClosingQuoteImpl :: String -> Bool
    hasClosingQuoteImpl [] = False
    hasClosingQuoteImpl ['\\'] = False
    hasClosingQuoteImpl ('\\':'"':[]) = True
    hasClosingQuoteImpl ('\\':'\'':[]) = True
    hasClosingQuoteImpl ('\\':x:xs) = hasClosingQuoteImpl xs
    hasClosingQuoteImpl ('"':_) = True
    hasClosingQuoteImpl ('\'':_) = True
    hasClosingQuoteImpl (_:xs) = hasClosingQuoteImpl xs
    
    trackHasClosingQuote :: String -> Int -> IO ()
    trackHasClosingQuote [] depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote [] = False"
    trackHasClosingQuote ['\\'] depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ['\\\\'] = False"
    trackHasClosingQuote ('\\':'"':[]) depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\\\\':'\"':[]) = True"
    trackHasClosingQuote ('\\':'\'':[]) depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\\\\':'\'':[]) = True"
    trackHasClosingQuote ('\\':x:xs) depth = do
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\\\\':" ++ show x ++ ":" ++ show xs ++ ") = hasClosingQuote " ++ show xs
        trackHasClosingQuote xs (depth + 2)
    trackHasClosingQuote ('"':_) depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\"':_) = True"
    trackHasClosingQuote ('\'':_) depth = 
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote ('\'':_) = True"
    trackHasClosingQuote (c:xs) depth = do
        putStrLn $ replicate depth ' ' ++ "hasClosingQuote (" ++ show c ++ ":" ++ show xs ++ ") = hasClosingQuote " ++ show xs
        trackHasClosingQuote xs (depth + 2)