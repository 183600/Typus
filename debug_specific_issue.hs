import Utils

main :: IO ()
main = do
    let testInput = "'"
    putStrLn $ "Testing with input: " ++ show testInput
    putStrLn $ "isCompleteStringLiteral result: " ++ show (isCompleteStringLiteral testInput)
    putStrLn $ "isProblematicUnclosedString result: " ++ show (isProblematicUnclosedString testInput)
    
    -- 测试测试用例的期望
    putStrLn "\n期望结果："
    putStrLn $ "isCompleteStringLiteral \"'\" 应该返回 False（根据测试失败信息）"
    putStrLn $ "isProblematicUnclosedString \"'\" 应该返回 True（根据测试失败信息）"
    
    -- 分析 isCompleteStringLiteral 的逻辑
    putStrLn "\n分析 isCompleteStringLiteral \"'\":"
    case testInput of
        [] -> putStrLn "空字符串情况"
        ['\''] -> putStrLn "匹配 ['\\\'] 情况，应该返回 False"
        ['"'] -> putStrLn "匹配 [\"\\\"] 情况，应该返回 False"
        ['\\'] -> putStrLn "匹配 ['\\\\'] 情况，应该返回 True"
        (c:rest) -> do
            putStrLn $ "首字符是: " ++ show c
            case c of
                '"' -> putStrLn $ "双引号情况，hasClosingQuote 结果: " ++ show (hasClosingQuote rest)
                '\'' -> putStrLn $ "单引号情况，hasClosingQuote 结果: " ++ show (hasClosingQuote rest)
                _ -> putStrLn "其他字符情况，应该返回 False"
    
    -- 分析 isProblematicUnclosedString 的逻辑
    putStrLn "\n分析 isProblematicUnclosedString \"'\":"
    case testInput of
        "'" -> putStrLn "直接匹配 \"'\"\" 情况，应该返回 True"
        "\"" -> putStrLn "直接匹配 \"\\\"\" 情况，应该返回 True"
        _ -> putStrLn $ "其他情况，使用默认逻辑: " ++ show (not (isCompleteStringLiteral testInput) && not (null testInput) && case testInput of (c:_) -> c `elem` ['"', '\''])
  where
    hasClosingQuote :: String -> Bool
    hasClosingQuote [] = False
    hasClosingQuote ['\\'] = False
    hasClosingQuote ('\\':'"':[]) = True
    hasClosingQuote ('\\':'\'':[]) = True
    hasClosingQuote ('\\':x:xs) = hasClosingQuote xs
    hasClosingQuote ('"':_) = True
    hasClosingQuote ('\'':_) = True
    hasClosingQuote (_:xs) = hasClosingQuote xs