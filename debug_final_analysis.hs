import Utils

main :: IO ()
main = do
    -- 测试 prop_is_complete_string_literal_invalid 的期望
    putStrLn "测试 prop_is_complete_string_literal_invalid 的期望:"
    
    let test1 = isCompleteStringLiteral "\"\\"
    let test2 = isCompleteStringLiteral "'\\"
    
    putStrLn $ "isCompleteStringLiteral \"\\\\\" = " ++ show test1 ++ " (期望 False)"
    putStrLn $ "isCompleteStringLiteral \"'\\\\\" = " ++ show test2 ++ " (期望 False)"
    putStrLn $ "not test1 && not test2 = " ++ show (not test1 && not test2)
    
    -- 测试 QuickCheck 实际生成的输入
    putStrLn "\n测试 QuickCheck 实际生成的输入:"
    let quickCheckInput = "'"
    let result = isCompleteStringLiteral quickCheckInput
    putStrLn $ "isCompleteStringLiteral \"'\" = " ++ show result ++ " (测试期望 False，但实际是 " ++ show result ++ ")"
    
    -- 分析问题
    putStrLn "\n问题分析:"
    putStrLn "1. 测试失败信息显示 QuickCheck 生成了输入 \"'\""
    putStrLn "2. 但 prop_is_complete_string_literal_invalid 测试的应该是 \"'\\\\\""
    putStrLn "3. 这表明 QuickCheck 可能直接测试了 isCompleteStringLiteral 函数，而不是通过测试用例"
    
    -- 测试 isProblematicUnclosedString
    putStrLn "\n测试 isProblematicUnclosedString:"
    let problematicInput = "\""
    let problematicResult = isProblematicUnclosedString problematicInput
    putStrLn $ "isProblematicUnclosedString \"\\\"\" = " ++ show problematicResult ++ " (测试期望 True)"