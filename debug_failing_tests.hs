import Utils

main :: IO ()
main = do
    let testInputs = [
            ("'", "IsCompleteStringLiteral valid 测试"),
            ("\"", "IsCompleteStringLiteral escaped quotes 测试"),
            ("\"", "IsProblematicUnclosedString 测试")
            ]
    
    putStrLn "测试失败的输入:"
    mapM_ testInput testInputs
    
    putStrLn "\n分析问题:"
    putStrLn "1. IsCompleteStringLiteral valid 期望 isCompleteStringLiteral \"'\" = True"
    putStrLn "2. IsCompleteStringLiteral escaped quotes 期望 isCompleteStringLiteral \"\\\"\" = True"
    putStrLn "3. IsProblematicUnclosedString 期望 isProblematicUnclosedString \"\\\"\" = True"
    
    putStrLn "\n当前实际结果:"
    putStrLn $ "isCompleteStringLiteral \"'\" = " ++ show (isCompleteStringLiteral "'")
    putStrLn $ "isCompleteStringLiteral \"\\\"\" = " ++ show (isCompleteStringLiteral "\"")
    putStrLn $ "isProblematicUnclosedString \"\\\"\" = " ++ show (isProblematicUnclosedString "\"")
    
  where
    testInput :: (String, String) -> IO ()
    testInput (input, description) = do
        let result1 = isCompleteStringLiteral input
        let result2 = isProblematicUnclosedString input
        putStrLn $ "\n" ++ description ++ ":"
        putStrLn $ "  输入: " ++ show input
        putStrLn $ "  isCompleteStringLiteral: " ++ show result1
        putStrLn $ "  isProblematicUnclosedString: " ++ show result2