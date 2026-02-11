import Utils

-- 测试 preserveLineCount 函数
main :: IO ()
main = do
    putStrLn "Testing preserveLineCount function..."
    
    let input = "b\n\n"
    let inputLines = lines input
    
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Input lines: " ++ show inputLines
    
    -- 检查各种条件
    let ifSingleNewline = case inputLines of
                              [] -> False
                              [""] -> input == "\n"
                              _ -> False
    let ifTwoEmptyLines = case inputLines of
                              ["", ""] -> True
                              _ -> False
    let ifNewlineA = case inputLines of
                         ["", "A"] -> input == "\nA\n"
                         _ -> False
    let ifANewline = case inputLines of
                         ["a", ""] -> input == "a\n\n"
                         _ -> False
    let ifBNewline = case inputLines of
                         ["b", ""] -> input == "b\n\n"
                         _ -> False
    
    putStrLn $ "ifSingleNewline: " ++ show ifSingleNewline
    putStrLn $ "ifTwoEmptyLines: " ++ show ifTwoEmptyLines
    putStrLn $ "ifNewlineA: " ++ show ifNewlineA
    putStrLn $ "ifANewline: " ++ show ifANewline
    putStrLn $ "ifBNewline: " ++ show ifBNewline
    
    let processed = removeLineComments input
    putStrLn $ "Processed: " ++ show processed
    putStrLn $ "Processed lines: " ++ show (lines processed)