import Utils

-- 测试 preserveLineCount 函数
main :: IO ()
main = do
    putStrLn "Testing preserveLineCount function..."
    
    let input = "\t  a\n\n"
    let inputLines = lines input
    
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Input lines: " ++ show inputLines
    putStrLn $ "Input == \"\\t  a\\n\\t  \\n\\n\": " ++ show (input == "\t  a\n\t  \n\n")
    
    let processed = preserveLineCount input
    putStrLn $ "Processed: " ++ show processed