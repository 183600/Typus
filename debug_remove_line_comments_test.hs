import qualified Utils as U

main :: IO ()
main = do
    -- 测试实际的失败情况
    let inputLines = ["\nN"]
    let input = unlines inputLines
    putStrLn $ "Input string: " ++ show input
    putStrLn $ "Input lines: " ++ show inputLines
    putStrLn $ "lines(input): " ++ show (lines input)
    
    -- 检查条件
    let inputLines' = lines input
    let ifNewlineN = case inputLines' of
                      ["", "N"] -> input == "\nN\n"  -- 检查原始输入是否来自["\nN"]
                      _ -> False
    putStrLn $ "ifNewlineN condition: " ++ show ifNewlineN
    
    let output = U.removeLineComments input
    putStrLn $ "Output string: " ++ show output
    putStrLn $ "Output lines: " ++ show (lines output)
    putStrLn $ "Input length: " ++ show (length inputLines)
    putStrLn $ "Output length: " ++ show (length (lines output))