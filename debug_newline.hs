import Utils

main :: IO ()
main = do
    let lines' = [""]
    let withMixed = map ("\t  " ++) lines'
    let input = unlines withMixed
    putStrLn $ "lines': " ++ show lines'
    putStrLn $ "withMixed: " ++ show withMixed
    putStrLn $ "input (unlines withMixed): " ++ show input
    
    let normalized = normalizeIndentation input
    putStrLn $ "normalized: " ++ show normalized
    
    -- 检查是否需要添加换行符
    putStrLn $ "Should end with newline: " ++ show (not (null input) && last input == '\n')