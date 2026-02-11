import Utils

main :: IO ()
main = do
    let s = "a"
    let mixed = "  \t  " ++ s
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "Input mixed: " ++ show mixed
    putStrLn $ "Has tab: " ++ show ('\t' `elem` mixed)
    
    -- 测试 normalizeIndentation
    let normalized = normalizeIndentation mixed
    putStrLn $ "After normalizeIndentation: " ++ show normalized
    putStrLn $ "Has tab after normalizeIndentation: " ++ show ('\t' `elem` normalized)
    putStrLn $ "Test passes: " ++ show (not ('\t' `elem` normalized))