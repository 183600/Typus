import Utils

main :: IO ()
main = do
    let s = "a"
    let mixed = "\t  \t  " ++ s ++ "  \t  "
    let normalized = normalizeIndentation mixed
    putStrLn $ "Input: " ++ show s
    putStrLn $ "Mixed: " ++ show mixed
    putStrLn $ "Normalized: " ++ show normalized
    putStrLn $ "Expected: " ++ show mixed  -- 测试期望保持原样