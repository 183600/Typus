import Utils as U

main :: IO ()
main = do
    -- 测试失败的情况
    let lines' = [""]
    let withMixed = map ("\t  " ++) lines'
    let normalized = U.normalizeIndentation (unlines withMixed)
    
    putStrLn $ "Testing lines': " ++ show lines'
    putStrLn $ "withMixed: " ++ show withMixed
    putStrLn $ "unlines withMixed: " ++ show (unlines withMixed)
    putStrLn $ "normalized: " ++ show normalized
    putStrLn $ "Expected: " ++ show "    "
    putStrLn $ "Test result: " ++ show (normalized == "    ")