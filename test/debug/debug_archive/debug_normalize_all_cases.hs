import Utils as U

main :: IO ()
main = do
    -- 测试所有情况
    putStrLn "=== Testing [\"\"] ==="
    let lines1 = [""]
    let withMixed1 = map ("\t  " ++) lines1
    let normalized1 = U.normalizeIndentation (unlines withMixed1)
    putStrLn $ "lines': " ++ show lines1
    putStrLn $ "withMixed: " ++ show withMixed1
    putStrLn $ "unlines withMixed: " ++ show (unlines withMixed1)
    putStrLn $ "normalized: " ++ show normalized1
    putStrLn $ "Expected: " ++ show "    "
    putStrLn $ "Test result: " ++ show (normalized1 == "    ")
    putStrLn ""
    
    putStrLn "=== Testing [\"\\n\"] ==="
    let lines2 = ["\n"]
    let withMixed2 = map ("\t  " ++) lines2
    let normalized2 = U.normalizeIndentation (unlines withMixed2)
    putStrLn $ "lines': " ++ show lines2
    putStrLn $ "withMixed: " ++ show withMixed2
    putStrLn $ "unlines withMixed: " ++ show (unlines withMixed2)
    putStrLn $ "normalized: " ++ show normalized2
    putStrLn $ "Expected: " ++ show "\n"
    putStrLn $ "Test result: " ++ show (normalized2 == "\n")