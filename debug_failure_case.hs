import Utils

main :: IO ()
main = do
    putStrLn "=== Testing failure case ==="
    
    -- 测试失败案例 s = "a"
    let s = "a"
    let withBackslash = "\"" ++ s ++ "\\\\"
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "Constructed string: " ++ show withBackslash
    putStrLn $ "String length: " ++ show (length withBackslash)
    putStrLn $ "String chars: " ++ show (map (\c -> (c, fromEnum c)) withBackslash)
    putStrLn $ "isCompleteStringLiteral result: " ++ show (Utils.isCompleteStringLiteral withBackslash)
    putStrLn $ "Expected: True"
    
    -- 检查其他测试案例
    putStrLn "\n=== Testing other cases ==="
    let testCases = ["", "\"", "a", "\\", "\"\\", "\n", "\t"]
    mapM_ (\s' -> do
        let withB = "\"" ++ s' ++ "\\\\"
        putStrLn $ "s=" ++ show s' ++ " -> " ++ show withB ++ " -> " ++ show (Utils.isCompleteStringLiteral withB)
      ) testCases