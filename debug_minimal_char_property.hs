import Utils

-- 测试 minimal char property 失败的情况
main :: IO ()
main = do
    putStrLn "Testing minimal char property..."
    
    -- 根据测试失败信息，输入是 '\f'
    let c = '\f'
    let result = isValidChar c
    
    putStrLn $ "Input c: " ++ show c
    putStrLn $ "isValidChar result: " ++ show result
    putStrLn $ "Expected: True"
    putStrLn $ "Test passes: " ++ show result