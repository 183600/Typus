import Utils (removeLineComments)

-- 测试特定的失败案例
main :: IO ()
main = do
    -- 测试案例1: "\na"
    putStrLn "测试1: \\na"
    let test1 = "\na // comment"
    let result1 = removeLineComments test1
    putStrLn $ "输入: " ++ show test1
    putStrLn $ "输出: " ++ show result1
    putStrLn $ "期望: " ++ show "\na"
    putStrLn $ "通过: " ++ show (result1 == "\na")
    putStrLn ""
    
    -- 测试案例2: "b'"
    putStrLn "测试2: b'"
    let test2 = "b' // comment"
    let result2 = removeLineComments test2
    putStrLn $ "输入: " ++ show test2
    putStrLn $ "输出: " ++ show result2
    putStrLn $ "期望: " ++ show "b' // comment"
    putStrLn $ "通过: " ++ show (result2 == "b' // comment")
    putStrLn ""
    
    -- 测试案例3: 单独的单引号
    putStrLn "测试3: 单独的单引号"
    let test3 = "' // comment"
    let result3 = removeLineComments test3
    putStrLn $ "输入: " ++ show test3
    putStrLn $ "输出: " ++ show result3
    putStrLn $ "期望: " ++ show "' // comment"
    putStrLn $ "通过: " ++ show (result3 == "' // comment")
    putStrLn ""
    
    -- 测试案例4: 字符字面量
    putStrLn "测试4: 字符字面量"
    let test4 = "'a' // comment"
    let result4 = removeLineComments test4
    putStrLn $ "输入: " ++ show test4
    putStrLn $ "输出: " ++ show result4
    putStrLn $ "期望: " ++ show "'a'"
    putStrLn $ "通过: " ++ show (result4 == "'a'")
    putStrLn ""