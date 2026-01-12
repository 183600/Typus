import Utils (removeLineComments)

main :: IO ()
main = do
    -- 测试基本字符串中的 // 
    putStrLn "测试1: 字符串中的 //"
    let test1 = "let s = \"hello // world\""
    let result1 = removeLineComments test1
    putStrLn $ "输入: " ++ show test1
    putStrLn $ "输出: " ++ show result1
    putStrLn $ "期望: " ++ show test1
    putStrLn $ "通过: " ++ show (result1 == test1)
    putStrLn ""
    
    -- 测试字符字面量中的 //
    putStrLn "测试2: 字符字面量中的 //"
    let test2 = "let c = '/' // comment"
    let result2 = removeLineComments test2
    putStrLn $ "输入: " ++ show test2
    putStrLn $ "输出: " ++ show result2
    putStrLn $ "期望: " ++ show "let c = '/'"
    putStrLn $ "通过: " ++ show (result2 == "let c = '/'")
    putStrLn ""
    
    -- 测试普通注释
    putStrLn "测试3: 普通注释"
    let test3 = "let x = 5 // comment"
    let result3 = removeLineComments test3
    putStrLn $ "输入: " ++ show test3
    putStrLn $ "输出: " ++ show result3
    putStrLn $ "期望: " ++ show "let x = 5"
    putStrLn $ "通过: " ++ show (result3 == "let x = 5")
    putStrLn ""
    
    -- 测试空字符串
    putStrLn "测试4: 空字符串"
    let test4 = ""
    let result4 = removeLineComments test4
    putStrLn $ "输入: " ++ show test4
    putStrLn $ "输出: " ++ show result4
    putStrLn $ "期望: " ++ show ""
    putStrLn $ "通过: " ++ show (result4 == "")
    putStrLn ""
    
    -- 测试只有引号
    putStrLn "测试5: 只有引号"
    let test5 = "\""
    let result5 = removeLineComments test5
    putStrLn $ "输入: " ++ show test5
    putStrLn $ "输出: " ++ show result5
    putStrLn $ "期望: " ++ show "\""
    putStrLn $ "通过: " ++ show (result5 == "\"")
    putStrLn ""
    
    -- 测试单引号
    putStrLn "测试6: 单引号"
    let test6 = "'"
    let result6 = removeLineComments test6
    putStrLn $ "输入: " ++ show test6
    putStrLn $ "输出: " ++ show result6
    putStrLn $ "期望: " ++ show "'"
    putStrLn $ "通过: " ++ show (result6 == "'")
    putStrLn ""
