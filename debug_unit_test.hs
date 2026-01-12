import Utils (removeLineComments)

-- 测试单元测试的期望
main :: IO ()
main = do
    -- 单元测试案例
    putStrLn "单元测试案例:"
    let input = "let x = 42 // comment\nlet y = 24 // another comment"
    let result = removeLineComments input
    let expected = "let x = 42\nlet y = 24"
    
    putStrLn $ "输入: " ++ show input
    putStrLn $ "输出: " ++ show result
    putStrLn $ "期望: " ++ show expected
    putStrLn $ "测试通过: " ++ show (result == expected)
    putStrLn ""