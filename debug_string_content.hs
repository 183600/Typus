import Data.Char

main :: IO ()
main = do
    putStrLn "=== Debugging string content ==="
    
    -- 测试案例1: "\\\\"
    let test1 = "\\\\"
    putStrLn $ "Test1: " ++ show test1
    putStrLn $ "Length: " ++ show (length test1)
    putStrLn $ "Chars with ord: " ++ show (map (\c -> (c, fromEnum c)) test1)
    
    -- 测试案例2: "a\\\\"
    let test2 = "a\\\\"
    putStrLn $ "\nTest2: " ++ show test2
    putStrLn $ "Length: " ++ show (length test2)
    putStrLn $ "Chars with ord: " ++ show (map (\c -> (c, fromEnum c)) test2)
    
    -- 让我们创建正确的字符串
    let correct1 = "\"" ++ "\\\\"
    putStrLn $ "\nCorrect1: " ++ show correct1
    putStrLn $ "Length: " ++ show (length correct1)
    putStrLn $ "Chars with ord: " ++ show (map (\c -> (c, fromEnum c)) correct1)
    
    let correct2 = "\"" ++ "a" ++ "\\\\"
    putStrLn $ "\nCorrect2: " ++ show correct2
    putStrLn $ "Length: " ++ show (length correct2)
    putStrLn $ "Chars with ord: " ++ show (map (\c -> (c, fromEnum c)) correct2)