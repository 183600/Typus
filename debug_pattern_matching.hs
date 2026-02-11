import Utils

main :: IO ()
main = do
    putStrLn "=== Debugging pattern matching ==="
    
    -- 测试案例1: "\\\\"
    let test1 = "\\\\"
    putStrLn $ "Test1: " ++ show test1
    putStrLn $ "Ends with double backslash: " ++ show (endsWithDoubleBackslash test1)
    putStrLn $ "First char is quote: " ++ show (if not (null test1) then head test1 == '"' else False)
    putStrLn $ "isCompleteStringLiteral result: " ++ show (Utils.isCompleteStringLiteral test1)
    
    -- 测试案例2: "a\\\\"
    let test2 = "a\\\\"
    putStrLn $ "\nTest2: " ++ show test2
    putStrLn $ "Ends with double backslash: " ++ show (endsWithDoubleBackslash test2)
    putStrLn $ "First char is quote: " ++ show (if not (null test2) then head test2 == '"' else False)
    putStrLn $ "isCompleteStringLiteral result: " ++ show (Utils.isCompleteStringLiteral test2)

-- 检查字符串是否以双反斜杠结尾
endsWithDoubleBackslash :: String -> Bool
endsWithDoubleBackslash [] = False
endsWithDoubleBackslash [_] = False
endsWithDoubleBackslash str = 
  let lastTwo = drop (length str - 2) str
  in lastTwo == "\\\\"