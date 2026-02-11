import Utils

main :: IO ()
main = do
    putStrLn "=== Testing specific case ==="
    
    let test2 = "\"" ++ "a" ++ "\\\\"
    putStrLn $ "Input: " ++ show test2
    putStrLn $ "Ends with \\\\: " ++ show (endsWithDoubleBackslash test2)
    
    -- 检查具体的模式匹配
    putStrLn "\nPattern matches:"
    putStrLn $ "  Length 2: " ++ show (length test2 == 2)
    putStrLn $ "  Matches [\"a]: " ++ show (test2 == "\"a")
    putStrLn $ "  Length 4: " ++ show (length test2 == 4)
    putStrLn $ "  First char is quote: " ++ show (if not (null test2) then head test2 == '\"' else False)
    
    -- 测试原始函数
    putStrLn $ "\nOriginal function result: " ++ show (Utils.isCompleteStringLiteral test2)

-- 检查字符串是否以双反斜杠结尾
endsWithDoubleBackslash :: String -> Bool
endsWithDoubleBackslash [] = False
endsWithDoubleBackslash [_] = False
endsWithDoubleBackslash str = 
  let lastTwo = drop (length str - 2) str
  in lastTwo == "\\\\"