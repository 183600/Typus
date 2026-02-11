import Utils

-- 测试 splitBy 的详细行为
testSplitByDetailed :: IO ()
testSplitByDetailed = do
    putStrLn "=== 测试 splitBy 的详细行为 ==="
    
    -- 测试 "\na"
    putStrLn $ "splitBy '\n' \"\\na\""
    putStrLn $ "  结果: " ++ show (splitBy '\n' "\na")
    putStrLn $ "  期望: [\"\", \"a\"]"
    
    -- 手动模拟 break 的行为
    let str = "\na"
    let (part, rest) = break (== '\n') str
    putStrLn $ "  break (== '\n') \"\\na\" = (" ++ show part ++ ", " ++ show rest ++ ")"
    
    case rest of
      [] -> putStrLn $ "  rest 为空，结果: [" ++ part ++ "]"
      [_] -> putStrLn $ "  rest 长度为1，结果: [" ++ part ++ "]"
      _:xs -> putStrLn $ "  rest 长度>1，xs = " ++ show xs ++ "，结果应该递归"

main :: IO ()
main = testSplitByDetailed