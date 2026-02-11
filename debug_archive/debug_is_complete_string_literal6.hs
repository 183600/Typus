import Utils

main :: IO ()
main = do
    let incomplete = "\"" ++ "\\"  -- 双引号 + 反斜杠
    putStrLn $ "Testing: " ++ show incomplete
    
    -- 直接调用 isCompleteStringLiteral
    putStrLn $ "isCompleteStringLiteral: " ++ show (isCompleteStringLiteral incomplete)