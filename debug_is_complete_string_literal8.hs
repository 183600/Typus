import Utils

main :: IO ()
main = do
    let incomplete = "\"" ++ "\\"  -- 双引号 + 反斜杠
    putStrLn $ "Testing: " ++ show incomplete
    putStrLn $ "Actual result: " ++ show (isCompleteStringLiteral incomplete)