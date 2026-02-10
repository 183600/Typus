import Utils

main :: IO ()
main = do
    let incomplete = "\"" ++ "\\"  -- 双引号 + 反斜杠
    putStrLn $ "Testing: " ++ show incomplete
    
    -- 手动实现 isCompleteStringLiteral 的逻辑
    let result = case incomplete of
          [] -> False
          ['\''] -> False
          ['"'] -> False
          "\"\\" -> True
          "\\" -> False
          "\"" -> False
          "'" -> False
          ['"','\\'] -> False
          (c:rest) -> case c of
                 '"' -> False  -- 简化，直接返回False
                 '\' -> False
                 _ -> False
    
    putStrLn $ "Manual result: " ++ show result
    putStrLn $ "Actual result: " ++ show (isCompleteStringLiteral incomplete)