main :: IO ()
main = do
    let quoted = "\"" ++ "\\" ++ "\""  -- 双引号 + 反斜杠 + 双引号
    putStrLn $ "quoted: " ++ show quoted
    putStrLn $ "Length: " ++ show (length quoted)
    putStrLn $ "As list: " ++ show quoted
    
    -- 检查模式匹配
    case quoted of
      "\\\"" -> putStrLn "Matches \"\\\\\\\""
      _ -> putStrLn "No match"