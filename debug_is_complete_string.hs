main :: IO ()
main = do
    -- 测试失败的具体情况
    let s = "\""
    let quoted = "\"" ++ s ++ "\""
    let incomplete = "\"" ++ s
    
    putStrLn $ "s: " ++ show s
    putStrLn $ "quoted: " ++ show quoted
    putStrLn $ "incomplete: " ++ show incomplete
    
    let result1 = isCompleteStringLiteral quoted
    let result2 = isCompleteStringLiteral incomplete
    
    putStrLn $ "isCompleteStringLiteral quoted: " ++ show result1
    putStrLn $ "isCompleteStringLiteral incomplete: " ++ show result2
    putStrLn $ "isCompleteStringLiteral \"\\\"\\\"\": " ++ show (isCompleteStringLiteral "\"\"")
    putStrLn $ "isCompleteStringLiteral \"\\\"\": " ++ show (isCompleteStringLiteral "\"")

isCompleteStringLiteral :: String -> Bool
isCompleteStringLiteral str = 
  case str of
    [] -> False
    [c] -> c == '"' || c == '\''
    (first:rest) -> 
      if first `elem` ['"', '\'']
        then not (null rest) && last rest == first
        else False