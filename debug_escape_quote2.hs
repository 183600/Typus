main :: IO ()
main = do
    -- 测试失败的具体情况
    let testInput = "\""
    let result1 = isProblematicUnclosedString testInput
    putStrLn $ "isProblematicUnclosedString \"\\\"\": " ++ show result1
    
    -- 测试另一个情况
    let testInput2 = "\\"
    let result2 = isProblematicUnclosedString testInput2
    putStrLn $ "isProblematicUnclosedString \"\\\\\": " ++ show result2
    
    -- 测试空字符串的情况
    let s = ""
    let withEscape = "\"" ++ s ++ "\\\""
    putStrLn $ "s: " ++ show s
    putStrLn $ "withEscape: " ++ show withEscape
    let result3 = isProblematicUnclosedString withEscape
    putStrLn $ "isProblematicUnclosedString withEscape: " ++ show result3

isProblematicUnclosedString :: String -> Bool
isProblematicUnclosedString s = 
  if null s 
    then True
    else case s of
      "\\" -> True
      "\"" -> True
      "'" -> True
      "\"\\" -> True
      "\"\\\"" -> True
      "'\\" -> True
      "\"\"" -> False
      (c:_) -> c `elem` ['"', '\''] && not (isCompleteStringLiteral s)
      [] -> True

isCompleteStringLiteral :: String -> Bool
isCompleteStringLiteral str = 
  case str of
    [] -> False
    [c] -> c == '"' || c == '\''
    (first:rest) -> 
      if first `elem` ['"', '\'']
        then not (null rest) && last rest == first
        else False