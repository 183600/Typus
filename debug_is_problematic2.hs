

main :: IO ()
main = do
    let s = ""
    let withEscape = "\"" ++ s ++ "\\\""
    
    putStrLn $ "s: " ++ show s
    putStrLn $ "withEscape: " ++ show withEscape
    putStrLn $ "s == \"\": " ++ show (s == "")
    
    -- 测试各个条件
    let result = if s == ""
                 then isProblematicUnclosedString "\""
                 else if s == ""
                      then isProblematicUnclosedString "\""
                      else if s == ""
                           then isProblematicUnclosedString "\""
                           else if s == ""
                                then isProblematicUnclosedString "\\"
                                else isProblematicUnclosedString withEscape
    
    putStrLn $ "Result: " ++ show result
    
    -- 测试实际函数调用
    putStrLn $ "isProblematicUnclosedString \"\\\"\": " ++ show (isProblematicUnclosedString "\"")
    putStrLn $ "isProblematicUnclosedString \"\\\\\": " ++ show (isProblematicUnclosedString "\\")
    putStrLn $ "isProblematicUnclosedString \"\\\"\\\\\\\"\": " ++ show (isProblematicUnclosedString "\"\\\"")

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