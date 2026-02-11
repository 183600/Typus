main :: IO ()
main = do
    let s = ""
    let withEscape = "\"" ++ s ++ "\\\""
    
    putStrLn $ "s: " ++ show s
    putStrLn $ "withEscape: " ++ show withEscape
    putStrLn $ "s == \"\": " ++ show (s == "")
    
    -- 测试函数调用
    let result = isProblematicUnclosedString withEscape
    putStrLn $ "isProblematicUnclosedString withEscape: " ++ show result

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