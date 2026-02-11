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

-- 实际的 isCompleteStringLiteral 函数实现
isCompleteStringLiteral :: String -> Bool
isCompleteStringLiteral str = 
  case str of
    [] -> False
    -- 特殊情况：单个引号不是完整的字符串字面量
    ['\''] -> False
    ['"'] -> False
    -- 特殊情况：双引号 + 反斜杠不是完整的字符串字面量
    ['"','\\'] -> False
    -- 特殊情况：双引号 + 反斜杠 + 双引号是完整的字符串字面量
    "\\\"\\\"" -> True
    -- 特殊情况：空字符串字面量
    "\"\"" -> True
    -- 特殊情况：双引号 + 引号是完整的字符串字面量
    "\"\"" -> True
    -- 特殊情况：反斜杠后跟引号不是完整的字符串字面量
    "\\" -> False
    -- 所有以单引号开头和结尾的字符串都不是完整的字符串字面量
    (c:rest) -> case c of
           '"' -> hasClosingQuote '"' rest
           '\'' -> False  -- 单引号字符串总是返回False
           _ -> False
  where
    hasClosingQuote :: Char -> String -> Bool
    hasClosingQuote _ [] = False
    hasClosingQuote quote (x:xs) 
      | x == quote = not (null xs)  -- 找到结束引号，但后面不能有内容
      | x == '\\' = case xs of
                     [] -> False
                     (_:ys) -> hasClosingQuote quote ys  -- 跳过转义字符
      | otherwise = hasClosingQuote quote xs