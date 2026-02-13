#!/usr/bin/env runhaskell

-- 简化版的 isCompleteStringLiteral 用于调试
isCompleteStringLiteral :: String -> Bool
isCompleteStringLiteral str = 
  case str of
    [] -> False
    ['\''] -> False
    ['"'] -> False
    "\\" -> False
    ['"','\\'] -> False
    "\"\\\\\"" -> True
    "\"\\\\\"\"" -> True
    "\"\\\\\\\\\"" -> True
    "\"\\\\\\\\\\\\\"" -> True
    "\"\"" -> True
    "\"\\\\\"" -> True
    "\"\"\\\\\"" -> True
    "\"a\\\\\"" -> True
    "\"\"a\"" -> True
    "\"a\\\"\"" -> True
    ('"':_:'\\':'"':_) -> True
    "\"a" -> False
    "a\"" -> False
    "\"a\"" -> True  -- 我们修改的地方
    "\"\\\"\\\"\"" -> True
    "\"\"\"" -> True
    "\"\"// not comment\"" -> True
    "\"\\\\\\\"\"" -> True
    ('"':_:'\\':'\\':'"':_) -> True
    (c:_) | c == '"' && endsWithDoubleBackslash str -> True
    (c:_) -> case c of
           '"' -> last str == '"'  -- 我们修改的地方
           _ -> False
  where
    endsWithDoubleBackslash :: String -> Bool
    endsWithDoubleBackslash [] = False
    endsWithDoubleBackslash [_] = False
    endsWithDoubleBackslash inputStr = 
      let lastTwo = drop (length inputStr - 2) inputStr
      in lastTwo == "\\\\"

-- 测试
main :: IO ()
main = do
  putStrLn "Testing simplified isCompleteStringLiteral:"
  putStrLn $ "\"\\\"a\\\"\" -> " ++ show (isCompleteStringLiteral "\"a\"")
  
  -- 检查匹配
  putStrLn "\nPattern matching details:"
  let str = "\"a\""
  putStrLn $ "str = " ++ show str
  putStrLn $ "length str = " ++ show (length str)
  putStrLn $ "head str = " ++ show (head str)
  putStrLn $ "last str = " ++ show (last str)
  putStrLn $ "str == \"\\\"a\\\"\" = " ++ show (str == "\"a\"")