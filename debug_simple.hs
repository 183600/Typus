#!/usr/bin/env stack
-- stack script --resolver lts-21.25

-- 测试 prop_is_complete_string_literal_escape_backslash 的具体情况

main :: IO ()
main = do
  -- 测试失败的具体情况
  let s = "\""
  let withBackslash = "\"" ++ s ++ "\\\\"
  putStrLn $ "Input s: " ++ show s
  putStrLn $ "Constructed string: " ++ show withBackslash
  putStrLn $ "Length: " ++ show (length withBackslash)
  putStrLn $ "Chars: " ++ show (map (\c -> (c, fromEnum c)) withBackslash)
  
  -- 检查模式匹配
  let result = case withBackslash of
                 "\"\"\\\\" -> True  -- 应该匹配这个模式
                 _ -> False
  putStrLn $ "Pattern match result: " ++ show result
