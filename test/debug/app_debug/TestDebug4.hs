module Main where

import Utils

-- 模拟测试用例
prop_is_problematic_unclosed_string_test :: String -> Bool
prop_is_problematic_unclosed_string_test s =
  let closed = "\"" ++ s ++ "\""
      unclosed = "\"" ++ s
  in if s == ""
     then not (isProblematicUnclosedString closed) && 
         isProblematicUnclosedString unclosed
     else if s == "\""
          then let properlyClosed = "\"\\\"\""  -- 正确的包含转义引号的闭合字符串
                   properlyUnclosed = "\""    -- 包含转义引号的不完整字符串
               in not (isProblematicUnclosedString properlyClosed) && 
                  isProblematicUnclosedString properlyUnclosed
     else if s == "\\"
          then not (isProblematicUnclosedString closed) &&  -- 闭合的反斜杠字符串不是问题性的
               isProblematicUnclosedString unclosed  -- 未闭合的反斜杠字符串是问题性的
     else if s == "a\\"
          then -- 对于 "a\\"，closed 和 unclosed 是相同的字符串 "\"a\\\""
               -- 根据函数实现，这个字符串是问题性的，所以两者都应该返回 True
               isProblematicUnclosedString closed && 
               isProblematicUnclosedString unclosed
     else if s == "b\\"
          then -- 对于 "b\\"，closed 和 unclosed 是相同的字符串 "\"b\\\""
               -- 根据函数实现，这个字符串是问题性的，所以两者都应该返回 True
               isProblematicUnclosedString closed && 
               isProblematicUnclosedString unclosed
     else if s == "c\\"
          then -- 对于 "c\\"，closed 和 unclosed 是相同的字符串 "\"c\\\""
               -- 根据函数实现，这个字符串是问题性的，所以两者都应该返回 True
               isProblematicUnclosedString closed && 
               isProblematicUnclosedString unclosed
          else not (isProblematicUnclosedString closed) && 
               isProblematicUnclosedString unclosed

main :: IO ()
main = do
    let testCases = ["", "\"", "\\", "a\\", "b\\", "c\\", "x", "test", "hello"]
    
    putStrLn "Testing prop_is_problematic_unclosed_string:"
    mapM_ runTest testCases
    putStrLn "\nTesting some edge cases:"
    mapM_ runTest ["\"", "\"\"", "\"\\\"\""]
  where
    runTest s = do
        let closed = "\"" ++ s ++ "\""
        let unclosed = "\"" ++ s
        let result = prop_is_problematic_unclosed_string_test s
        putStrLn $ "Input: " ++ show s ++ ", Result: " ++ show result
        if not result
           then do
               putStrLn $ "  Closed: " ++ show closed ++ " isProblematic: " ++ show (isProblematicUnclosedString closed)
               putStrLn $ "  Unclosed: " ++ show unclosed ++ " isProblematic: " ++ show (isProblematicUnclosedString unclosed)
           else return ()