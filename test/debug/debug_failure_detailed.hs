#!/usr/bin/env runhaskell

import Utils

-- 测试失败的情况
main :: IO ()
main = do
  putStrLn "Testing prop_is_problematic_unclosed_string failure case:"
  let s = "a"
  let closed = "\"" ++ s ++ "\""
  let unclosed = "\"" ++ s
  
  putStrLn $ "s = " ++ show s
  putStrLn $ "closed = " ++ show closed
  putStrLn $ "unclosed = " ++ show unclosed
  
  putStrLn $ "isCompleteStringLiteral closed = " ++ show (isCompleteStringLiteral closed)
  putStrLn $ "isCompleteStringLiteral unclosed = " ++ show (isCompleteStringLiteral unclosed)
  
  putStrLn $ "isProblematicUnclosedString closed = " ++ show (isProblematicUnclosedString closed)
  putStrLn $ "isProblematicUnclosedString unclosed = " ++ show (isProblematicUnclosedString unclosed)
  
  -- 根据测试用例逻辑检查
  let testResult = if s == ""
                  then not (isProblematicUnclosedString closed) && isProblematicUnclosedString unclosed
                  else if s == "\""
                       then let properlyClosed = "\"\\\"\""
                                properlyUnclosed = "\""
                            in not (isProblematicUnclosedString properlyClosed) && isProblematicUnclosedString properlyUnclosed
                       else if s == "\\"
                            then not (isProblematicUnclosedString closed) && isProblematicUnclosedString unclosed
                            else not (isProblematicUnclosedString closed) && isProblematicUnclosedString unclosed
                            
  putStrLn $ "Test result (should be True): " ++ show testResult
  
  -- 检查特殊情况
  putStrLn "\nChecking special cases:"
  putStrLn $ "isProblematicUnclosedString \"a\\\\\" = " ++ show (isProblematicUnclosedString "a\\")
  putStrLn $ "isProblematicUnclosedString \"a\\\"\"\" = " ++ show (isProblematicUnclosedString "a\"")