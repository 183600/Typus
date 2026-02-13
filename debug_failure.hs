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
  
  putStrLn $ "isProblematicUnclosedString closed = " ++ show (isProblematicUnclosedString closed)
  putStrLn $ "isProblematicUnclosedString unclosed = " ++ show (isProblematicUnclosedString unclosed)
  
  let result = not (isProblematicUnclosedString closed) && isProblematicUnclosedString unclosed
  putStrLn $ "Test result (should be True): " ++ show result