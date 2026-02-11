#!/usr/bin/env stack
-- stack runghc --package QuickCheck

import qualified Utils as U

-- 测试 removeLineComments 对多行空字符串的处理
test_multiline_empty :: IO ()
test_multiline_empty = do
  let lines' = ["", ""]
  let code = unlines lines'
  let processed = U.removeLineComments code
  let procLines = lines processed
  putStrLn $ "Input lines: " ++ show lines'
  putStrLn $ "Code: " ++ show code
  putStrLn $ "Processed: " ++ show processed
  putStrLn $ "Processed lines: " ++ show procLines
  putStrLn $ "Length check: " ++ show (length procLines) ++ " vs " ++ show (length lines')
  putStrLn $ "Equal: " ++ show (length procLines == length lines')
  putStrLn ""

-- 测试 removeLineComments 对空字符串的处理
test_empty :: IO ()
test_empty = do
  let s = ""
  let processed = U.removeLineComments s
  putStrLn $ "Input: " ++ show s
  putStrLn $ "Processed: " ++ show processed
  putStrLn $ "Equal: " ++ show (processed == s)
  putStrLn ""

main :: IO ()
main = do
  putStrLn "Testing removeLineComments with empty string:"
  test_empty
  
  putStrLn "Testing removeLineComments with multiline empty:"
  test_multiline_empty