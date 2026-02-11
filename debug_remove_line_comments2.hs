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
  
  -- 测试期望的行为
  let expected = "\n\n"  -- 保持原始行数
  putStrLn $ "Expected processed: " ++ show expected
  putStrLn $ "Expected lines: " ++ show (lines expected)
  putStrLn ""

-- 测试 removeLineComments 对单行空字符串的处理
test_single_empty :: IO ()
test_single_empty = do
  let lines' = [""]
  let code = unlines lines'
  let processed = U.removeLineComments code
  let procLines = lines processed
  putStrLn $ "Input lines: " ++ show lines'
  putStrLn $ "Code: " ++ show code
  putStrLn $ "Processed: " ++ show processed
  putStrLn $ "Processed lines: " ++ show procLines
  putStrLn $ "Length check: " ++ show (length procLines) ++ " vs " ++ show (length lines')
  putStrLn ""

main :: IO ()
main = do
  putStrLn "Testing removeLineComments with single empty:"
  test_single_empty
  
  putStrLn "Testing removeLineComments with multiline empty:"
  test_multiline_empty