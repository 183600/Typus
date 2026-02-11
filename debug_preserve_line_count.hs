#!/usr/bin/env stack
-- stack runghc --package QuickCheck

import Data.List (lines, unlines)

-- 测试 preserveLineCount 函数的行为
test_preserve_line_count :: IO ()
test_preserve_line_count = do
  let input = "\n\n"
  let inputLines = lines input
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Input lines: " ++ show inputLines
  putStrLn $ "Length of input lines: " ++ show (length inputLines)
  
  -- 模拟 processLine 函数
  let processLine line = 
        if null line
          then line  -- 空行保持不变
          else line  -- 简化处理
  
  let processedLines = map processLine inputLines
  let result = unlines processedLines
  putStrLn $ "Processed lines: " ++ show processedLines
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Result lines: " ++ show (lines result)
  putStrLn $ "Length of result lines: " ++ show (length (lines result))
  putStrLn ""

main :: IO ()
main = do
  putStrLn "Testing preserveLineCount behavior:"
  test_preserve_line_count