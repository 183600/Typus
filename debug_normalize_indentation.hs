#!/usr/bin/env stack
-- stack runghc --package QuickCheck

import qualified Utils as U

-- 测试 normalizeIndentation 对包含换行符的单行处理
test_normalize_indentation_newline :: IO ()
test_normalize_indentation_newline = do
  let lines' = ["\n"]
  let input = unlines lines'
  let normalized = U.normalizeIndentation input
  let normLines = lines normalized
  putStrLn $ "Input lines: " ++ show lines'
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn $ "Normalized lines: " ++ show normLines
  putStrLn $ "Expected: " ++ show "\n"
  putStrLn $ "Equal: " ++ show (normalized == "\n")
  putStrLn ""

main :: IO ()
main = do
  putStrLn "Testing normalizeIndentation with newline:"
  test_normalize_indentation_newline
