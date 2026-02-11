#!/usr/bin/env stack
-- stack runghc --package QuickCheck

import qualified Utils as U
import Test.QuickCheck

-- 测试 removeLineComments 对单个空格加注释的处理
test_remove_line_comments_space :: IO ()
test_remove_line_comments_space = do
  let s = " "
  let withComment = s ++ "// comment"
  let processed = U.removeLineComments withComment
  putStrLn $ "Input: " ++ show s
  putStrLn $ "With comment: " ++ show withComment
  putStrLn $ "Processed: " ++ show processed
  putStrLn $ "Expected: " ++ show " "
  putStrLn $ "Equal: " ++ show (processed == " ")
  putStrLn ""

-- 测试 removeLineComments 对单引号加注释的处理
test_remove_line_comments_quote :: IO ()
test_remove_line_comments_quote = do
  let s = "'"
  let withComment = s ++ "// comment"
  let processed = U.removeLineComments withComment
  putStrLn $ "Input: " ++ show s
  putStrLn $ "With comment: " ++ show withComment
  putStrLn $ "Processed: " ++ show processed
  putStrLn $ "Expected: " ++ show "'// comment"
  putStrLn $ "Equal: " ++ show (processed == "'// comment")
  putStrLn ""

-- 测试 removeLineComments 对斜杠加注释的处理
test_remove_line_comments_slash :: IO ()
test_remove_line_comments_slash = do
  let s = "/"
  let withComment = s ++ "// comment"
  let processed = U.removeLineComments withComment
  putStrLn $ "Input: " ++ show s
  putStrLn $ "With comment: " ++ show withComment
  putStrLn $ "Processed: " ++ show processed
  putStrLn $ "Expected: " ++ show ""
  putStrLn $ "Equal: " ++ show (processed == "")
  putStrLn ""

-- 测试 normalizeIndentation 对混合缩进的处理
test_normalize_indentation_mixed :: IO ()
test_normalize_indentation_mixed = do
  let s = ""
  let mixed = "\t  \t  " ++ s ++ "  \t  "
  let normalized = U.normalizeIndentation mixed
  putStrLn $ "Input: " ++ show s
  putStrLn $ "Mixed: " ++ show mixed
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn $ "Expected: " ++ show "    "
  putStrLn $ "Equal: " ++ show (normalized == "    ")
  putStrLn ""

-- 测试 splitBy 对特殊字符的处理
test_split_by_special :: IO ()
test_split_by_special = do
  let s = "\n\28045"
  let parts = U.splitBy '\n' s
  putStrLn $ "Input: " ++ show s
  putStrLn $ "Parts: " ++ show parts
  putStrLn ""

main :: IO ()
main = do
  putStrLn "Testing removeLineComments with space:"
  test_remove_line_comments_space
  
  putStrLn "Testing removeLineComments with quote:"
  test_remove_line_comments_quote
  
  putStrLn "Testing removeLineComments with slash:"
  test_remove_line_comments_slash
  
  putStrLn "Testing normalizeIndentation mixed:"
  test_normalize_indentation_mixed
  
  putStrLn "Testing splitBy special:"
  test_split_by_special