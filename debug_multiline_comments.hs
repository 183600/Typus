#!/usr/bin/env stack
-- stack runghc --package QuickCheck

import qualified Utils as U

-- 测试 removeLineComments 对多行注释的处理
test_multiline_comments :: IO ()
test_multiline_comments = do
  let input = "code\n// comment1\n// comment2\nmore code"
  let expected = "code\n\n\nmore code"
  let processed = U.removeLineComments input
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Expected: " ++ show expected
  putStrLn $ "Processed: " ++ show processed
  putStrLn $ "Equal: " ++ show (processed == expected)
  putStrLn ""

main :: IO ()
main = do
  putStrLn "Testing removeLineComments with multiline comments:"
  test_multiline_comments