#!/usr/bin/env runhaskell

import qualified Utils as U

main :: IO ()
main = do
  -- 测试空字符串的情况
  let testInput = ""
  let result = U.normalizeIndentation testInput
  putStrLn $ "Input: " ++ show testInput
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Expected: " ++ show ""
