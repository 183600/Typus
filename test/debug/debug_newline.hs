#!/usr/bin/env runhaskell

import qualified Utils as U

main :: IO ()
main = do
  -- 测试\n的情况
  let testInput = "\n"
  let result = U.normalizeIndentation testInput
  putStrLn $ "Input: " ++ show testInput
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Expected: " ++ show "\n"
