#!/usr/bin/env runhaskell

import qualified Utils as U

main :: IO ()
main = do
  -- 测试a\的情况
  let testInput = "a\\"
  let result = U.isProblematicUnclosedString testInput
  putStrLn $ "Input: " ++ show testInput
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Expected: " ++ show True
