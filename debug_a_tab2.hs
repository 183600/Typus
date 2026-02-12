#!/usr/bin/env runhaskell

import qualified Utils as U
import Data.Char (isPrint, isSpace)

main :: IO ()
main = do
  putStrLn "Testing normalizeIndentation with a\t..."
  let testInput = "a\t"
  putStrLn $ "Input: " ++ show testInput
  putStrLn $ "Contains tab: " ++ show ('\t' `elem` testInput)
  putStrLn $ "Contains space: " ++ show (' ' `elem` testInput)
  putStrLn $ "All isSpace: " ++ show (all isSpace testInput)
  putStrLn $ "Should match condition: " ++ show ('\t' `elem` testInput && not (' ' `elem` testInput) && not (all isSpace testInput))
  
  -- 手动测试转换逻辑
  let converted = map (\c -> if c == '\t' then ' ' else c) testInput
  putStrLn $ "Manual conversion: " ++ show converted
  
  let normalized = U.normalizeIndentation testInput
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn $ "Expected: " ++ show "a "
  putStrLn $ "Test " ++ if normalized == "a " then "PASSED" else "FAILED"