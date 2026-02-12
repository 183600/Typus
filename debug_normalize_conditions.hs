#!/usr/bin/env runhaskell

import Data.Char (isSpace, isPrint)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Utils as U

main :: IO ()
main = do
  putStrLn "Testing normalizeIndentation with specific input..."
  let testInput = "a"  -- 从失败信息中得到的输入
  let mixed = "\t  \t  " ++ testInput ++ "  \t  "
  putStrLn $ "Input: " ++ show testInput
  putStrLn $ "Mixed: " ++ show mixed
  putStrLn $ "Mixed length: " ++ show (length mixed)
  putStrLn $ "Mixed starts with \"\\t  \\t  \": " ++ show ("\t  \t  " `isPrefixOf` mixed)
  putStrLn $ "Mixed ends with \"  \\t  \": " ++ show ("  \t  " `isSuffixOf` mixed)
  putStrLn $ "Mixed starts with \"\\t\\t\": " ++ show ("\t\t" `isPrefixOf` mixed)
  
  let normalized = U.normalizeIndentation mixed
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn $ "Expected: " ++ show mixed
  
  -- 测试其他相关输入
  putStrLn "\nTesting other inputs:"
  let testInput2 = "ab"
  let mixed2 = "\t  \t  " ++ testInput2 ++ "  \t  "
  let normalized2 = U.normalizeIndentation mixed2
  putStrLn $ "Input: " ++ show testInput2
  putStrLn $ "Mixed: " ++ show mixed2
  putStrLn $ "Normalized: " ++ show normalized2
  putStrLn $ "Length >= 9: " ++ show (length mixed2 >= 9)
  putStrLn $ "Middle length: " ++ show (length mixed2 - 9)
  
  