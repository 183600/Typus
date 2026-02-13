#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Src.Utils as U

-- Test case 1: prop_remove_line_comments_multiline failure
test1 :: IO ()
test1 = do
  let lines' = ["\n0"]
  let normalizedLines = map (reverse . dropWhile (== '\n') . reverse) lines'
  let code = unlines normalizedLines
  let processed = U.removeLineComments code
  let procLines = lines processed
  putStrLn "=== Test case 1: prop_remove_line_comments_multiline ==="
  putStrLn $ "Input lines': " ++ show lines'
  putStrLn $ "Normalized lines': " ++ show normalizedLines
  putStrLn $ "Code (unlines): " ++ show code
  putStrLn $ "Processed: " ++ show processed
  putStrLn $ "Processed lines: " ++ show procLines
  putStrLn $ "Length of processed lines: " ++ show (length procLines)
  putStrLn $ "Expected: 1, Actual: " ++ show (length procLines)
  putStrLn ""

-- Test case 2: prop_remove_line_comments_end failure
test2 :: IO ()
test2 = do
  let s = "'a"
  let withComment = s ++ "// comment"
  let processed = U.removeLineComments withComment
  putStrLn "=== Test case 2: prop_remove_line_comments_end ==="
  putStrLn $ "Input s: " ++ show s
  putStrLn $ "With comment: " ++ show withComment
  putStrLn $ "Processed: " ++ show processed
  putStrLn $ "Expected: \"'a\", Actual: " ++ show processed
  putStrLn ""

main :: IO ()
main = do
  test1
  test2
