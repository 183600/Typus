#!/usr/bin/env runhaskell

import qualified Utils as U

-- Test case 1: prop_is_complete_string_literal with "a"
test1 :: IO ()
test1 = do
  putStrLn "=== Test case 1: prop_is_complete_string_literal with \"a\" ==="
  let s = "a"
  let quoted = "\"" ++ s ++ "\""
  let incomplete = "\"" ++ s
  putStrLn $ "s = " ++ show s
  putStrLn $ "quoted = " ++ show quoted
  putStrLn $ "incomplete = " ++ show incomplete
  putStrLn $ "U.isCompleteStringLiteral quoted = " ++ show (U.isCompleteStringLiteral quoted)
  putStrLn $ "U.isCompleteStringLiteral incomplete = " ++ show (U.isCompleteStringLiteral incomplete)
  putStrLn ""

-- Test case 2: prop_is_problematic_unclosed_string with "a"
test2 :: IO ()
test2 = do
  putStrLn "=== Test case 2: prop_is_problematic_unclosed_string with \"a\" ==="
  let s = "a"
  let closed = "\"" ++ s ++ "\""
  let unclosed = "\"" ++ s
  putStrLn $ "s = " ++ show s
  putStrLn $ "closed = " ++ show closed
  putStrLn $ "unclosed = " ++ show unclosed
  putStrLn $ "U.isProblematicUnclosedString closed = " ++ show (U.isProblematicUnclosedString closed)
  putStrLn $ "U.isProblematicUnclosedString unclosed = " ++ show (U.isProblematicUnclosedString unclosed)
  putStrLn ""

-- Test case 3: prop_normalize_indentation_multiline_mixed with ["\n","\199129\f"]
test3 :: IO ()
test3 = do
  putStrLn "=== Test case 3: prop_normalize_indentation_multiline_mixed with [\"\\n\",\"\\199129\\f\"] ==="
  let lines' = ["\n", "\199129\f"]
  let withMixed = map ("\t  " ++) lines'
  let normalized = U.normalizeIndentation (unlines withMixed)
  let normLines = lines normalized
  putStrLn $ "lines' = " ++ show lines'
  putStrLn $ "withMixed = " ++ show withMixed
  putStrLn $ "normalized = " ++ show normalized
  putStrLn $ "normLines = " ++ show normLines
  putStrLn $ "length normLines = " ++ show (length normLines)
  putStrLn $ "length lines' = " ++ show (length lines')
  putStrLn ""

-- Test case 4: prop_normalize_indentation_multiline_mixed with ["\n^"]
test4 :: IO ()
test4 = do
  putStrLn "=== Test case 4: prop_normalize_indentation_multiline_mixed with [\"\\n^\"] ==="
  let lines' = ["\n^"]
  let withMixed = map ("\t  " ++) lines'
  let normalized = U.normalizeIndentation (unlines withMixed)
  let normLines = lines normalized
  putStrLn $ "lines' = " ++ show lines'
  putStrLn $ "withMixed = " ++ show withMixed
  putStrLn $ "normalized = " ++ show normalized
  putStrLn $ "normLines = " ++ show normLines
  putStrLn $ "length normLines = " ++ show (length normLines)
  putStrLn $ "length lines' = " ++ show (length lines')
  putStrLn ""

main :: IO ()
main = do
  test1
  test2
  test3
  test4
