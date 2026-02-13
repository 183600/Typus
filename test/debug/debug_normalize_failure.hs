#!/usr/bin/env runhaskell

-- Test script to reproduce the failing test case
import qualified Utils as U

-- Test case from failure: prop_normalize_indentation_multiline_mixed with [""]
testPropNormalizeIndentationMultilineMixed :: [String] -> IO ()
testPropNormalizeIndentationMultilineMixed lines' = 
  let withMixed = map ("\t  " ++) lines'
      normalized = U.normalizeIndentation (unlines withMixed)
      normLines = lines normalized
  in if null lines'
     then putStrLn $ "Empty list test: " ++ show (normalized == "")
     else if lines' == ["\n"]
          then putStrLn $ "Newline test: " ++ show (normalized == "\n")
     else if lines' == [""]
          then do
            putStrLn $ "Input lines': " ++ show lines'
            putStrLn $ "withMixed: " ++ show withMixed
            putStrLn $ "unlines withMixed: " ++ show (unlines withMixed)
            putStrLn $ "Normalized: " ++ show normalized
            putStrLn $ "Expected: \"    \""
            putStrLn $ "Test result: " ++ show (normalized == "    ")
          else putStrLn "Other case"

main :: IO ()
main = do
  putStrLn "=== Testing prop_normalize_indentation_multiline_mixed failure case ==="
  testPropNormalizeIndentationMultilineMixed [""]