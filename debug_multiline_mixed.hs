#!/usr/bin/env runhaskell

import qualified Utils

main :: IO ()
main = do
  -- Test prop_normalize_indentation_multiline_mixed failure case: ["\n\1097959"]
  putStrLn "Testing prop_normalize_indentation_multiline_mixed with lines' = [\"\\n\\1097959\"]:"
  let lines' = ["\n\1097959"]
  let withMixed = map ("\t  " ++) lines'
  let normalized = Utils.normalizeIndentation (unlines withMixed)
  let normLines = lines normalized
  putStrLn $ "lines': " ++ show lines'
  putStrLn $ "withMixed: " ++ show withMixed
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "normLines: " ++ show normLines
  putStrLn $ "length normLines: " ++ show (length normLines)
  putStrLn $ "Expected length: 2"
  putStrLn $ "Test passes: " ++ show (length normLines == 2)
  putStrLn ""