#!/usr/bin/env runhaskell
import Utils

main :: IO ()
main = do
  let testInput = ["\n"]
  putStrLn $ "Testing with input: " ++ show testInput
  let withMixed = map ("\t  " ++) testInput
  putStrLn $ "With mixed indentation: " ++ show withMixed
  let normalized = normalizeIndentation (unlines withMixed)
  putStrLn $ "Normalized result: " ++ show normalized
  putStrLn $ "Expected: \"\\n\""
  putStrLn $ "Test passes: " ++ show (normalized == "\n")