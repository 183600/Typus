#!/usr/bin/env runhaskell

import qualified Utils
import Data.List (isPrefixOf)

main :: IO ()
main = do
  -- Test prop_normalize_indentation_tabs failure case: " "
  putStrLn "Testing normalizeIndentation with \"\t\t \t\":"
  let input = "\t\t \t"
  let result = Utils.normalizeIndentation input
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Output: " ++ show result
  putStrLn $ "Starts with \t\t: " ++ show ("\t\t" `isPrefixOf` result)
  putStrLn ""