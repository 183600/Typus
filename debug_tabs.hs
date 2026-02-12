#!/usr/bin/env runhaskell

import qualified Utils

main :: IO ()
main = do
  -- Test prop_normalize_indentation_tabs failure case: " "
  putStrLn "Testing normalizeIndentation with \"\t\t \t\":"
  let input = "		 	"
  let result = Utils.normalizeIndentation input
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Output: " ++ show result
  putStrLn $ "Starts with \t\t: " ++ show ("\t\t" `isPrefixOf` result)
  putStrLn ""